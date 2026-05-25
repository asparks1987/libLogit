"""Internal runtime handlers for the Python libLogit binding."""

from __future__ import annotations

import logging
import sqlite3
import sys
import threading
import time
from datetime import datetime, timezone
from logging import Handler
from pathlib import Path
from typing import Any, Mapping, Optional

from .errors import LogConfigurationError
from .events import LogEvent, merge_metadata
from .formatters import format_json_event, format_text_event
from .redaction import redact_metadata, redact_text

DEFAULT_BUFFER_CAPACITY = 100
DEFAULT_BUFFER_FLUSH_INTERVAL_SECONDS = 1.0


class _JsonLogFormatter(logging.Formatter):
    """Render log records as one-line JSON events."""

    def __init__(self, config: Any) -> None:
        super().__init__()
        self.config = config

    def format(self, record: logging.LogRecord) -> str:
        event = LogEvent(
            logger=self.config.name,
            level=_canonical_level_from_record(record),
            message=redact_text(record.getMessage(), self.config.redaction),
            timestamp=datetime.fromtimestamp(record.created, timezone.utc) if self.config.timestamp else None,
            metadata=_metadata_for_record(self.config, record),
        )
        return format_json_event(event)


class _TextLogFormatter(logging.Formatter):
    """Render log records with libLogit text-level labels."""

    def __init__(self, config: Any) -> None:
        super().__init__()
        self.config = config

    def format(self, record: logging.LogRecord) -> str:
        event = LogEvent(
            logger=self.config.name,
            level=_canonical_level_from_record(record),
            message=redact_text(record.getMessage(), self.config.redaction),
            timestamp=(
                datetime.fromtimestamp(record.created).astimezone().strftime("%Y-%m-%dT%H:%M:%S%z")
                if self.config.timestamp
                else None
            ),
            metadata=_metadata_for_record(self.config, record),
        )
        return format_text_event(event, tag_level=self.config.tag_level, timestamp=self.config.timestamp)


class _CloseAfterEmitFileHandler(logging.FileHandler):
    """Append one record and release the file handle immediately."""

    def __init__(self, filename: str | Path, rotation: Optional[Mapping[str, int]] = None) -> None:
        super().__init__(filename, mode="a", encoding="utf-8", delay=True)
        self.rotation = dict(rotation or {})

    def emit(self, record: logging.LogRecord) -> None:
        try:
            if self.rotation:
                message = self.format(record)
                self._rotate_if_needed(message)
                if self.stream is None:
                    self.stream = self._open()
                self.stream.write(message + self.terminator)
                self.flush()
            else:
                super().emit(record)
        except Exception:
            self.handleError(record)
        finally:
            self._close_stream()

    def close(self) -> None:
        self._close_stream()
        logging.Handler.close(self)

    def _close_stream(self) -> None:
        if self.stream is None:
            return
        try:
            self.flush()
        finally:
            stream = self.stream
            self.stream = None
            stream.close()

    def _rotate_if_needed(self, formatted_message: str) -> None:
        max_bytes = self.rotation.get("max_bytes")
        max_files = self.rotation.get("max_files", 1)
        if not max_bytes:
            return

        path = Path(self.baseFilename)
        incoming_bytes = len((formatted_message + self.terminator).encode("utf-8"))
        current_bytes = path.stat().st_size if path.exists() else 0
        if current_bytes == 0 or current_bytes + incoming_bytes <= max_bytes:
            return

        oldest = _rotation_backup_path(path, max_files)
        if oldest.exists():
            oldest.unlink()
        for index in range(max_files - 1, 0, -1):
            source = _rotation_backup_path(path, index)
            if source.exists():
                source.replace(_rotation_backup_path(path, index + 1))
        if path.exists():
            path.replace(_rotation_backup_path(path, 1))


class _SQLiteLogHandler(logging.Handler):
    """Write events to the alpha SQLite log store."""

    def __init__(self, config: Any) -> None:
        super().__init__(config.threshold_level)
        if not config.database_location:
            raise LogConfigurationError("databasePath is required when the database sink is enabled")
        self.config = config
        self.database_path = Path(config.database_location)
        if self.database_path.parent and str(self.database_path.parent) not in {".", ""}:
            self.database_path.parent.mkdir(parents=True, exist_ok=True)
        self._ensure_schema()

    def emit(self, record: logging.LogRecord) -> None:
        try:
            created_at = datetime.fromtimestamp(record.created, timezone.utc).isoformat()
            event = LogEvent(
                logger=self.config.name,
                level=_canonical_level_from_record(record),
                message=redact_text(record.getMessage(), self.config.redaction),
                timestamp=created_at,
                metadata=_metadata_for_record(self.config, record),
                source=record.name,
            )
            with sqlite3.connect(self.database_path) as connection:
                self._ensure_schema(connection)
                sequence = connection.execute(
                    "SELECT COALESCE(MAX(sequence), 0) + 1 FROM logit_events"
                ).fetchone()[0]
                connection.execute(
                    """
                    INSERT INTO logit_events (
                        created_at,
                        sequence,
                        logger,
                        level,
                        message,
                        format,
                        metadata_json,
                        source
                    )
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                    """,
                    (
                        created_at,
                        sequence,
                        self.config.name,
                        event.level_name,
                        event.rendered_message,
                        self.config.format,
                        event.metadata_json,
                        record.name,
                    ),
                )
                self._apply_retention(connection)
        except Exception:
            self.handleError(record)

    def _ensure_schema(self, connection: Optional[sqlite3.Connection] = None) -> None:
        owns_connection = connection is None
        if connection is None:
            connection = sqlite3.connect(self.database_path)
        try:
            connection.execute(
                """
                CREATE TABLE IF NOT EXISTS logit_events (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    created_at TEXT NOT NULL,
                    sequence INTEGER NOT NULL,
                    logger TEXT NOT NULL,
                    level TEXT NOT NULL,
                    message TEXT NOT NULL,
                    format TEXT NOT NULL,
                    metadata_json TEXT,
                    source TEXT
                )
                """
            )
            connection.execute(
                "CREATE INDEX IF NOT EXISTS idx_logit_events_logger_created_at "
                "ON logit_events(logger, created_at)"
            )
            connection.execute(
                "CREATE INDEX IF NOT EXISTS idx_logit_events_level_created_at "
                "ON logit_events(level, created_at)"
            )
            connection.execute(
                "CREATE INDEX IF NOT EXISTS idx_logit_events_sequence "
                "ON logit_events(sequence)"
            )
            if owns_connection:
                connection.commit()
        finally:
            if owns_connection:
                connection.close()

    def _apply_retention(self, connection: sqlite3.Connection) -> None:
        max_age_seconds = self.config.retention.get("max_age_seconds")
        if max_age_seconds:
            cutoff = datetime.fromtimestamp(
                datetime.now(timezone.utc).timestamp() - max_age_seconds,
                timezone.utc,
            ).isoformat()
            connection.execute("DELETE FROM logit_events WHERE created_at < ?", (cutoff,))

        max_records = self.config.retention.get("max_records")
        if max_records:
            connection.execute(
                """
                DELETE FROM logit_events
                WHERE id IN (
                    SELECT id
                    FROM logit_events
                    ORDER BY sequence DESC
                    LIMIT -1 OFFSET ?
                )
                """,
                (max_records,),
            )

        max_bytes = self.config.retention.get("max_bytes")
        if max_bytes:
            self._apply_logical_byte_retention(connection, max_bytes)

    @staticmethod
    def _apply_logical_byte_retention(connection: sqlite3.Connection, max_bytes: int) -> None:
        """Evict oldest rows until retained event payload text fits the byte budget."""

        while True:
            logical_bytes = connection.execute(
                """
                SELECT COALESCE(SUM(
                    length(COALESCE(created_at, '')) +
                    length(COALESCE(logger, '')) +
                    length(COALESCE(level, '')) +
                    length(COALESCE(message, '')) +
                    length(COALESCE(format, '')) +
                    length(COALESCE(metadata_json, '')) +
                    length(COALESCE(source, ''))
                ), 0)
                FROM logit_events
                """
            ).fetchone()[0]
            if logical_bytes <= max_bytes:
                return

            oldest = connection.execute(
                "SELECT id FROM logit_events ORDER BY sequence ASC LIMIT 1"
            ).fetchone()
            if oldest is None:
                return
            connection.execute("DELETE FROM logit_events WHERE id = ?", (oldest[0],))


def _safe_close_handler(handler: Handler) -> None:
    """Best-effort cleanup for handlers that may already own closed streams."""

    try:
        handler.flush()
    except (OSError, ValueError):
        pass
    try:
        handler.close()
    except (OSError, ValueError):
        pass


class _SinkEmitError(Exception):
    """Internal wrapper for one failed sink emit attempt."""

    def __init__(self, error: BaseException) -> None:
        super().__init__(str(error))
        self.error = error


class _FallbackFileHandler(logging.FileHandler):
    """Fallback file sink used only after another sink fails."""

    def __init__(self, filename: str | Path) -> None:
        fallback_path = Path(filename)
        if fallback_path.parent and str(fallback_path.parent) not in {".", ""}:
            fallback_path.parent.mkdir(parents=True, exist_ok=True)
        super().__init__(fallback_path, mode="a", encoding="utf-8", delay=True)

    def emit(self, record: logging.LogRecord) -> None:
        try:
            super().emit(record)
        finally:
            self.close()


class _FailurePolicyHandler(logging.Handler):
    """Apply configured sink-failure behavior around a concrete handler."""

    def __init__(self, target: Handler, failure_policy: Mapping[str, Any]) -> None:
        super().__init__(target.level)
        self.target = target
        self.failure_policy = dict(failure_policy)
        self._closing = False
        self._original_handle_error = target.handleError
        target.handleError = self._target_handle_error  # type: ignore[method-assign]

    def emit(self, record: logging.LogRecord) -> None:
        try:
            self._emit_once(record)
        except _SinkEmitError as exc:
            self._handle_failure(record, exc.error)

    def flush(self) -> None:
        if self._closing or getattr(self.target, "_closed", False):
            return
        try:
            self.target.flush()
        except (OSError, ValueError):
            return
        except Exception as exc:
            self._handle_failure(None, exc)

    def close(self) -> None:
        self._closing = True
        self.target.handleError = self._original_handle_error  # type: ignore[method-assign]
        _safe_close_handler(self.target)
        logging.Handler.close(self)

    def _target_handle_error(self, record: logging.LogRecord) -> None:
        _, error, _ = sys.exc_info()
        raise _SinkEmitError(error or RuntimeError("sink failed without exception details"))

    def _emit_once(self, record: logging.LogRecord) -> None:
        try:
            self.target.handle(record)
        except _SinkEmitError:
            raise
        except Exception as exc:
            raise _SinkEmitError(exc) from exc

    def _handle_failure(self, record: Optional[logging.LogRecord], error: BaseException) -> None:
        mode = str(self.failure_policy.get("mode", "warn"))
        if mode == "drop":
            return
        if mode == "raise":
            raise error
        if mode == "retry" and record is not None:
            if self._retry(record):
                return
            _warn_sink_failure(error)
            return
        if mode == "fallback" and record is not None:
            if self._fallback(record):
                return
            _warn_sink_failure(error)
            return
        _warn_sink_failure(error)

    def _retry(self, record: logging.LogRecord) -> bool:
        attempts = int(self.failure_policy.get("retry_attempts", 1))
        delay = float(self.failure_policy.get("retry_delay_seconds", 0.0))
        for _ in range(attempts):
            if delay:
                time.sleep(delay)
            try:
                self._emit_once(record)
            except _SinkEmitError:
                continue
            return True
        return False

    def _fallback(self, record: logging.LogRecord) -> bool:
        fallback_path = self.failure_policy.get("fallback_path")
        if not fallback_path:
            return False
        try:
            fallback_handler = _FallbackFileHandler(str(fallback_path))
            fallback_handler.setLevel(self.level)
            fallback_handler.setFormatter(self.target.formatter)
            fallback_handler.handle(record)
        except Exception as exc:
            _warn_sink_failure(exc)
            return False
        return True


def _warn_sink_failure(error: BaseException) -> None:
    """Write a concise sink failure warning without recursing through logging."""

    sys.stderr.write(f"libLogit sink failure: {error}\n")


class _BufferedLogHandler(logging.Handler):
    """Buffer records and forward them to a concrete sink on batch/interval/close."""

    def __init__(self, target: Handler, buffering: Mapping[str, Any]) -> None:
        super().__init__(target.level)
        self.target = target
        self.capacity = int(buffering.get("capacity", DEFAULT_BUFFER_CAPACITY))
        self.flush_interval_seconds = float(
            buffering.get("flush_interval_seconds", DEFAULT_BUFFER_FLUSH_INTERVAL_SECONDS)
        )
        self.buffer: list[logging.LogRecord] = []
        self._timer: Optional[threading.Timer] = None
        self._closing = False
        self._schedule_timer()

    def emit(self, record: logging.LogRecord) -> None:
        self.buffer.append(record)
        if len(self.buffer) >= self.capacity:
            self.flush()

    def flush(self) -> None:
        self.acquire()
        try:
            records = list(self.buffer)
            self.buffer.clear()
        finally:
            self.release()

        for record in records:
            try:
                self.target.handle(record)
            except Exception:
                self.handleError(record)

        if records:
            try:
                self.target.flush()
            except Exception:
                self.handleError(records[-1])

    def close(self) -> None:
        self.acquire()
        try:
            self._closing = True
            timer = self._timer
            self._timer = None
        finally:
            self.release()

        if timer is not None:
            timer.cancel()
        try:
            self.flush()
        finally:
            _safe_close_handler(self.target)
            logging.Handler.close(self)

    def _schedule_timer(self) -> None:
        if self._closing:
            return
        timer = threading.Timer(self.flush_interval_seconds, self._flush_from_timer)
        timer.daemon = True
        self._timer = timer
        timer.start()

    def _flush_from_timer(self) -> None:
        try:
            self.flush()
        finally:
            self.acquire()
            try:
                if not self._closing:
                    self._schedule_timer()
            finally:
                self.release()


def _metadata_for_record(config: Any, record: logging.LogRecord) -> dict[str, Any]:
    """Merge static LOGIT metadata with per-message metadata."""

    event_metadata = getattr(record, "liblogit_metadata", None)
    try:
        return redact_metadata(merge_metadata(config.metadata, event_metadata), config.redaction)
    except TypeError as exc:
        raise LogConfigurationError(str(exc)) from exc


def _canonical_level_from_record(record: logging.LogRecord) -> str:
    """Return the libLogit canonical level name for a Python log record."""

    if record.levelno <= logging.NOTSET:
        return "trace"
    if record.levelno <= logging.DEBUG:
        return "debug"
    if record.levelno <= logging.INFO:
        return "info"
    if record.levelno <= logging.WARNING:
        return "warn"
    if record.levelno <= logging.ERROR:
        return "error"
    return "fatal"


def _rotation_backup_path(path: Path, index: int) -> Path:
    """Return the backup path for one rotation index."""

    return Path(f"{path}.{index}")
