"""Internal logger state management for the Python libLogit binding."""

from __future__ import annotations

import logging
import sqlite3
import threading
from collections.abc import Iterable
from logging import Handler, Logger
from logging.handlers import SocketHandler
from pathlib import Path
from typing import Any, Callable, Iterator, Mapping, Optional
from urllib.parse import urlparse

from .errors import LogConfigurationError
from .events import merge_metadata
from .internal_config import _Config, _normalize_level_name
from .levels import LEVEL_MAP
from .redaction import redact_text
from .runtime import _BufferedLogHandler, _CloseAfterEmitFileHandler
from .runtime import _FailurePolicyHandler, _JsonLogFormatter, _SQLiteLogHandler, _TextLogFormatter
from .runtime import _safe_close_handler

FileHandlerFactory = Callable[[Path, Mapping[str, int]], Handler]


class _LoggerState:
    """Owns the configured :mod: logger instance."""

    def __init__(
        self,
        logger_name: str = "liblogit",
        *,
        file_handler_factory: Optional[FileHandlerFactory] = None,
    ) -> None:
        self.config: Optional[_Config] = None
        self.logger: Optional[Logger] = None
        self.logger_name = logger_name
        self._file_handler_factory = file_handler_factory or _CloseAfterEmitFileHandler
        self._lock = threading.RLock()

    def configure(self, config: _Config) -> None:
        """Configure / reconfigure the internal logger to match *config*."""

        with self._lock:
            self.config = config
            logger = logging.getLogger(self.logger_name)
            logger.setLevel(config.threshold_level)
            logger.propagate = False

            for handler in list(logger.handlers):
                logger.removeHandler(handler)
                _safe_close_handler(handler)

            formatter: logging.Formatter
            if config.format == "json":
                formatter = _JsonLogFormatter(config)
            else:
                formatter = _TextLogFormatter(config)

            if "console" in config.sinks:
                self._attach_handler(
                    logger,
                    logging.StreamHandler(),
                    formatter,
                    config.threshold_level,
                    config.buffering,
                    config.failure_policy,
                )

            file_targets: list[Optional[str]] = []
            if "file" in config.sinks:
                file_targets.append(config.file_location)
            if "network" in config.sinks:
                file_targets.append(config.network_file_location)

            for path in _filter_paths(file_targets):
                file_path = _resolve_file_sink_path(path, config)
                try:
                    if file_path.parent and str(file_path.parent) not in {".", ""}:
                        file_path.parent.mkdir(parents=True, exist_ok=True)
                    file_handler = self._file_handler_factory(file_path, config.rotation)
                except (OSError, ValueError) as exc:
                    logger.warning("Unable to attach file logger at %s: %s", path, exc)
                    continue
                self._attach_handler(
                    logger,
                    file_handler,
                    formatter,
                    config.threshold_level,
                    config.buffering,
                    config.failure_policy,
                )

            if "network" in config.sinks and config.network_file_location and _looks_like_socket(config.network_file_location):
                parsed = urlparse(config.network_file_location)
                if parsed.hostname and parsed.port:
                    try:
                        socket_handler = SocketHandler(parsed.hostname, parsed.port)
                    except (OSError, ValueError) as exc:
                        logger.warning(
                            "Unable to attach socket logger at %s: %s",
                            config.network_file_location,
                            exc,
                        )
                    else:
                        self._attach_handler(
                            logger,
                            socket_handler,
                            formatter,
                            config.threshold_level,
                            config.buffering,
                            config.failure_policy,
                        )

            if "database" in config.sinks:
                try:
                    database_handler = _SQLiteLogHandler(config)
                except (OSError, sqlite3.Error, LogConfigurationError) as exc:
                    logger.warning("Unable to attach SQLite log store at %s: %s", config.database_location, exc)
                else:
                    self._attach_handler(
                        logger,
                        database_handler,
                        formatter,
                        config.threshold_level,
                        config.buffering,
                        config.failure_policy,
                    )

            self.logger = logger

    @staticmethod
    def _attach_handler(
        logger: Logger,
        handler: Handler,
        formatter: logging.Formatter,
        threshold_level: int,
        buffering: Optional[Mapping[str, Any]] = None,
        failure_policy: Optional[Mapping[str, Any]] = None,
    ) -> None:
        """Attach *handler* with the common formatter and threshold."""

        handler.setFormatter(formatter)
        handler.setLevel(threshold_level)
        handler = _FailurePolicyHandler(handler, failure_policy or {"mode": "warn"})
        if buffering and buffering.get("mode") == "async":
            handler = _BufferedLogHandler(handler, buffering)
            handler.setLevel(threshold_level)
        logger.addHandler(handler)

    def log(self, level: str, message: str, *, metadata: Optional[Mapping[str, Any]] = None) -> None:
        """Send *message* to the configured logger at *level*."""

        normalized = _normalize_level_name(level)
        event_metadata = _normalize_event_metadata(metadata)
        with self._lock:
            if self.logger is None or self.config is None:
                raise RuntimeError("Logger not configured. Call init_from_config first.")
            if not self.config.enabled:
                return
            redacted_message = redact_text(message, self.config.redaction)
            self.logger.log(LEVEL_MAP[normalized], redacted_message, extra={"liblogit_metadata": event_metadata})

    def flush(self) -> None:
        """Flush every attached logging handler."""

        with self._lock:
            if self.logger is None:
                return
            for handler in list(self.logger.handlers):
                handler.flush()

    def close(self) -> None:
        """Flush and close every attached logging handler."""

        with self._lock:
            if self.logger is None:
                return
            for handler in list(self.logger.handlers):
                self.logger.removeHandler(handler)
                _safe_close_handler(handler)


def _filter_paths(paths: Iterable[Optional[str]]) -> Iterator[str]:
    """Yield non-empty paths that are not socket descriptors."""

    for path in paths:
        if path and not _looks_like_socket(path):
            yield path


def _resolve_file_sink_path(path: str, config: _Config) -> Path:
    """Resolve a configured path to the concrete file the sink should append."""

    base_path = Path(path)
    if config.path_mode == "directory":
        return base_path / f"{_safe_logit_filename(config.name)}.log"
    return base_path


def _safe_logit_filename(name: str) -> str:
    """Return a deterministic filename fragment for a LOGIT name."""

    cleaned = "".join(character if character.isalnum() or character in {"-", "_", "."} else "_" for character in name)
    cleaned = cleaned.strip("._")
    return cleaned or "default"


def _looks_like_socket(target: str) -> bool:
    """Return True when *target* represents a TCP/UDP socket destination."""

    parsed = urlparse(target)
    return parsed.scheme in {"tcp", "udp"} and parsed.hostname is not None and parsed.port is not None


def _normalize_event_metadata(metadata: Optional[Mapping[str, Any]]) -> dict[str, Any]:
    """Return validated per-message metadata."""

    if metadata is None:
        return {}
    try:
        return merge_metadata(metadata)
    except TypeError as exc:
        raise LogConfigurationError(str(exc)) from exc
