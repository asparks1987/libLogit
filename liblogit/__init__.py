"""Python binding for the libLogit JSON-configured logging toolkit.

This module exposes direct LOGIT objects, config-loaded LOGIT registries, and
compatibility helpers for the original global logger API.

Usage example::

    from liblogit import Logit, INFO, ENDL
    app_log = Logit(path="logs/app.log", level=INFO)
    app_log(INFO) << "Boot" << ENDL

Legacy example::

    from liblogit import init_from_config, LOG, ENDL
    init_from_config("logit.json")
    LOG("warn") << {"event": "calibration"} << ENDL

The configuration format is intentionally minimal and documented in
docs/configuration.md. A sample logit.sample.json ships with the package and
can be copied into a project directory via :func:`copy_sample_config`.
"""

from __future__ import annotations

import json
import logging
import os
import re
import sqlite3
import sys
import threading
import time
from collections.abc import Iterable as IterableABC
from collections.abc import Mapping as MappingABC
from dataclasses import dataclass, field
from datetime import datetime, timezone
from importlib import resources
from json import JSONDecodeError
from logging import Handler, Logger
from logging.handlers import SocketHandler
from pathlib import Path as _Path
from typing import Any, Dict, Iterable, Iterator, Mapping, Optional
from urllib.parse import urlparse

from .config import LogitConfig
from .events import LogEvent, merge_metadata, render_payload
from .formatters import format_json_event, format_text_event
from .levels import DEBUG, ERROR, FATAL, INFO, TRACE, WARN, LEVEL_MAP, Level
from .levels import normalize_level_name as _normalize_level_value

__version__ = "0.1.0"

__all__ = [
    "init_from_config",
    "configure",
    "load_logits",
    "get_logit",
    "Logit",
    "LOGIT",
    "LogitConfig",
    "Level",
    "LogEvent",
    "LOG",
    "ENDL",
    "LogConfigurationError",
    "copy_sample_config",
    "TRACE",
    "DEBUG",
    "INFO",
    "WARN",
    "ERROR",
    "FATAL",
    "__version__",
]

_SAMPLE_FILENAME = "logit.sample.json"


VALID_TOP_LEVEL_KEYS = {"level", "timestamp", "file_location", "network_file_location"}
"""Set of supported top-level keys in the JSON configuration."""

VALID_LEVEL_KEYS = {"threshold", "tag"}
"""Set of supported keys inside the `level` object."""

VALID_V2_TOP_LEVEL_KEYS = {"version", "defaults", "logits"}
"""Set of supported top-level keys in the named LOGIT configuration."""

VALID_LOGIT_KEYS = {
    "name",
    "path",
    "localPath",
    "local_path",
    "file_location",
    "pathMode",
    "path_mode",
    "remotePath",
    "remote_path",
    "network_path",
    "network_file_location",
    "databasePath",
    "database_path",
    "database_location",
    "level",
    "enabled",
    "sinks",
    "timestamp",
    "tag_level",
    "format",
    "metadata",
    "retention",
    "rotation",
    "redaction",
    "buffering",
    "failurePolicy",
    "failure_policy",
}
"""Set of supported keys for one named LOGIT object."""

VALID_REDACTION_KEYS = {"mask", "keys", "patterns"}
VALID_BUFFERING_KEYS = {
    "mode",
    "capacity",
    "batchSize",
    "batch_size",
    "flushIntervalSeconds",
    "flush_interval_seconds",
    "intervalSeconds",
    "interval_seconds",
}
VALID_BUFFERING_MODES = {"sync", "async", "buffered", "batch"}
VALID_FAILURE_POLICY_KEYS = {
    "mode",
    "retryAttempts",
    "retry_attempts",
    "retryDelaySeconds",
    "retry_delay_seconds",
    "fallbackPath",
    "fallback_path",
}
VALID_FAILURE_POLICY_MODES = {"warn", "drop", "raise", "retry", "fallback"}
VALID_SINKS = {"console", "file", "network", "database"}
VALID_FORMATS = {"text", "json"}
VALID_PATH_MODES = {"file", "directory"}
VALID_RETENTION_MODES = {"records", "age", "bytes", "bounded"}
DEFAULT_REDACTION_MASK = "[REDACTED]"
DEFAULT_BUFFER_CAPACITY = 100
DEFAULT_BUFFER_FLUSH_INTERVAL_SECONDS = 1.0
ENV_PREFIX = "LIBLOGIT_"
ENV_OVERRIDE_FIELDS = (
    ("LEVEL", "level"),
    ("ENABLED", "enabled"),
    ("PATH", "file_location"),
    ("LOCAL_PATH", "file_location"),
    ("FILE_LOCATION", "file_location"),
    ("REMOTE_PATH", "network_file_location"),
    ("NETWORK_PATH", "network_file_location"),
    ("NETWORK_FILE_LOCATION", "network_file_location"),
    ("DATABASE_PATH", "database_location"),
    ("DATABASE_LOCATION", "database_location"),
    ("PATH_MODE", "pathMode"),
)
PATH_OVERRIDE_FIELDS = {"file_location", "network_file_location", "database_location"}


class LogConfigurationError(RuntimeError):
    """Raised when the JSON configuration fails validation."""


@dataclass
class _Config:
    """In-memory representation of a validated configuration file."""

    threshold: str
    tag_level: bool
    timestamp: bool
    file_location: Optional[str]
    network_file_location: Optional[str]
    database_location: Optional[str] = None
    path_mode: str = "file"
    name: str = "default"
    enabled: bool = True
    sinks: tuple[str, ...] = ("console", "file", "network")
    format: str = "text"
    metadata: Dict[str, Any] = field(default_factory=dict)
    retention: Dict[str, int] = field(default_factory=dict)
    rotation: Dict[str, int] = field(default_factory=dict)
    redaction: Dict[str, Any] = field(default_factory=dict)
    buffering: Dict[str, Any] = field(default_factory=dict)
    failure_policy: Dict[str, Any] = field(default_factory=lambda: {"mode": "warn"})

    @classmethod
    def from_file(cls, path: str | _Path) -> "_Config":
        """Load configuration from *path* and return the parsed instance."""

        try:
            payload = json.loads(_Path(path).read_text(encoding="utf-8"))
        except FileNotFoundError as exc:
            raise LogConfigurationError(f"Configuration file not found: {path}") from exc
        except JSONDecodeError as exc:
            raise LogConfigurationError(f"Invalid JSON configuration: {exc}") from exc
        return cls.from_dict(payload)

    @classmethod
    def from_dict(cls, payload: Dict[str, Any]) -> "_Config":
        """Validate *payload* and return an :class: instance."""

        extra = set(payload.keys()) - VALID_TOP_LEVEL_KEYS
        if extra:
            raise LogConfigurationError(f"Unsupported configuration keys: {sorted(extra)}")

        payload = _apply_environment_overrides(payload, "default")
        level_cfg = payload.get("level")
        if level_cfg is None:
            raise LogConfigurationError("Missing required field: level")

        if isinstance(level_cfg, str):
            threshold_value: Any = level_cfg
            tag_level = True
        elif isinstance(level_cfg, dict):
            extra_level = set(level_cfg.keys()) - VALID_LEVEL_KEYS
            if extra_level:
                raise LogConfigurationError(f"Unsupported level options: {sorted(extra_level)}")
            threshold_value = level_cfg.get("threshold")
            if threshold_value is None:
                raise LogConfigurationError("level.threshold is required when level is an object")
            tag_level = bool(level_cfg.get("tag", True))
        else:
            raise LogConfigurationError("level must be a string or object")

        normalized_threshold = _normalize_level_name(threshold_value)
        timestamp = bool(payload.get("timestamp", True))
        file_location = payload.get("file_location")
        network_location = payload.get("network_file_location")
        database_location = payload.get("database_location")
        path_mode = _normalize_path_mode(payload.get("pathMode", "file"))
        enabled = _normalize_bool(payload.get("enabled", True), "enabled")

        _validate_optional_path("file_location", file_location)
        _validate_optional_path("network_file_location", network_location)
        _validate_optional_path("database_path", database_location)

        return cls(
            threshold=normalized_threshold,
            tag_level=tag_level,
            timestamp=timestamp,
            file_location=file_location or None,
            network_file_location=network_location or None,
            database_location=database_location or None,
            path_mode=path_mode,
            enabled=enabled,
            sinks=_normalize_sinks(None, file_location, network_location, database_location),
            retention=_normalize_retention(None, database_location is not None),
        )

    @classmethod
    def from_logit_dict(
        cls,
        payload: Dict[str, Any],
        *,
        name: str = "default",
        defaults: Optional[Dict[str, Any]] = None,
    ) -> "_Config":
        """Validate a v2 LOGIT object payload and return the parsed instance."""

        merged: Dict[str, Any] = {}
        if defaults:
            merged.update(defaults)
        merged.update(payload)

        extra = set(merged.keys()) - VALID_LOGIT_KEYS
        if extra:
            raise LogConfigurationError(f"Unsupported LOGIT configuration keys: {sorted(extra)}")

        raw_name = merged.get("name", name)
        if not isinstance(raw_name, str) or raw_name.strip() == "":
            raise LogConfigurationError("LOGIT name must be a non-empty string")

        merged = _apply_environment_overrides(merged, raw_name)
        threshold = merged.get("level")
        if threshold is None:
            threshold = INFO

        normalized_threshold = _normalize_level_name(threshold)
        file_location = merged.get(
            "file_location",
            merged.get("path", merged.get("localPath", merged.get("local_path"))),
        )
        network_location = merged.get(
            "network_file_location",
            merged.get("network_path", merged.get("remotePath", merged.get("remote_path"))),
        )
        database_location = merged.get(
            "database_location",
            merged.get("databasePath", merged.get("database_path")),
        )
        path_mode = _normalize_path_mode(merged.get("pathMode", merged.get("path_mode", "file")))
        _validate_optional_path("path", file_location)
        _validate_optional_path("network_path", network_location)
        _validate_optional_path("database_path", database_location)

        sinks = _normalize_sinks(merged.get("sinks"), file_location, network_location, database_location)
        log_format = str(merged.get("format", "text")).strip().lower()
        if log_format not in VALID_FORMATS:
            raise LogConfigurationError(f"Unknown format: {merged.get('format')}")

        metadata = merged.get("metadata", {})
        if not isinstance(metadata, dict):
            raise LogConfigurationError("metadata must be an object")

        retention = _normalize_retention(merged.get("retention"), "database" in sinks)
        rotation = _normalize_rotation(merged.get("rotation"))
        redaction = _normalize_redaction(merged.get("redaction"))
        buffering = _normalize_buffering(merged.get("buffering"))
        failure_policy = _normalize_failure_policy(merged.get("failurePolicy", merged.get("failure_policy")))

        return cls(
            threshold=normalized_threshold,
            tag_level=bool(merged.get("tag_level", True)),
            timestamp=bool(merged.get("timestamp", True)),
            file_location=file_location or None,
            network_file_location=network_location or None,
            database_location=database_location or None,
            path_mode=path_mode,
            name=raw_name,
            enabled=_normalize_bool(merged.get("enabled", True), "enabled"),
            sinks=sinks,
            format=log_format,
            metadata=metadata,
            retention=retention,
            rotation=rotation,
            redaction=redaction,
            buffering=buffering,
            failure_policy=failure_policy,
        )

    @property
    def threshold_level(self) -> int:
        """Return the :mod: level corresponding to :attr:."""

        return LEVEL_MAP[self.threshold]


def _normalize_level_name(level: str) -> str:
    """Normalise user supplied *level* text to the canonical form."""

    try:
        return _normalize_level_value(level)
    except TypeError as exc:
        raise LogConfigurationError(f"level must be a string, got {type(level).__name__}") from exc
    except ValueError as exc:
        raise LogConfigurationError(f"Unknown level: {level}") from exc


def _normalize_path_mode(value: Any) -> str:
    """Return the canonical file path interpretation mode."""

    if not isinstance(value, str):
        raise LogConfigurationError(f"pathMode must be a string, got {type(value).__name__}")
    mode = value.strip().lower()
    if mode not in VALID_PATH_MODES:
        raise LogConfigurationError(f"pathMode must be one of {sorted(VALID_PATH_MODES)}")
    return mode


def _normalize_bool(value: Any, name: str) -> bool:
    """Return a strict boolean for config fields that support environment overrides."""

    if isinstance(value, bool):
        return value
    raise LogConfigurationError(f"{name} must be a boolean, got {type(value).__name__}")


def _parse_environment_bool(value: str, env_name: str) -> bool:
    """Parse a boolean environment value."""

    normalized = value.strip().lower()
    if normalized in {"1", "true", "yes", "on", "enabled"}:
        return True
    if normalized in {"0", "false", "no", "off", "disabled"}:
        return False
    raise LogConfigurationError(
        f"{env_name} must be a boolean value: true/false, yes/no, on/off, or 1/0"
    )


def _environment_name_fragment(name: str) -> str:
    """Convert a LOGIT name into the environment variable name fragment."""

    fragment: list[str] = []
    last_was_separator = True
    for char in name:
        if char.isascii() and char.isalnum():
            fragment.append(char.upper())
            last_was_separator = False
        elif not last_was_separator:
            fragment.append("_")
            last_was_separator = True
    return "".join(fragment).strip("_") or "DEFAULT"


def _apply_environment_overrides(config: Dict[str, Any], name: str) -> Dict[str, Any]:
    """Apply global and named LIBLOGIT_* overrides to a config payload."""

    result = dict(config)
    scopes = ("", f"{_environment_name_fragment(name)}_")
    for scope in scopes:
        for suffix, field_name in ENV_OVERRIDE_FIELDS:
            env_name = f"{ENV_PREFIX}{scope}{suffix}"
            if env_name not in os.environ:
                continue
            _apply_environment_override_value(result, field_name, os.environ[env_name], env_name)
    return result


def _apply_environment_override_value(
    config: Dict[str, Any],
    field_name: str,
    value: str,
    env_name: str,
) -> None:
    """Set one environment override on *config*."""

    if field_name == "enabled":
        config[field_name] = _parse_environment_bool(value, env_name)
        return

    if field_name in PATH_OVERRIDE_FIELDS:
        config[field_name] = value if value.strip() else None
        return

    if value.strip() == "":
        raise LogConfigurationError(f"{env_name} cannot be empty")

    config[field_name] = value


def _validate_optional_path(name: str, value: Any) -> None:
    """Ensure that optional path *value* is either None or a non-empty string."""

    if value is None:
        return
    if not isinstance(value, str):
        raise LogConfigurationError(f"{name} must be a string or null")
    if value.strip() == "":
        raise LogConfigurationError(f"{name} cannot be an empty string")


def _normalize_sinks(
    value: Any,
    file_location: Optional[str],
    network_location: Optional[str],
    database_location: Optional[str] = None,
) -> tuple[str, ...]:
    """Return a validated sink tuple, adding obvious file sinks when needed."""

    if value is None:
        sinks = ["console"]
        if file_location:
            sinks.append("file")
        if network_location:
            sinks.append("network")
        if database_location:
            sinks.append("database")
        return tuple(sinks)

    if not isinstance(value, list):
        raise LogConfigurationError("sinks must be a list")

    normalized: list[str] = []
    for item in value:
        if not isinstance(item, str):
            raise LogConfigurationError("sinks entries must be strings")
        sink = item.strip().lower()
        if sink not in VALID_SINKS:
            raise LogConfigurationError(f"Unknown sink: {item}")
        if sink not in normalized:
            normalized.append(sink)
    return tuple(normalized)


def _normalize_retention(value: Any, database_enabled: bool) -> Dict[str, int]:
    """Return alpha retention settings for the SQLite log store."""

    if value is None:
        return {"max_records": 10000} if database_enabled else {}
    if not isinstance(value, dict):
        raise LogConfigurationError("retention must be an object")

    mode = str(value.get("mode", "records")).strip().lower()
    if mode not in VALID_RETENTION_MODES:
        raise LogConfigurationError(f"retention.mode must be one of {sorted(VALID_RETENTION_MODES)}")

    retention: Dict[str, int] = {}
    raw_max_records = value.get("maxRecords", value.get("max_records"))
    if raw_max_records is None:
        if database_enabled:
            retention["max_records"] = 10000
    elif not isinstance(raw_max_records, int) or raw_max_records <= 0:
        raise LogConfigurationError("retention.maxRecords must be a positive integer")
    else:
        retention["max_records"] = raw_max_records

    raw_max_age_seconds = value.get("maxAgeSeconds", value.get("max_age_seconds"))
    if raw_max_age_seconds is not None:
        if not isinstance(raw_max_age_seconds, int) or raw_max_age_seconds <= 0:
            raise LogConfigurationError("retention.maxAgeSeconds must be a positive integer")
        retention["max_age_seconds"] = raw_max_age_seconds

    raw_max_bytes = value.get("maxBytes", value.get("max_bytes"))
    if raw_max_bytes is not None:
        if not isinstance(raw_max_bytes, int) or raw_max_bytes <= 0:
            raise LogConfigurationError("retention.maxBytes must be a positive integer")
        retention["max_bytes"] = raw_max_bytes

    return retention


def _normalize_rotation(value: Any) -> Dict[str, int]:
    """Return alpha size-based file rotation settings."""

    if value is None:
        return {}
    if not isinstance(value, dict):
        raise LogConfigurationError("rotation must be an object")

    raw_max_bytes = value.get("maxBytes", value.get("max_bytes"))
    raw_max_files = value.get("maxFiles", value.get("max_files"))
    if raw_max_bytes is None and raw_max_files is None:
        return {}
    if raw_max_bytes is None:
        raise LogConfigurationError("rotation.maxBytes is required when rotation.maxFiles is set")
    if not isinstance(raw_max_bytes, int) or raw_max_bytes <= 0:
        raise LogConfigurationError("rotation.maxBytes must be a positive integer")
    if raw_max_files is None:
        raw_max_files = 1
    if not isinstance(raw_max_files, int) or raw_max_files <= 0:
        raise LogConfigurationError("rotation.maxFiles must be a positive integer")

    return {"max_bytes": raw_max_bytes, "max_files": raw_max_files}


def _normalize_redaction(value: Any) -> Dict[str, Any]:
    """Return configured key and regex redaction rules."""

    if value is None:
        return {}
    if not isinstance(value, MappingABC):
        raise LogConfigurationError("redaction must be an object")

    extra = set(value.keys()) - VALID_REDACTION_KEYS
    if extra:
        raise LogConfigurationError(f"Unsupported redaction options: {sorted(extra)}")

    mask = value.get("mask", DEFAULT_REDACTION_MASK)
    if not isinstance(mask, str) or mask == "":
        raise LogConfigurationError("redaction.mask must be a non-empty string")

    keys = _normalize_redaction_strings(value.get("keys", ()), "redaction.keys", lower=True)
    patterns = _normalize_redaction_strings(value.get("patterns", ()), "redaction.patterns")
    for pattern in patterns:
        try:
            re.compile(pattern)
        except re.error as exc:
            raise LogConfigurationError(f"redaction.patterns contains invalid regex: {exc}") from exc

    if not keys and not patterns:
        return {}
    return {"mask": mask, "keys": tuple(keys), "patterns": tuple(patterns)}


def _normalize_redaction_strings(value: Any, field_name: str, *, lower: bool = False) -> tuple[str, ...]:
    """Return unique, non-empty redaction strings from a sequence."""

    if value is None:
        return ()
    if isinstance(value, str) or not isinstance(value, IterableABC):
        raise LogConfigurationError(f"{field_name} must be an array of strings")

    normalized: list[str] = []
    seen: set[str] = set()
    for item in value:
        if not isinstance(item, str) or item.strip() == "":
            raise LogConfigurationError(f"{field_name} must contain only non-empty strings")
        candidate = item.strip().lower() if lower else item
        if candidate not in seen:
            normalized.append(candidate)
            seen.add(candidate)
    return tuple(normalized)


def _normalize_buffering(value: Any) -> Dict[str, Any]:
    """Return configured buffering rules."""

    if value is None:
        return {}
    if not isinstance(value, MappingABC):
        raise LogConfigurationError("buffering must be an object")

    extra = set(value.keys()) - VALID_BUFFERING_KEYS
    if extra:
        raise LogConfigurationError(f"Unsupported buffering options: {sorted(extra)}")

    raw_mode = value.get("mode", "sync")
    if not isinstance(raw_mode, str):
        raise LogConfigurationError(f"buffering.mode must be a string, got {type(raw_mode).__name__}")
    mode = raw_mode.strip().lower()
    if mode not in VALID_BUFFERING_MODES:
        raise LogConfigurationError(f"buffering.mode must be one of {sorted(VALID_BUFFERING_MODES)}")
    if mode == "sync":
        return {"mode": "sync"} if "mode" in value else {}

    raw_capacity = value.get("capacity", value.get("batchSize", value.get("batch_size", DEFAULT_BUFFER_CAPACITY)))
    if not isinstance(raw_capacity, int) or isinstance(raw_capacity, bool) or raw_capacity <= 0:
        raise LogConfigurationError("buffering.capacity must be a positive integer")

    raw_interval = value.get(
        "flushIntervalSeconds",
        value.get(
            "flush_interval_seconds",
            value.get("intervalSeconds", value.get("interval_seconds", DEFAULT_BUFFER_FLUSH_INTERVAL_SECONDS)),
        ),
    )
    if (
        not isinstance(raw_interval, (int, float))
        or isinstance(raw_interval, bool)
        or raw_interval <= 0
    ):
        raise LogConfigurationError("buffering.flushIntervalSeconds must be a positive number")

    return {
        "mode": "async",
        "capacity": raw_capacity,
        "flush_interval_seconds": float(raw_interval),
    }


def _normalize_failure_policy(value: Any) -> Dict[str, Any]:
    """Return configured sink failure behavior."""

    if value is None:
        return {"mode": "warn", "retry_attempts": 0, "retry_delay_seconds": 0.0}
    if not isinstance(value, MappingABC):
        raise LogConfigurationError("failurePolicy must be an object")

    extra = set(value.keys()) - VALID_FAILURE_POLICY_KEYS
    if extra:
        raise LogConfigurationError(f"Unsupported failurePolicy options: {sorted(extra)}")

    raw_mode = value.get("mode", "warn")
    if not isinstance(raw_mode, str):
        raise LogConfigurationError(f"failurePolicy.mode must be a string, got {type(raw_mode).__name__}")
    mode = raw_mode.strip().lower()
    if mode not in VALID_FAILURE_POLICY_MODES:
        raise LogConfigurationError(f"failurePolicy.mode must be one of {sorted(VALID_FAILURE_POLICY_MODES)}")

    raw_retry_attempts = value.get("retryAttempts", value.get("retry_attempts", 1 if mode == "retry" else 0))
    if (
        not isinstance(raw_retry_attempts, int)
        or isinstance(raw_retry_attempts, bool)
        or raw_retry_attempts < 0
    ):
        raise LogConfigurationError("failurePolicy.retryAttempts must be a non-negative integer")

    raw_retry_delay = value.get("retryDelaySeconds", value.get("retry_delay_seconds", 0.0))
    if (
        not isinstance(raw_retry_delay, (int, float))
        or isinstance(raw_retry_delay, bool)
        or raw_retry_delay < 0
    ):
        raise LogConfigurationError("failurePolicy.retryDelaySeconds must be a non-negative number")

    fallback_path = value.get("fallbackPath", value.get("fallback_path"))
    if fallback_path is not None:
        if not isinstance(fallback_path, (str, os.PathLike)):
            raise LogConfigurationError("failurePolicy.fallbackPath must be a string or path-like value")
        fallback_path = str(fallback_path)
    if mode == "fallback":
        _validate_optional_path("failurePolicy.fallbackPath", fallback_path)
        if fallback_path is None:
            raise LogConfigurationError("failurePolicy.fallbackPath is required when mode is fallback")
    elif fallback_path is not None:
        _validate_optional_path("failurePolicy.fallbackPath", fallback_path)

    normalized: Dict[str, Any] = {
        "mode": mode,
        "retry_attempts": raw_retry_attempts,
        "retry_delay_seconds": float(raw_retry_delay),
    }
    if fallback_path is not None:
        normalized["fallback_path"] = str(fallback_path)
    return normalized


class _JsonLogFormatter(logging.Formatter):
    """Render log records as one-line JSON events."""

    def __init__(self, config: _Config) -> None:
        super().__init__()
        self.config = config

    def format(self, record: logging.LogRecord) -> str:
        event = LogEvent(
            logger=self.config.name,
            level=_canonical_level_from_record(record),
            message=_redact_text(record.getMessage(), self.config.redaction),
            timestamp=datetime.fromtimestamp(record.created, timezone.utc) if self.config.timestamp else None,
            metadata=_metadata_for_record(self.config, record),
        )
        return format_json_event(event)


class _TextLogFormatter(logging.Formatter):
    """Render log records with libLogit text-level labels."""

    def __init__(self, config: _Config) -> None:
        super().__init__()
        self.config = config

    def format(self, record: logging.LogRecord) -> str:
        event = LogEvent(
            logger=self.config.name,
            level=_canonical_level_from_record(record),
            message=_redact_text(record.getMessage(), self.config.redaction),
            timestamp=(
                datetime.fromtimestamp(record.created).astimezone().strftime("%Y-%m-%dT%H:%M:%S%z")
                if self.config.timestamp
                else None
            ),
            metadata=_metadata_for_record(self.config, record),
        )
        return format_text_event(event, tag_level=self.config.tag_level, timestamp=self.config.timestamp)


class _CloseAfterEmitFileHandler(logging.FileHandler):
    """Append one record and release the file handle immediately.

    Keeping file handles open is normal for logging, but it is awkward for
    embeddable library users on Windows because project setup and tests often
    create and remove temporary log directories. Alpha favors predictable file
    ownership over maximum throughput; buffered/async sinks can optimize later.
    """

    def __init__(self, filename: str | _Path, rotation: Optional[Dict[str, int]] = None) -> None:
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

        path = _Path(self.baseFilename)
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

    def __init__(self, config: _Config) -> None:
        super().__init__(config.threshold_level)
        if not config.database_location:
            raise LogConfigurationError("databasePath is required when the database sink is enabled")
        self.config = config
        self.database_path = _Path(config.database_location)
        if self.database_path.parent and str(self.database_path.parent) not in {".", ""}:
            self.database_path.parent.mkdir(parents=True, exist_ok=True)
        self._ensure_schema()

    def emit(self, record: logging.LogRecord) -> None:
        try:
            created_at = datetime.fromtimestamp(record.created, timezone.utc).isoformat()
            event = LogEvent(
                logger=self.config.name,
                level=_canonical_level_from_record(record),
                message=_redact_text(record.getMessage(), self.config.redaction),
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

    def __init__(self, filename: str | _Path) -> None:
        fallback_path = _Path(filename)
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


class _LoggerState:
    """Owns the configured :mod: logger instance."""

    def __init__(self, logger_name: str = "liblogit") -> None:
        self.config: Optional[_Config] = None
        self.logger: Optional[Logger] = None
        self.logger_name = logger_name
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
                    file_handler = _CloseAfterEmitFileHandler(file_path, config.rotation)
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
            redacted_message = _redact_text(message, self.config.redaction)
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


def _resolve_file_sink_path(path: str, config: _Config) -> _Path:
    """Resolve a configured path to the concrete file the sink should append."""

    base_path = _Path(path)
    if config.path_mode == "directory":
        return base_path / f"{_safe_logit_filename(config.name)}.log"
    return base_path


def _rotation_backup_path(path: _Path, index: int) -> _Path:
    """Return the backup path for one rotation index."""

    return _Path(f"{path}.{index}")


def _redact_value(value: Any, redaction: Mapping[str, Any]) -> Any:
    """Return *value* with configured redaction rules applied."""

    if not redaction:
        return value

    mask = str(redaction.get("mask", DEFAULT_REDACTION_MASK))
    keys = set(redaction.get("keys", ()))
    if isinstance(value, MappingABC):
        redacted: dict[Any, Any] = {}
        for key, item in value.items():
            if str(key).lower() in keys:
                redacted[key] = mask
            else:
                redacted[key] = _redact_value(item, redaction)
        return redacted
    if isinstance(value, list):
        return [_redact_value(item, redaction) for item in value]
    if isinstance(value, tuple):
        return tuple(_redact_value(item, redaction) for item in value)
    if isinstance(value, str):
        return _redact_text(value, redaction)
    return value


def _redact_text(text: str, redaction: Mapping[str, Any]) -> str:
    """Apply configured regex replacement rules to text."""

    if not redaction:
        return text

    result = text
    mask = str(redaction.get("mask", DEFAULT_REDACTION_MASK))
    for pattern in redaction.get("patterns", ()):
        result = re.sub(str(pattern), mask, result)
    return result


def _redact_metadata(metadata: Dict[str, Any], redaction: Mapping[str, Any]) -> Dict[str, Any]:
    """Return metadata with configured redaction rules applied."""

    redacted = _redact_value(metadata, redaction)
    if isinstance(redacted, MappingABC):
        return dict(redacted)
    return metadata


def _safe_logit_filename(name: str) -> str:
    """Return a deterministic filename fragment for a LOGIT name."""

    cleaned = "".join(character if character.isalnum() or character in {"-", "_", "."} else "_" for character in name)
    cleaned = cleaned.strip("._")
    return cleaned or "default"


def _looks_like_socket(target: str) -> bool:
    """Return True when *target* represents a TCP/UDP socket destination."""

    parsed = urlparse(target)
    return parsed.scheme in {"tcp", "udp"} and parsed.hostname is not None and parsed.port is not None


def _build_format(config: _Config) -> str:
    """Build a logging format string based on *config* toggles."""

    parts: list[str] = []
    if config.timestamp:
        parts.append("%(asctime)s")
    if config.tag_level:
        parts.append("%(levelname)s")
    parts.append("%(message)s")
    return " ".join(parts)


def _normalize_event_metadata(metadata: Optional[Mapping[str, Any]]) -> Dict[str, Any]:
    """Return validated per-message metadata."""

    if metadata is None:
        return {}
    try:
        return merge_metadata(metadata)
    except TypeError as exc:
        raise LogConfigurationError(str(exc)) from exc


def _metadata_for_record(config: _Config, record: logging.LogRecord) -> Dict[str, Any]:
    """Merge static LOGIT metadata with per-message metadata."""

    event_metadata = getattr(record, "liblogit_metadata", None)
    try:
        return _redact_metadata(merge_metadata(config.metadata, event_metadata), config.redaction)
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


_STATE = _LoggerState()
"""Module-level logger state shared by :func: and :func:."""

_REGISTRY: Dict[str, "Logit"] = {}


def init_from_config(path: str | _Path) -> None:
    """Initialise the logging system using the JSON configuration at *path*."""

    config = _Config.from_file(path)
    _STATE.configure(config)


def configure(
    *,
    path: Optional[str | _Path] = "logs/app.log",
    level: str = INFO,
    network_path: Optional[str | _Path] = None,
    database_path: Optional[str | _Path] = None,
    retention: Optional[Dict[str, Any]] = None,
    buffering: Optional[Dict[str, Any]] = None,
    redaction: Optional[Dict[str, Any]] = None,
    failure_policy: Optional[Dict[str, Any]] = None,
    console: bool = True,
    timestamp: bool = True,
    tag_level: bool = True,
    format: str = "text",
    metadata: Optional[Dict[str, Any]] = None,
) -> "Logit":
    """Configure and return the default LOGIT object using direct arguments."""

    sinks: list[str] = []
    if console:
        sinks.append("console")
    if path:
        sinks.append("file")
    if network_path:
        sinks.append("network")
    if database_path:
        sinks.append("database")

    default_logit = Logit(
        name="default",
        path=path,
        level=level,
        network_path=network_path,
        database_path=database_path,
        retention=retention,
        buffering=buffering,
        redaction=redaction,
        failure_policy=failure_policy,
        sinks=sinks,
        timestamp=timestamp,
        tag_level=tag_level,
        format=format,
        metadata=metadata,
    )
    _STATE.configure(default_logit.config)
    _REGISTRY["default"] = default_logit
    return default_logit


def load_logits(path: str | _Path) -> Dict[str, "Logit"]:
    """Load named LOGIT objects from a v2 config or migrate a v0.1 config."""

    try:
        payload = json.loads(_Path(path).read_text(encoding="utf-8"))
    except FileNotFoundError as exc:
        raise LogConfigurationError(f"Configuration file not found: {path}") from exc
    except JSONDecodeError as exc:
        raise LogConfigurationError(f"Invalid JSON configuration: {exc}") from exc

    if "logits" not in payload:
        config = _Config.from_dict(payload)
        logit = Logit.from_config(config)
        _REGISTRY.clear()
        _REGISTRY["default"] = logit
        _STATE.configure(config)
        return dict(_REGISTRY)

    extra = set(payload.keys()) - VALID_V2_TOP_LEVEL_KEYS
    if extra:
        raise LogConfigurationError(f"Unsupported configuration keys: {sorted(extra)}")

    logits = payload.get("logits")
    if not isinstance(logits, dict) or not logits:
        raise LogConfigurationError("logits must be a non-empty object")

    defaults = payload.get("defaults", {})
    if not isinstance(defaults, dict):
        raise LogConfigurationError("defaults must be an object")

    loaded: Dict[str, Logit] = {}
    for name, logit_payload in logits.items():
        if not isinstance(logit_payload, dict):
            raise LogConfigurationError(f"LOGIT '{name}' must be an object")
        config = _Config.from_logit_dict(logit_payload, name=name, defaults=defaults)
        loaded[config.name] = Logit.from_config(config)

    _REGISTRY.clear()
    _REGISTRY.update(loaded)
    if "default" in _REGISTRY:
        _STATE.configure(_REGISTRY["default"].config)
    return dict(_REGISTRY)


def get_logit(name: str = "default") -> "Logit":
    """Return a loaded LOGIT object by name."""

    try:
        return _REGISTRY[name]
    except KeyError as exc:
        raise KeyError(f"No LOGIT named '{name}' has been loaded") from exc


class Logit:
    """User-instantiated logger that owns its own configuration and sinks."""

    def __init__(
        self,
        structure: Optional[Dict[str, Any] | LogitConfig] = None,
        *,
        name: str = "default",
        path: Optional[str | _Path] = None,
        localPath: Optional[str | _Path] = None,
        local_path: Optional[str | _Path] = None,
        level: Optional[str] = None,
        network_path: Optional[str | _Path] = None,
        remotePath: Optional[str | _Path] = None,
        remote_path: Optional[str | _Path] = None,
        databasePath: Optional[str | _Path] = None,
        database_path: Optional[str | _Path] = None,
        database_location: Optional[str | _Path] = None,
        pathMode: Optional[str] = None,
        path_mode: Optional[str] = None,
        retention: Optional[Dict[str, Any]] = None,
        rotation: Optional[Dict[str, Any]] = None,
        redaction: Optional[Dict[str, Any]] = None,
        buffering: Optional[Dict[str, Any]] = None,
        failurePolicy: Optional[Dict[str, Any]] = None,
        failure_policy: Optional[Dict[str, Any]] = None,
        sinks: Optional[Iterable[str]] = None,
        timestamp: bool = True,
        tag_level: bool = True,
        format: str = "text",
        metadata: Optional[Dict[str, Any]] = None,
        enabled: bool = True,
    ) -> None:
        if isinstance(structure, LogitConfig):
            payload = structure.to_structure()
        elif structure is not None and not isinstance(structure, dict):
            raise LogConfigurationError("LOGIT structure must be a dictionary when provided")
        else:
            payload = dict(structure or {})
        resolved_path = _first_path(
            path,
            localPath,
            local_path,
            payload.get("path"),
            payload.get("localPath"),
            payload.get("local_path"),
            payload.get("file_location"),
        )
        resolved_network_path = _first_path(
            network_path,
            remotePath,
            remote_path,
            payload.get("network_path"),
            payload.get("remotePath"),
            payload.get("remote_path"),
            payload.get("network_file_location"),
        )
        resolved_database_path = _first_path(
            database_path,
            databasePath,
            database_location,
            payload.get("database_path"),
            payload.get("databasePath"),
            payload.get("database_location"),
        )
        payload.setdefault("name", name)
        payload.setdefault("level", level)
        payload["path"] = resolved_path
        payload["network_path"] = resolved_network_path
        payload["database_path"] = resolved_database_path
        resolved_path_mode = pathMode if pathMode is not None else path_mode
        if resolved_path_mode is not None:
            payload["pathMode"] = resolved_path_mode
        elif "pathMode" not in payload and "path_mode" not in payload:
            payload["pathMode"] = "file"
        payload.setdefault("timestamp", timestamp)
        payload.setdefault("tag_level", tag_level)
        payload.setdefault("format", format)
        payload.setdefault("metadata", metadata or {})
        payload.setdefault("enabled", enabled)
        if retention is not None:
            payload["retention"] = retention
        if rotation is not None:
            payload["rotation"] = rotation
        if redaction is not None:
            payload["redaction"] = redaction
        if buffering is not None:
            payload["buffering"] = buffering
        resolved_failure_policy = failurePolicy if failurePolicy is not None else failure_policy
        if resolved_failure_policy is not None:
            payload["failurePolicy"] = resolved_failure_policy
        if sinks is not None:
            payload["sinks"] = list(sinks)
        elif "sinks" not in payload:
            payload["sinks"] = list(_normalize_sinks(None, resolved_path, resolved_network_path, resolved_database_path))

        self.config = _Config.from_logit_dict(payload, name=name)
        self._state = _LoggerState(f"liblogit.{self.config.name}")
        self._state.configure(self.config)

    @classmethod
    def from_config(cls, config: _Config) -> "Logit":
        """Build a LOGIT object from an already validated config."""

        logit = cls.__new__(cls)
        logit.config = config
        logit._state = _LoggerState(f"liblogit.{config.name}")
        logit._state.configure(config)
        return logit

    def _replace_config(self, **changes: Any) -> None:
        """Apply mutable LOGIT attribute changes and rebuild handlers."""

        payload: Dict[str, Any] = {
            "name": self.config.name,
            "level": self.config.threshold,
            "path": self.config.file_location,
            "network_path": self.config.network_file_location,
            "database_path": self.config.database_location,
            "pathMode": self.config.path_mode,
            "retention": dict(self.config.retention),
            "rotation": dict(self.config.rotation),
            "redaction": dict(self.config.redaction),
            "buffering": dict(self.config.buffering),
            "failurePolicy": dict(self.config.failure_policy),
            "sinks": list(self.config.sinks),
            "timestamp": self.config.timestamp,
            "tag_level": self.config.tag_level,
            "format": self.config.format,
            "metadata": dict(self.config.metadata),
            "enabled": self.config.enabled,
        }
        payload.update(changes)
        self.config = _Config.from_logit_dict(payload, name=str(payload.get("name", self.config.name)))
        logger_name = f"liblogit.{self.config.name}"
        if self._state.logger_name != logger_name:
            self._state.close()
            self._state = _LoggerState(logger_name)
        self._state.configure(self.config)

    @property
    def name(self) -> str:
        """Return the LOGIT object's configured name."""

        return self.config.name

    @name.setter
    def name(self, value: str) -> None:
        self._replace_config(name=value)

    @property
    def level(self) -> str:
        """Return the minimum severity threshold."""

        return self.config.threshold

    @level.setter
    def level(self, value: str) -> None:
        self._replace_config(level=value)

    @property
    def localPath(self) -> Optional[str]:
        """Return the local file path, if configured."""

        return self.config.file_location

    @localPath.setter
    def localPath(self, value: Optional[str | _Path]) -> None:
        sinks = list(self.config.sinks)
        if value is not None and "file" not in sinks:
            sinks.append("file")
        if value is None and "file" in sinks:
            sinks.remove("file")
        self._replace_config(path=str(value) if value is not None else None, sinks=sinks)

    local_path = localPath
    path = localPath

    @property
    def remotePath(self) -> Optional[str]:
        """Return the remote file path or socket endpoint, if configured."""

        return self.config.network_file_location

    @remotePath.setter
    def remotePath(self, value: Optional[str | _Path]) -> None:
        sinks = list(self.config.sinks)
        if value is not None and "network" not in sinks:
            sinks.append("network")
        if value is None and "network" in sinks:
            sinks.remove("network")
        self._replace_config(network_path=str(value) if value is not None else None, sinks=sinks)

    remote_path = remotePath
    network_path = remotePath

    @property
    def databasePath(self) -> Optional[str]:
        """Return the SQLite log store path, if configured."""

        return self.config.database_location

    @databasePath.setter
    def databasePath(self, value: Optional[str | _Path]) -> None:
        sinks = list(self.config.sinks)
        if value is not None and "database" not in sinks:
            sinks.append("database")
        if value is None and "database" in sinks:
            sinks.remove("database")
        self._replace_config(database_path=str(value) if value is not None else None, sinks=sinks)

    database_path = databasePath
    database_location = databasePath

    @property
    def pathMode(self) -> str:
        """Return whether path-like file sinks are treated as files or directories."""

        return self.config.path_mode

    @pathMode.setter
    def pathMode(self, value: str) -> None:
        self._replace_config(pathMode=value)

    path_mode = pathMode

    @property
    def sinks(self) -> tuple[str, ...]:
        """Return enabled sink names."""

        return self.config.sinks

    @sinks.setter
    def sinks(self, value: Iterable[str]) -> None:
        self._replace_config(sinks=list(value))

    @property
    def format(self) -> str:
        """Return the output format."""

        return self.config.format

    @format.setter
    def format(self, value: str) -> None:
        self._replace_config(format=value)

    @property
    def timestamp(self) -> bool:
        """Return whether timestamp output is enabled."""

        return self.config.timestamp

    @timestamp.setter
    def timestamp(self, value: bool) -> None:
        self._replace_config(timestamp=bool(value))

    @property
    def tagLevel(self) -> bool:
        """Return whether text output includes the level label."""

        return self.config.tag_level

    @tagLevel.setter
    def tagLevel(self, value: bool) -> None:
        self._replace_config(tag_level=bool(value))

    tag_level = tagLevel

    @property
    def enabled(self) -> bool:
        """Return whether this LOGIT emits messages."""

        return self.config.enabled

    @enabled.setter
    def enabled(self, value: bool) -> None:
        self._replace_config(enabled=bool(value))

    @property
    def metadata(self) -> Dict[str, Any]:
        """Return static metadata attached to structured output."""

        return dict(self.config.metadata)

    @metadata.setter
    def metadata(self, value: Dict[str, Any]) -> None:
        self._replace_config(metadata=value)

    @property
    def retention(self) -> Dict[str, int]:
        """Return database retention settings."""

        return dict(self.config.retention)

    @retention.setter
    def retention(self, value: Dict[str, Any]) -> None:
        self._replace_config(retention=value)

    @property
    def rotation(self) -> Dict[str, int]:
        """Return file rotation settings."""

        return dict(self.config.rotation)

    @rotation.setter
    def rotation(self, value: Dict[str, Any]) -> None:
        self._replace_config(rotation=value)

    @property
    def redaction(self) -> Dict[str, Any]:
        """Return configured redaction settings."""

        return dict(self.config.redaction)

    @redaction.setter
    def redaction(self, value: Dict[str, Any]) -> None:
        self._replace_config(redaction=value)

    @property
    def buffering(self) -> Dict[str, Any]:
        """Return configured buffering settings."""

        return dict(self.config.buffering)

    @buffering.setter
    def buffering(self, value: Dict[str, Any]) -> None:
        self._replace_config(buffering=value)

    @property
    def failurePolicy(self) -> Dict[str, Any]:
        """Return configured sink failure policy."""

        return dict(self.config.failure_policy)

    @failurePolicy.setter
    def failurePolicy(self, value: Dict[str, Any]) -> None:
        self._replace_config(failurePolicy=value)

    failure_policy = failurePolicy

    def __call__(self, level: str) -> "_LogBuilder":
        """Return a streaming log builder bound to this LOGIT."""

        return _LogBuilder(level, state=self._state)

    def log(
        self,
        level: str,
        message: Any,
        *,
        metadata: Optional[Mapping[str, Any]] = None,
    ) -> None:
        """Emit *message* at *level* immediately."""

        self._state.log(level, _stringify(message, self.config.redaction), metadata=metadata)

    def flush(self) -> None:
        """Flush every attached sink."""

        self._state.flush()

    def close(self) -> None:
        """Close every attached sink."""

        self._state.close()


def _first_path(*values: Any) -> Optional[str]:
    """Return the first explicitly supplied path value as text."""

    for value in values:
        if value is not None:
            return str(value)
    return None


LOGIT = Logit


class _EndlType:
    """Sentinel used to explicitly flush streaming log builders."""

    __slots__ = ()

    def __repr__(self) -> str:  # pragma: no cover - representational helper
        return "<liblogit.ENDL>"


ENDL = _EndlType()
"""Sentinel value that triggers :meth: when streamed in."""


class _LogBuilder:
    """Imitates the C++ LOG(level) << streaming API in Python."""

    def __init__(self, level: str, *, state: Optional[_LoggerState] = None) -> None:
        self.fragments: list[str] = []
        self._metadata: Dict[str, Any] = {}
        self._committed = False
        self._state = state or _STATE
        self.level = _normalize_level_name(level)

    def __lshift__(self, value: Any) -> "_LogBuilder":
        """Append *value* to the buffer or flush when :data: is observed."""

        if value is ENDL:
            return self.commit()
        redaction = self._state.config.redaction if self._state.config else {}
        self.fragments.append(_stringify(value, redaction))
        return self

    def with_metadata(self, metadata: Mapping[str, Any]) -> "_LogBuilder":
        """Attach per-message metadata to the eventual event."""

        self._metadata = merge_metadata(self._metadata, _normalize_event_metadata(metadata))
        return self

    def commit(self) -> "_LogBuilder":
        """Emit buffered fragments to the configured logger and mark as committed."""

        if not self._committed and self.fragments:
            message = "".join(self.fragments)
            self._state.log(self.level, message, metadata=self._metadata)
            self._committed = True
        return self

    def __del__(self) -> None:  # pragma: no cover - destructor best-effort only
        if self.fragments and not self._committed:
            try:
                self.commit()
            except Exception:
                pass


def LOG(level: str) -> _LogBuilder:
    """Return a streaming log builder for *level*."""

    return _LogBuilder(level)


def _stringify(value: Any, redaction: Optional[Mapping[str, Any]] = None) -> str:
    """Render arbitrary *value* as a string for log output."""

    if redaction:
        value = _redact_value(value, redaction)
    return render_payload(value)


def copy_sample_config(target: str | _Path, *, overwrite: bool = False) -> _Path:
    """Copy the bundled sample configuration to *target* and return the final path.

    If *target* is a directory (existing or not) the file is written as
    ``logit.sample.json`` within that directory. When *target* looks like a JSON
    file path, the sample is written directly to that location instead. Existing
    files are preserved unless ``overwrite`` is True.
    """

    destination = _Path(target)

    if destination.suffix.lower() == ".json" or destination.is_file():
        target_file = destination
    else:
        if destination.exists() and not destination.is_dir():
            raise NotADirectoryError(f"Cannot copy sample config into non-directory: {destination}")
        destination.mkdir(parents=True, exist_ok=True)
        target_file = destination / _SAMPLE_FILENAME

    if target_file.exists() and not overwrite:
        raise FileExistsError(f"Refusing to overwrite existing file: {target_file}")

    try:
        sample_bytes = resources.files(__package__).joinpath("data").joinpath(_SAMPLE_FILENAME).read_bytes()
    except FileNotFoundError as exc:  # pragma: no cover - defensive path
        raise RuntimeError("Bundled liblogit sample configuration is missing") from exc

    target_file.parent.mkdir(parents=True, exist_ok=True)
    target_file.write_bytes(sample_bytes)
    return target_file
