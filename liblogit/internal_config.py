"""Internal configuration parsing for the Python libLogit runtime."""

from __future__ import annotations

import json
import os
import re
from collections.abc import Iterable as IterableABC
from collections.abc import Mapping as MappingABC
from dataclasses import dataclass, field
from json import JSONDecodeError
from pathlib import Path
from typing import Any, Dict, Optional

from .errors import LogConfigurationError
from .levels import INFO, LEVEL_MAP
from .levels import normalize_level_name as _normalize_level_value
from .redaction import DEFAULT_REDACTION_MASK
from .runtime import DEFAULT_BUFFER_CAPACITY, DEFAULT_BUFFER_FLUSH_INTERVAL_SECONDS

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
    def from_file(cls, path: str | Path) -> "_Config":
        """Load configuration from *path* and return the parsed instance."""

        try:
            payload = json.loads(Path(path).read_text(encoding="utf-8"))
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
