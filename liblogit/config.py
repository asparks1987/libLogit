"""Public configuration data model for the Python libLogit binding."""

from __future__ import annotations

import re
from collections.abc import Iterable as IterableABC
from collections.abc import Mapping as MappingABC
from dataclasses import dataclass, field
from os import PathLike
from typing import Any, Iterable, Mapping

from .levels import INFO, normalize_level_name

VALID_SINKS = {"console", "file", "network", "database"}
VALID_FORMATS = {"text", "json"}
VALID_PATH_MODES = {"file", "directory"}
VALID_RETENTION_MODES = {"records", "age", "bytes", "bounded"}
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
DEFAULT_REDACTION_MASK = "[REDACTED]"
DEFAULT_BUFFER_CAPACITY = 100
DEFAULT_BUFFER_FLUSH_INTERVAL_SECONDS = 1.0


@dataclass(frozen=True)
class LogitConfig:
    """Validated, portable configuration for one Python `LOGIT` object."""

    name: str = "default"
    path: str | PathLike[str] | None = None
    remote_path: str | PathLike[str] | None = None
    database_path: str | PathLike[str] | None = None
    path_mode: str = "file"
    level: str = INFO
    enabled: bool = True
    sinks: Iterable[str] | None = None
    timestamp: bool = True
    tag_level: bool = True
    format: str = "text"
    metadata: Mapping[str, Any] = field(default_factory=dict)
    retention: Mapping[str, Any] = field(default_factory=dict)
    rotation: Mapping[str, Any] = field(default_factory=dict)
    redaction: Mapping[str, Any] = field(default_factory=dict)
    buffering: Mapping[str, Any] = field(default_factory=dict)
    failure_policy: Mapping[str, Any] = field(default_factory=dict)

    def __post_init__(self) -> None:
        name = str(self.name).strip()
        if not name:
            raise ValueError("name must not be empty")

        path = _normalize_optional_path("path", self.path)
        remote_path = _normalize_optional_path("remote_path", self.remote_path)
        database_path = _normalize_optional_path("database_path", self.database_path)

        object.__setattr__(self, "name", name)
        object.__setattr__(self, "path", path)
        object.__setattr__(self, "remote_path", remote_path)
        object.__setattr__(self, "database_path", database_path)
        object.__setattr__(self, "path_mode", _normalize_path_mode(self.path_mode))
        object.__setattr__(self, "level", _normalize_public_level(self.level))
        object.__setattr__(self, "enabled", _normalize_bool(self.enabled, "enabled"))
        object.__setattr__(self, "sinks", _normalize_sinks(self.sinks, path, remote_path, database_path))
        object.__setattr__(self, "timestamp", _normalize_bool(self.timestamp, "timestamp"))
        object.__setattr__(self, "tag_level", _normalize_bool(self.tag_level, "tag_level"))
        object.__setattr__(self, "format", _normalize_format(self.format))
        object.__setattr__(self, "metadata", _normalize_metadata(self.metadata))
        object.__setattr__(self, "retention", _normalize_retention(self.retention, database_path is not None))
        object.__setattr__(self, "rotation", _normalize_rotation(self.rotation))
        object.__setattr__(self, "redaction", _normalize_redaction(self.redaction))
        object.__setattr__(self, "buffering", _normalize_buffering(self.buffering))
        object.__setattr__(self, "failure_policy", _normalize_failure_policy(self.failure_policy))

    @classmethod
    def from_structure(
        cls,
        structure: Mapping[str, Any],
        *,
        name: str = "default",
        defaults: Mapping[str, Any] | None = None,
    ) -> "LogitConfig":
        """Validate a v2 LOGIT structure and return a config object."""

        if not isinstance(structure, MappingABC):
            raise ValueError("LOGIT structure must be a mapping")
        if defaults is not None and not isinstance(defaults, MappingABC):
            raise ValueError("LOGIT defaults must be a mapping")

        merged: dict[str, Any] = {}
        for source_name, source in (("defaults", defaults), ("LOGIT", structure)):
            if source is None:
                continue
            extra = set(source) - VALID_LOGIT_KEYS
            if extra:
                raise ValueError(f"Unsupported {source_name} keys: {sorted(extra)}")
            merged.update(source)

        return cls(
            name=str(merged.get("name", name)),
            path=_first_path(
                merged.get("path"),
                merged.get("localPath"),
                merged.get("local_path"),
                merged.get("file_location"),
            ),
            remote_path=_first_path(
                merged.get("remotePath"),
                merged.get("remote_path"),
                merged.get("network_path"),
                merged.get("network_file_location"),
            ),
            database_path=_first_path(
                merged.get("databasePath"),
                merged.get("database_path"),
                merged.get("database_location"),
            ),
            path_mode=merged.get("pathMode", merged.get("path_mode", "file")),
            level=merged.get("level", INFO),
            enabled=merged.get("enabled", True),
            sinks=merged.get("sinks"),
            timestamp=merged.get("timestamp", True),
            tag_level=merged.get("tag_level", True),
            format=merged.get("format", "text"),
            metadata=merged.get("metadata", {}),
            retention=merged.get("retention", {}),
            rotation=merged.get("rotation", {}),
            redaction=merged.get("redaction", {}),
            buffering=merged.get("buffering", {}),
            failure_policy=merged.get("failurePolicy", merged.get("failure_policy", {})),
        )

    def to_structure(self) -> dict[str, Any]:
        """Return the canonical v2 LOGIT structure for this config."""

        payload: dict[str, Any] = {
            "name": self.name,
            "level": self.level,
            "enabled": self.enabled,
            "sinks": list(self.sinks or ()),
            "timestamp": self.timestamp,
            "tag_level": self.tag_level,
            "format": self.format,
            "pathMode": self.path_mode,
        }
        if self.path is not None:
            payload["path"] = self.path
        if self.remote_path is not None:
            payload["remotePath"] = self.remote_path
        if self.database_path is not None:
            payload["databasePath"] = self.database_path
        if self.metadata:
            payload["metadata"] = dict(self.metadata)
        if self.retention:
            payload["retention"] = _retention_to_structure(self.retention)
        if self.rotation:
            payload["rotation"] = _rotation_to_structure(self.rotation)
        if self.redaction:
            payload["redaction"] = _redaction_to_structure(self.redaction)
        if self.buffering:
            payload["buffering"] = _buffering_to_structure(self.buffering)
        if self.failure_policy:
            payload["failurePolicy"] = _failure_policy_to_structure(self.failure_policy)
        return payload


def _first_path(*values: Any) -> Any:
    for value in values:
        if value is not None:
            return value
    return None


def _normalize_optional_path(name: str, value: str | PathLike[str] | None) -> str | None:
    if value is None:
        return None
    if not isinstance(value, (str, PathLike)):
        raise ValueError(f"{name} must be a string or path-like value")
    path = str(value)
    if not path:
        raise ValueError(f"{name} must not be empty")
    return path


def _normalize_path_mode(value: Any) -> str:
    if not isinstance(value, str):
        raise ValueError(f"pathMode must be a string, got {type(value).__name__}")
    path_mode = value.strip().lower()
    if path_mode not in VALID_PATH_MODES:
        raise ValueError(f"pathMode must be one of {sorted(VALID_PATH_MODES)}")
    return path_mode


def _normalize_public_level(value: Any) -> str:
    try:
        return normalize_level_name(value)
    except (TypeError, ValueError) as exc:
        raise ValueError(str(exc)) from exc


def _normalize_bool(value: Any, name: str) -> bool:
    if not isinstance(value, bool):
        raise ValueError(f"{name} must be a boolean")
    return value


def _normalize_sinks(
    value: Iterable[str] | None,
    path: str | None,
    remote_path: str | None,
    database_path: str | None,
) -> tuple[str, ...]:
    if value is None:
        sinks = ["console"]
        if path:
            sinks.append("file")
        if remote_path:
            sinks.append("network")
        if database_path:
            sinks.append("database")
        return tuple(sinks)

    if isinstance(value, str) or not isinstance(value, IterableABC):
        raise ValueError("sinks must be an iterable of strings")

    normalized: list[str] = []
    for item in value:
        if not isinstance(item, str):
            raise ValueError("sinks entries must be strings")
        sink = item.strip().lower()
        if sink not in VALID_SINKS:
            raise ValueError(f"Unknown sink: {item}")
        if sink not in normalized:
            normalized.append(sink)
    return tuple(normalized)


def _normalize_format(value: Any) -> str:
    if not isinstance(value, str):
        raise ValueError(f"format must be a string, got {type(value).__name__}")
    log_format = value.strip().lower()
    if log_format not in VALID_FORMATS:
        raise ValueError(f"format must be one of {sorted(VALID_FORMATS)}")
    return log_format


def _normalize_metadata(value: Any) -> dict[str, Any]:
    if not isinstance(value, MappingABC):
        raise ValueError("metadata must be an object")
    return dict(value)


def _normalize_retention(value: Any, database_enabled: bool) -> dict[str, int]:
    if value is None:
        return {"max_records": 10000} if database_enabled else {}
    if not isinstance(value, MappingABC):
        raise ValueError("retention must be an object")
    mode = value.get("mode")
    if mode is not None and mode not in VALID_RETENTION_MODES:
        raise ValueError(f"retention.mode must be one of {sorted(VALID_RETENTION_MODES)}")

    retention: dict[str, int] = {}
    max_records = value.get("maxRecords", value.get("max_records"))
    if max_records is None:
        if database_enabled:
            retention["max_records"] = 10000
    elif not isinstance(max_records, int) or max_records <= 0:
        raise ValueError("retention.maxRecords must be a positive integer")
    else:
        retention["max_records"] = max_records

    max_age_seconds = value.get("maxAgeSeconds", value.get("max_age_seconds"))
    if max_age_seconds is not None:
        if not isinstance(max_age_seconds, int) or max_age_seconds <= 0:
            raise ValueError("retention.maxAgeSeconds must be a positive integer")
        retention["max_age_seconds"] = max_age_seconds

    max_bytes = value.get("maxBytes", value.get("max_bytes"))
    if max_bytes is not None:
        if not isinstance(max_bytes, int) or max_bytes <= 0:
            raise ValueError("retention.maxBytes must be a positive integer")
        retention["max_bytes"] = max_bytes

    return retention


def _normalize_rotation(value: Any) -> dict[str, int]:
    if value is None:
        return {}
    if not isinstance(value, MappingABC):
        raise ValueError("rotation must be an object")

    max_bytes = value.get("maxBytes", value.get("max_bytes"))
    max_files = value.get("maxFiles", value.get("max_files"))
    if max_bytes is None and max_files is None:
        return {}
    if max_bytes is None:
        raise ValueError("rotation.maxBytes is required when rotation.maxFiles is set")
    if not isinstance(max_bytes, int) or max_bytes <= 0:
        raise ValueError("rotation.maxBytes must be a positive integer")
    if max_files is None:
        max_files = 1
    if not isinstance(max_files, int) or max_files <= 0:
        raise ValueError("rotation.maxFiles must be a positive integer")
    return {"max_bytes": max_bytes, "max_files": max_files}


def _normalize_redaction(value: Any) -> dict[str, Any]:
    if value is None:
        return {}
    if not isinstance(value, MappingABC):
        raise ValueError("redaction must be an object")

    extra = set(value.keys()) - VALID_REDACTION_KEYS
    if extra:
        raise ValueError(f"Unsupported redaction options: {sorted(extra)}")

    mask = value.get("mask", DEFAULT_REDACTION_MASK)
    if not isinstance(mask, str) or mask == "":
        raise ValueError("redaction.mask must be a non-empty string")

    keys = _normalize_redaction_strings(value.get("keys", ()), "redaction.keys", lower=True)
    patterns = _normalize_redaction_strings(value.get("patterns", ()), "redaction.patterns")
    for pattern in patterns:
        try:
            re.compile(pattern)
        except re.error as exc:
            raise ValueError(f"redaction.patterns contains invalid regex: {exc}") from exc

    if not keys and not patterns:
        return {}
    return {"mask": mask, "keys": tuple(keys), "patterns": tuple(patterns)}


def _normalize_redaction_strings(value: Any, field_name: str, *, lower: bool = False) -> tuple[str, ...]:
    if value is None:
        return ()
    if isinstance(value, str) or not isinstance(value, IterableABC):
        raise ValueError(f"{field_name} must be an array of strings")

    normalized: list[str] = []
    seen: set[str] = set()
    for item in value:
        if not isinstance(item, str) or item.strip() == "":
            raise ValueError(f"{field_name} must contain only non-empty strings")
        candidate = item.strip().lower() if lower else item
        if candidate not in seen:
            normalized.append(candidate)
            seen.add(candidate)
    return tuple(normalized)


def _normalize_buffering(value: Any) -> dict[str, Any]:
    if value is None:
        return {}
    if not isinstance(value, MappingABC):
        raise ValueError("buffering must be an object")

    extra = set(value.keys()) - VALID_BUFFERING_KEYS
    if extra:
        raise ValueError(f"Unsupported buffering options: {sorted(extra)}")

    mode = value.get("mode", "sync")
    if not isinstance(mode, str):
        raise ValueError(f"buffering.mode must be a string, got {type(mode).__name__}")
    mode = mode.strip().lower()
    if mode not in VALID_BUFFERING_MODES:
        raise ValueError(f"buffering.mode must be one of {sorted(VALID_BUFFERING_MODES)}")
    if mode == "sync":
        return {"mode": "sync"} if "mode" in value else {}

    capacity = value.get("capacity", value.get("batchSize", value.get("batch_size", DEFAULT_BUFFER_CAPACITY)))
    if not isinstance(capacity, int) or isinstance(capacity, bool) or capacity <= 0:
        raise ValueError("buffering.capacity must be a positive integer")

    interval = value.get(
        "flushIntervalSeconds",
        value.get(
            "flush_interval_seconds",
            value.get("intervalSeconds", value.get("interval_seconds", DEFAULT_BUFFER_FLUSH_INTERVAL_SECONDS)),
        ),
    )
    if not isinstance(interval, (int, float)) or isinstance(interval, bool) or interval <= 0:
        raise ValueError("buffering.flushIntervalSeconds must be a positive number")

    return {
        "mode": "async",
        "capacity": capacity,
        "flush_interval_seconds": float(interval),
    }


def _normalize_failure_policy(value: Any) -> dict[str, Any]:
    if value is None:
        return {}
    if not isinstance(value, MappingABC):
        raise ValueError("failurePolicy must be an object")

    extra = set(value.keys()) - VALID_FAILURE_POLICY_KEYS
    if extra:
        raise ValueError(f"Unsupported failurePolicy options: {sorted(extra)}")

    mode = value.get("mode", "warn")
    if not isinstance(mode, str):
        raise ValueError(f"failurePolicy.mode must be a string, got {type(mode).__name__}")
    mode = mode.strip().lower()
    if mode not in VALID_FAILURE_POLICY_MODES:
        raise ValueError(f"failurePolicy.mode must be one of {sorted(VALID_FAILURE_POLICY_MODES)}")

    retry_attempts = value.get("retryAttempts", value.get("retry_attempts", 1 if mode == "retry" else 0))
    if not isinstance(retry_attempts, int) or isinstance(retry_attempts, bool) or retry_attempts < 0:
        raise ValueError("failurePolicy.retryAttempts must be a non-negative integer")

    retry_delay = value.get("retryDelaySeconds", value.get("retry_delay_seconds", 0.0))
    if not isinstance(retry_delay, (int, float)) or isinstance(retry_delay, bool) or retry_delay < 0:
        raise ValueError("failurePolicy.retryDelaySeconds must be a non-negative number")

    fallback_path = value.get("fallbackPath", value.get("fallback_path"))
    if mode == "fallback":
        fallback_path = _normalize_optional_path("failurePolicy.fallbackPath", fallback_path)
        if fallback_path is None:
            raise ValueError("failurePolicy.fallbackPath is required when mode is fallback")
    elif fallback_path is not None:
        fallback_path = _normalize_optional_path("failurePolicy.fallbackPath", fallback_path)

    normalized: dict[str, Any] = {
        "mode": mode,
        "retry_attempts": retry_attempts,
        "retry_delay_seconds": float(retry_delay),
    }
    if fallback_path is not None:
        normalized["fallback_path"] = fallback_path
    return normalized


def _retention_to_structure(retention: Mapping[str, Any]) -> dict[str, int]:
    payload: dict[str, int] = {}
    if "max_records" in retention:
        payload["maxRecords"] = int(retention["max_records"])
    if "max_age_seconds" in retention:
        payload["maxAgeSeconds"] = int(retention["max_age_seconds"])
    if "max_bytes" in retention:
        payload["maxBytes"] = int(retention["max_bytes"])
    return payload


def _rotation_to_structure(rotation: Mapping[str, Any]) -> dict[str, int]:
    payload: dict[str, int] = {}
    if "max_bytes" in rotation:
        payload["maxBytes"] = int(rotation["max_bytes"])
    if "max_files" in rotation:
        payload["maxFiles"] = int(rotation["max_files"])
    return payload


def _redaction_to_structure(redaction: Mapping[str, Any]) -> dict[str, Any]:
    payload: dict[str, Any] = {}
    if "mask" in redaction:
        payload["mask"] = str(redaction["mask"])
    if "keys" in redaction:
        payload["keys"] = list(redaction["keys"])
    if "patterns" in redaction:
        payload["patterns"] = list(redaction["patterns"])
    return payload


def _buffering_to_structure(buffering: Mapping[str, Any]) -> dict[str, Any]:
    payload: dict[str, Any] = {}
    if "mode" in buffering:
        payload["mode"] = str(buffering["mode"])
    if "capacity" in buffering:
        payload["capacity"] = int(buffering["capacity"])
    if "flush_interval_seconds" in buffering:
        payload["flushIntervalSeconds"] = float(buffering["flush_interval_seconds"])
    return payload


def _failure_policy_to_structure(failure_policy: Mapping[str, Any]) -> dict[str, Any]:
    payload: dict[str, Any] = {}
    if "mode" in failure_policy:
        payload["mode"] = str(failure_policy["mode"])
    if "retry_attempts" in failure_policy:
        payload["retryAttempts"] = int(failure_policy["retry_attempts"])
    if "retry_delay_seconds" in failure_policy:
        payload["retryDelaySeconds"] = float(failure_policy["retry_delay_seconds"])
    if "fallback_path" in failure_policy:
        payload["fallbackPath"] = str(failure_policy["fallback_path"])
    return payload
