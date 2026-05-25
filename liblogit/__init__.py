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
from importlib import resources
from json import JSONDecodeError
from logging import Handler
from pathlib import Path as _Path
from typing import Any, Dict, Iterable, Mapping, Optional

from .builder import ENDL, _LogBuilder, _set_default_state, _stringify
from .config import LogitConfig
from .errors import LogConfigurationError
from .events import LogEvent
from .internal_config import VALID_V2_TOP_LEVEL_KEYS, _Config, _normalize_sinks
from .levels import DEBUG, ERROR, FATAL, INFO, TRACE, WARN, Level
from .runtime import _CloseAfterEmitFileHandler
from .state import _LoggerState

__version__ = "1.0.0a1"

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


def _file_handler_factory(filename: _Path, rotation: Mapping[str, int]) -> Handler:
    return _CloseAfterEmitFileHandler(filename, rotation)


def _new_logger_state(logger_name: str = "liblogit") -> _LoggerState:
    return _LoggerState(logger_name, file_handler_factory=_file_handler_factory)


_STATE = _new_logger_state()
"""Module-level logger state shared by :func: and :func:."""
_set_default_state(_STATE)

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
        self._state = _new_logger_state(f"liblogit.{self.config.name}")
        self._state.configure(self.config)

    @classmethod
    def from_config(cls, config: _Config) -> "Logit":
        """Build a LOGIT object from an already validated config."""

        logit = cls.__new__(cls)
        logit.config = config
        logit._state = _new_logger_state(f"liblogit.{config.name}")
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
            self._state = _new_logger_state(logger_name)
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


def LOG(level: str) -> _LogBuilder:
    """Return a streaming log builder for *level*."""

    return _LogBuilder(level, state=_STATE)


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
