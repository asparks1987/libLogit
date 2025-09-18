"""Python binding for the libLogit JSON-configured logging toolkit.

This module exposes a tiny public surface consisting of the init_from_config function,
the LOG helper, the ENDL sentinel, and the LogConfigurationError exception.

Usage example::

    from liblogit import init_from_config, LOG, ENDL
    init_from_config("logit.json")
    LOG("info") << "Boot" << ENDL
    LOG("warn") << {"event": "calibration"} << ENDL

The configuration format is intentionally minimal and documented in
docs/configuration.md and the sample logit.sample.json file.
"""

from __future__ import annotations

import json
import logging
from dataclasses import dataclass
from json import JSONDecodeError
from logging import Logger
from logging.handlers import SocketHandler
from pathlib import Path as _Path
from typing import Any, Dict, Iterable, Iterator, Optional
from urllib.parse import urlparse

__all__ = ["init_from_config", "LOG", "ENDL", "LogConfigurationError"]


LEVEL_MAP: Dict[str, int] = {
    "trace": logging.NOTSET,
    "debug": logging.DEBUG,
    "info": logging.INFO,
    "warn": logging.WARNING,
    "warning": logging.WARNING,
    "error": logging.ERROR,
    "fatal": logging.CRITICAL,
}
"""Mapping of textual level names to Python logging severities."""

VALID_TOP_LEVEL_KEYS = {"level", "timestamp", "file_location", "network_file_location"}
"""Set of supported top-level keys in the JSON configuration."""

VALID_LEVEL_KEYS = {"threshold", "tag"}
"""Set of supported keys inside the `level` object."""


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

        level_cfg = payload.get("level")
        if level_cfg is None:
            raise LogConfigurationError("Missing required field: level")

        if isinstance(level_cfg, str):
            threshold = level_cfg
            tag_level = True
        elif isinstance(level_cfg, dict):
            extra_level = set(level_cfg.keys()) - VALID_LEVEL_KEYS
            if extra_level:
                raise LogConfigurationError(f"Unsupported level options: {sorted(extra_level)}")
            threshold = level_cfg.get("threshold")
            if threshold is None:
                raise LogConfigurationError("level.threshold is required when level is an object")
            tag_level = bool(level_cfg.get("tag", True))
        else:
            raise LogConfigurationError("level must be a string or object")

        normalized_threshold = _normalize_level_name(threshold)
        timestamp = bool(payload.get("timestamp", True))
        file_location = payload.get("file_location")
        network_location = payload.get("network_file_location")

        _validate_optional_path("file_location", file_location)
        _validate_optional_path("network_file_location", network_location)

        return cls(
            threshold=normalized_threshold,
            tag_level=tag_level,
            timestamp=timestamp,
            file_location=file_location or None,
            network_file_location=network_location or None,
        )

    @property
    def threshold_level(self) -> int:
        """Return the :mod: level corresponding to :attr:."""

        return LEVEL_MAP[self.threshold]


def _normalize_level_name(level: str) -> str:
    """Normalise user supplied *level* text to the canonical form."""

    norm = level.strip().lower()
    if norm not in LEVEL_MAP:
        raise LogConfigurationError(f"Unknown level: {level}")
    if norm == "warning":
        norm = "warn"
    return norm


def _validate_optional_path(name: str, value: Any) -> None:
    """Ensure that optional path *value* is either None or a non-empty string."""

    if value is None:
        return
    if not isinstance(value, str):
        raise LogConfigurationError(f"{name} must be a string or null")
    if value.strip() == "":
        raise LogConfigurationError(f"{name} cannot be an empty string")


class _LoggerState:
    """Owns the configured :mod: logger instance."""

    def __init__(self) -> None:
        self.config: Optional[_Config] = None
        self.logger: Optional[Logger] = None

    def configure(self, config: _Config) -> None:
        """Configure / reconfigure the internal logger to match *config*."""

        self.config = config
        logger = logging.getLogger("liblogit")
        logger.setLevel(config.threshold_level)
        logger.propagate = False

        for handler in list(logger.handlers):
            logger.removeHandler(handler)

        formatter = logging.Formatter(
            _build_format(config),
            datefmt="%Y-%m-%dT%H:%M:%S%z",
        )

        console_handler = logging.StreamHandler()
        console_handler.setFormatter(formatter)
        console_handler.setLevel(config.threshold_level)
        logger.addHandler(console_handler)

        for path in _filter_paths([config.file_location, config.network_file_location]):
            file_path = _Path(path)
            try:
                if file_path.parent and str(file_path.parent) not in {".", ""}:
                    file_path.parent.mkdir(parents=True, exist_ok=True)
                file_handler = logging.FileHandler(file_path, encoding="utf-8")
            except (OSError, ValueError) as exc:
                logger.warning("Unable to attach file logger at %s: %s", path, exc)
                continue
            file_handler.setFormatter(formatter)
            file_handler.setLevel(config.threshold_level)
            logger.addHandler(file_handler)

        if config.network_file_location and _looks_like_socket(config.network_file_location):
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
                    socket_handler.setFormatter(formatter)
                    socket_handler.setLevel(config.threshold_level)
                    logger.addHandler(socket_handler)

        self.logger = logger

    def log(self, level: str, message: str) -> None:
        """Send *message* to the configured logger at *level*."""

        if self.logger is None or self.config is None:
            raise RuntimeError("Logger not configured. Call init_from_config first.")

        normalized = _normalize_level_name(level)
        self.logger.log(LEVEL_MAP[normalized], message)


def _filter_paths(paths: Iterable[Optional[str]]) -> Iterator[str]:
    """Yield non-empty paths that are not socket descriptors."""

    for path in paths:
        if path and not _looks_like_socket(path):
            yield path


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


_STATE = _LoggerState()
"""Module-level logger state shared by :func: and :func:."""


def init_from_config(path: str | _Path) -> None:
    """Initialise the logging system using the JSON configuration at *path*."""

    config = _Config.from_file(path)
    _STATE.configure(config)


class _EndlType:
    """Sentinel used to explicitly flush streaming log builders."""

    __slots__ = ()

    def __repr__(self) -> str:  # pragma: no cover - representational helper
        return "<liblogit.ENDL>"


ENDL = _EndlType()
"""Sentinel value that triggers :meth: when streamed in."""


class _LogBuilder:
    """Imitates the C++ LOG(level) << streaming API in Python."""

    def __init__(self, level: str) -> None:
        self.level = _normalize_level_name(level)
        self.fragments: list[str] = []
        self._committed = False

    def __lshift__(self, value: Any) -> "_LogBuilder":
        """Append *value* to the buffer or flush when :data: is observed."""

        if value is ENDL:
            return self.commit()
        self.fragments.append(_stringify(value))
        return self

    def commit(self) -> "_LogBuilder":
        """Emit buffered fragments to the configured logger and mark as committed."""

        if not self._committed and self.fragments:
            message = "".join(self.fragments)
            _STATE.log(self.level, message)
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


def _stringify(value: Any) -> str:
    """Render arbitrary *value* as a string for log output."""

    if isinstance(value, str):
        return value
    if isinstance(value, (dict, list, tuple)):
        try:
            return json.dumps(value)
        except TypeError:
            pass
    return str(value)
