import json
import logging
from dataclasses import dataclass
from json import JSONDecodeError
from logging import Logger
from logging.handlers import SocketHandler
from pathlib import Path as _Path
from typing import Any, Dict, Iterable, Optional
from urllib.parse import urlparse

__all__ = ['init_from_config', 'LOG', 'ENDL', 'LogConfigurationError']

LEVEL_MAP = {
    'trace': logging.NOTSET,
    'debug': logging.DEBUG,
    'info': logging.INFO,
    'warn': logging.WARNING,
    'warning': logging.WARNING,
    'error': logging.ERROR,
    'fatal': logging.CRITICAL,
}

VALID_TOP_LEVEL_KEYS = {'level', 'timestamp', 'file_location', 'network_file_location'}
VALID_LEVEL_KEYS = {'threshold', 'tag'}


class LogConfigurationError(RuntimeError):
    """Raised when a configuration file fails validation."""


@dataclass
class _Config:
    threshold: str
    tag_level: bool
    timestamp: bool
    file_location: Optional[str]
    network_file_location: Optional[str]

    @classmethod
    def from_file(cls, path: str | _Path) -> '_Config':
        try:
            payload = json.loads(_Path(path).read_text(encoding='utf-8'))
        except FileNotFoundError as exc:
            raise LogConfigurationError(f'Configuration file not found: {path}') from exc
        except JSONDecodeError as exc:
            raise LogConfigurationError(f'Invalid JSON configuration: {exc}') from exc
        return cls.from_dict(payload)

    @classmethod
    def from_dict(cls, payload: Dict[str, Any]) -> '_Config':
        extra = set(payload.keys()) - VALID_TOP_LEVEL_KEYS
        if extra:
            raise LogConfigurationError(f'Unsupported configuration keys: {sorted(extra)}')

        level_cfg = payload.get('level')
        if level_cfg is None:
            raise LogConfigurationError('Missing required field: level')

        if isinstance(level_cfg, str):
            threshold = level_cfg
            tag_level = True
        elif isinstance(level_cfg, dict):
            extra_level = set(level_cfg.keys()) - VALID_LEVEL_KEYS
            if extra_level:
                raise LogConfigurationError(f'Unsupported level options: {sorted(extra_level)}')
            threshold = level_cfg.get('threshold')
            if threshold is None:
                raise LogConfigurationError('level.threshold is required when level is an object')
            tag_level = bool(level_cfg.get('tag', True))
        else:
            raise LogConfigurationError('level must be a string or object')

        normalized_threshold = _normalize_level_name(threshold)
        timestamp = bool(payload.get('timestamp', True))
        file_location = payload.get('file_location')
        network_location = payload.get('network_file_location')

        _validate_optional_path('file_location', file_location)
        _validate_optional_path('network_file_location', network_location)

        return cls(
            threshold=normalized_threshold,
            tag_level=tag_level,
            timestamp=timestamp,
            file_location=file_location or None,
            network_file_location=network_location or None,
        )

    @property
    def threshold_level(self) -> int:
        return LEVEL_MAP[self.threshold]


def _normalize_level_name(level: str) -> str:
    norm = level.strip().lower()
    if norm not in LEVEL_MAP:
        raise LogConfigurationError(f'Unknown level: {level}')
    if norm == 'warning':
        norm = 'warn'
    return norm


def _validate_optional_path(name: str, value: Any) -> None:
    if value is None:
        return
    if not isinstance(value, str):
        raise LogConfigurationError(f'{name} must be a string or null')
    if value.strip() == '':
        raise LogConfigurationError(f'{name} cannot be an empty string')


class _LoggerState:
    def __init__(self) -> None:
        self.config: Optional[_Config] = None
        self.logger: Optional[Logger] = None

    def configure(self, config: _Config) -> None:
        self.config = config
        logger = logging.getLogger('liblogit')
        logger.setLevel(config.threshold_level)
        logger.propagate = False

        for handler in list(logger.handlers):
            logger.removeHandler(handler)

        formatter = logging.Formatter(
            _build_format(config),
            datefmt='%Y-%m-%dT%H:%M:%S%z',
        )

        console_handler = logging.StreamHandler()
        console_handler.setFormatter(formatter)
        console_handler.setLevel(config.threshold_level)
        logger.addHandler(console_handler)

        for path in _filter_paths([config.file_location, config.network_file_location]):
            _Path(path).parent.mkdir(parents=True, exist_ok=True)
            file_handler = logging.FileHandler(path)
            file_handler.setFormatter(formatter)
            file_handler.setLevel(config.threshold_level)
            logger.addHandler(file_handler)

        if config.network_file_location and _looks_like_socket(config.network_file_location):
            parsed = urlparse(config.network_file_location)
            if parsed.hostname and parsed.port:
                socket_handler = SocketHandler(parsed.hostname, parsed.port)
                socket_handler.setFormatter(formatter)
                socket_handler.setLevel(config.threshold_level)
                logger.addHandler(socket_handler)

        self.logger = logger

    def log(self, level: str, message: str) -> None:
        if self.logger is None or self.config is None:
            raise RuntimeError('Logger not configured. Call init_from_config first.')
        normalized = _normalize_level_name(level)
        self.logger.log(LEVEL_MAP[normalized], message)


def _filter_paths(paths: Iterable[Optional[str]]) -> Iterable[str]:
    for path in paths:
        if path and not _looks_like_socket(path):
            yield path


def _looks_like_socket(target: str) -> bool:
    parsed = urlparse(target)
    return parsed.scheme in {'tcp', 'udp'} and parsed.hostname is not None and parsed.port is not None


def _build_format(config: _Config) -> str:
    parts: list[str] = []
    if config.timestamp:
        parts.append('%(asctime)s')
    if config.tag_level:
        parts.append('%(levelname)s')
    parts.append('%(message)s')
    return ' '.join(parts)


_STATE = _LoggerState()


def init_from_config(path: str | _Path) -> None:
    config = _Config.from_file(path)
    _STATE.configure(config)


class _LogBuilder:
    def __init__(self, level: str) -> None:
        self.level = _normalize_level_name(level)
        self.fragments: list[str] = []
        self._committed = False

    def __lshift__(self, value: Any) -> '_LogBuilder':
        if value is ENDL:
            return self.commit()
        self.fragments.append(_stringify(value))
        return self

    def commit(self) -> '_LogBuilder':
        if not self._committed and self.fragments:
            message = ''.join(self.fragments)
            _STATE.log(self.level, message)
            self._committed = True
        return self

    def __del__(self) -> None:
        if self.fragments and not self._committed:
            try:
                self.commit()
            except Exception:
                pass


ENDL = object()


def LOG(level: str) -> _LogBuilder:
    return _LogBuilder(level)


def _stringify(value: Any) -> str:
    if isinstance(value, str):
        return value
    if isinstance(value, (dict, list, tuple)):
        try:
            return json.dumps(value)
        except TypeError:
            pass
    return str(value)
