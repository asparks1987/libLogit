"""Level definitions shared by the Python libLogit binding."""

from __future__ import annotations

import logging
from enum import Enum
from functools import total_ordering
from typing import Any


@total_ordering
class Level(str, Enum):
    """Canonical libLogit severity names in ascending order."""

    TRACE = "trace"
    DEBUG = "debug"
    INFO = "info"
    WARN = "warn"
    ERROR = "error"
    FATAL = "fatal"

    @classmethod
    def parse(cls, value: str | "Level") -> "Level":
        """Parse a user supplied level name or return an existing level."""

        if isinstance(value, cls):
            return value
        if not isinstance(value, str):
            raise TypeError(f"level must be a string or Level, got {type(value).__name__}")
        normalized = value.strip().lower()
        if normalized == "warning":
            normalized = cls.WARN.value
        try:
            return cls(normalized)
        except ValueError as exc:
            raise ValueError(f"Unknown level: {value}") from exc

    @property
    def rank(self) -> int:
        """Return the stable ordering rank for this severity."""

        return _LEVEL_RANKS[self]

    @property
    def python_level(self) -> int:
        """Return the matching :mod:`logging` severity."""

        return LEVEL_MAP[self.value]

    def __lt__(self, other: Any) -> bool:
        try:
            other_level = Level.parse(other)
        except (TypeError, ValueError):
            return NotImplemented
        return self.rank < other_level.rank

    def __str__(self) -> str:
        return self.value


TRACE = Level.TRACE.value
DEBUG = Level.DEBUG.value
INFO = Level.INFO.value
WARN = Level.WARN.value
ERROR = Level.ERROR.value
FATAL = Level.FATAL.value

_LEVEL_RANKS = {
    Level.TRACE: 0,
    Level.DEBUG: 1,
    Level.INFO: 2,
    Level.WARN: 3,
    Level.ERROR: 4,
    Level.FATAL: 5,
}

LEVEL_MAP = {
    TRACE: logging.NOTSET,
    DEBUG: logging.DEBUG,
    INFO: logging.INFO,
    WARN: logging.WARNING,
    "warning": logging.WARNING,
    ERROR: logging.ERROR,
    FATAL: logging.CRITICAL,
}
"""Mapping of canonical/accepted level names to Python logging severities."""


def normalize_level_name(level: str | Level) -> str:
    """Return the canonical lower-case level name."""

    return Level.parse(level).value
