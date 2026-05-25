"""Event and structured payload helpers for the Python libLogit binding."""

from __future__ import annotations

import json
from collections.abc import Mapping as MappingABC
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Mapping

from .levels import Level, normalize_level_name


def render_payload(value: Any) -> str:
    """Render a log payload into deterministic text.

    Strings pass through unchanged. Mapping/list/tuple values render as compact
    JSON with sorted object keys so text files, JSON-lines messages, and the
    SQLite store all receive the same representation.
    """

    if isinstance(value, str):
        return value
    if isinstance(value, tuple):
        value = list(value)
    if isinstance(value, (dict, list)):
        try:
            return json.dumps(value, separators=(",", ":"), sort_keys=True)
        except (TypeError, ValueError):
            pass
    return str(value)


def merge_metadata(*values: Mapping[str, Any] | None) -> dict[str, Any]:
    """Merge metadata maps with deterministic later-value precedence."""

    merged: dict[str, Any] = {}
    for value in values:
        if value is None:
            continue
        if not isinstance(value, MappingABC):
            raise TypeError(f"metadata must be an object, got {type(value).__name__}")
        merged.update(dict(value))
    return merged


@dataclass(frozen=True)
class LogEvent:
    """Structured representation of one libLogit event."""

    logger: str
    level: str | Level
    message: Any
    timestamp: datetime | str | None = None
    metadata: Mapping[str, Any] = field(default_factory=dict)
    source: str | None = None

    def __post_init__(self) -> None:
        object.__setattr__(self, "level", normalize_level_name(self.level))
        object.__setattr__(self, "message", render_payload(self.message))
        object.__setattr__(self, "metadata", merge_metadata(self.metadata))

    @property
    def level_name(self) -> str:
        """Return the canonical level name."""

        return str(self.level)

    @property
    def rendered_message(self) -> str:
        """Return the deterministic text representation of the payload."""

        return str(self.message)

    @property
    def metadata_json(self) -> str | None:
        """Return compact JSON metadata for persistence, if present."""

        if not self.metadata:
            return None
        return json.dumps(dict(self.metadata), separators=(",", ":"), sort_keys=True)

    def as_json_object(self) -> dict[str, Any]:
        """Return the stable JSON-lines object for this event."""

        event: dict[str, Any] = {
            "logger": self.logger,
            "level": self.level_name,
            "message": self.rendered_message,
        }
        if self.timestamp is not None:
            event["timestamp"] = (
                self.timestamp.isoformat() if isinstance(self.timestamp, datetime) else self.timestamp
            )
        if self.metadata:
            event["metadata"] = dict(self.metadata)
        if self.source:
            event["source"] = self.source
        return event
