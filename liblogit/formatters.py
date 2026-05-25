"""Formatter helpers for structured libLogit events."""

from __future__ import annotations

import json

from .events import LogEvent

DISPLAY_LEVELS = {
    "trace": "TRACE",
    "debug": "DEBUG",
    "info": "INFO",
    "warn": "WARNING",
    "error": "ERROR",
    "fatal": "FATAL",
}


def format_json_event(event: LogEvent) -> str:
    """Render one event as canonical JSON-lines text."""

    return json.dumps(event.as_json_object(), separators=(",", ":"), sort_keys=True)


def format_text_event(event: LogEvent, *, tag_level: bool = True, timestamp: bool = False) -> str:
    """Render one event using the Alpha text format."""

    parts: list[str] = []
    if timestamp and event.timestamp is not None:
        parts.append(event.timestamp.isoformat() if hasattr(event.timestamp, "isoformat") else str(event.timestamp))
    if tag_level:
        parts.append(DISPLAY_LEVELS.get(event.level_name, event.level_name.upper()))
    parts.append(event.rendered_message)
    return " ".join(parts)
