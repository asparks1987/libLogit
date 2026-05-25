"""Base sink protocol for Python libLogit events."""

from __future__ import annotations

from typing import Protocol

from ..events import LogEvent


class SinkError(RuntimeError):
    """Raised when a sink cannot accept an event."""


class LogSink(Protocol):
    """Minimal sink contract for future Python sink implementations."""

    def emit(self, event: LogEvent) -> None:
        """Write one event to the sink."""

    def flush(self) -> None:
        """Flush buffered data, if the sink buffers."""

    def close(self) -> None:
        """Release sink resources."""
