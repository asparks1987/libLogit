"""Streaming log builder helpers for the Python libLogit binding."""

from __future__ import annotations

from typing import Any, Mapping, Optional

from .events import merge_metadata, render_payload
from .internal_config import _normalize_level_name
from .redaction import redact_value
from .state import _LoggerState, _normalize_event_metadata


class _EndlType:
    """Sentinel used to explicitly flush streaming log builders."""

    __slots__ = ()

    def __repr__(self) -> str:  # pragma: no cover - representational helper
        return "<liblogit.ENDL>"


ENDL = _EndlType()
"""Sentinel value that triggers :meth: when streamed in."""

_DEFAULT_STATE: Optional[_LoggerState] = None


def _set_default_state(state: _LoggerState) -> None:
    """Set the module-level state used by legacy `LOG(level)` builders."""

    global _DEFAULT_STATE
    _DEFAULT_STATE = state


class _LogBuilder:
    """Imitates the C++ LOG(level) << streaming API in Python."""

    def __init__(self, level: str, *, state: Optional[_LoggerState] = None) -> None:
        self.fragments: list[str] = []
        self._metadata: dict[str, Any] = {}
        self._committed = False
        resolved_state = state or _DEFAULT_STATE
        if resolved_state is None:
            raise RuntimeError("Logger not configured. Call init_from_config first.")
        self._state: _LoggerState = resolved_state
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


def _stringify(value: Any, redaction: Optional[Mapping[str, Any]] = None) -> str:
    """Render arbitrary *value* as a string for log output."""

    if redaction:
        value = redact_value(value, redaction)
    return render_payload(value)
