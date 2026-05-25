"""Runtime redaction helpers for libLogit events."""

from __future__ import annotations

import re
from collections.abc import Mapping as MappingABC
from typing import Any, Mapping

DEFAULT_REDACTION_MASK = "[REDACTED]"


def redact_value(value: Any, redaction: Mapping[str, Any]) -> Any:
    """Return *value* with configured redaction rules applied."""

    if not redaction:
        return value

    mask = str(redaction.get("mask", DEFAULT_REDACTION_MASK))
    keys = set(redaction.get("keys", ()))
    if isinstance(value, MappingABC):
        redacted: dict[Any, Any] = {}
        for key, item in value.items():
            if str(key).lower() in keys:
                redacted[key] = mask
            else:
                redacted[key] = redact_value(item, redaction)
        return redacted
    if isinstance(value, list):
        return [redact_value(item, redaction) for item in value]
    if isinstance(value, tuple):
        return tuple(redact_value(item, redaction) for item in value)
    if isinstance(value, str):
        return redact_text(value, redaction)
    return value


def redact_text(text: str, redaction: Mapping[str, Any]) -> str:
    """Apply configured regex replacement rules to text."""

    if not redaction:
        return text

    result = text
    mask = str(redaction.get("mask", DEFAULT_REDACTION_MASK))
    for pattern in redaction.get("patterns", ()):
        result = re.sub(str(pattern), mask, result)
    return result


def redact_metadata(metadata: Mapping[str, Any], redaction: Mapping[str, Any]) -> dict[str, Any]:
    """Return metadata with configured redaction rules applied."""

    redacted = redact_value(dict(metadata), redaction)
    if isinstance(redacted, MappingABC):
        return dict(redacted)
    return dict(metadata)
