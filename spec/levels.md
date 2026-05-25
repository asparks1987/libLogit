# Severity Level Specification

Status: v0.2 alpha draft

Every Alpha binding must support these canonical levels in ascending severity order:

| Rank | Name | Aliases | Meaning |
|------|------|---------|---------|
| 0 | `trace` | none | Most detailed diagnostic output. |
| 1 | `debug` | none | Developer diagnostic output. |
| 2 | `info` | none | Normal operational messages. |
| 3 | `warn` | `warning` | Recoverable or suspicious conditions. |
| 4 | `error` | none | Failed operation that does not necessarily stop the process. |
| 5 | `fatal` | none | Severe failure. Alpha only logs it; process termination is not automatic. |

## Parsing

- Level parsing is case-insensitive.
- `warning` normalizes to `warn`.
- Unknown values must produce a configuration or argument error.
- Numeric level values are not part of the Alpha contract.

## Filtering

A `LOGIT` emits an event only when the event level rank is greater than or equal to the object's threshold rank.

Example:

```text
threshold = info
debug -> dropped
info -> emitted
warn -> emitted
```

## Native Mappings

Bindings may map levels to native logging systems internally. Config semantics,
JSON output, SQLite/database rows, and structured event fields must preserve the
canonical names. Human text output may use the display labels below so logs feel
familiar in each ecosystem while still filtering and storing canonical levels.

Recommended mappings and text labels:

| Canonical | Python logging | Java logging | .NET | Common text label |
|-----------|----------------|--------------|------|-------------------|
| `trace` | `NOTSET` | `FINEST` | `Trace` | `TRACE` |
| `debug` | `DEBUG` | `FINE` | `Debug` | `DEBUG` |
| `info` | `INFO` | `INFO` | `Information` | `INFO` |
| `warn` | `WARNING` | `WARNING` | `Warning` | `WARNING` |
| `error` | `ERROR` | `SEVERE` | `Error` | `ERROR` |
| `fatal` | `CRITICAL` | `SEVERE` | `Critical` | `FATAL` |
