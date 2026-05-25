# Sink Interface Specification

Status: v0.2 alpha draft

Sinks are the destination layer for `LOGIT` events. The Alpha SDK exposes
console, file, network-path-as-file, and Python SQLite database sinks through
configuration. Beta can add richer implementations without changing the event
contract.

## Event Contract

Every sink accepts a `LogEvent` equivalent:

| Field | Meaning |
|-------|---------|
| `logger` | The owning `LOGIT` name. |
| `level` | Canonical level: `trace`, `debug`, `info`, `warn`, `error`, `fatal`. |
| `message` | Deterministically rendered payload text. |
| `timestamp` | Optional event time. |
| `metadata` | Static and per-event metadata after deterministic merging. |
| `source` | Optional runtime source such as module or logger name. |

## Required Operations

Each sink implementation should support:

- `emit(event)`: accept one event.
- `flush()`: make buffered data visible where buffering exists.
- `close()`: release file handles, sockets, database connections, or other
  runtime resources.

The Python Alpha reference captures this as `liblogit.sinks.base.LogSink`.

## Buffering

The default sink path is synchronous. Python Alpha also supports a process-local
buffered mode. Buffered sinks must flush when their batch reaches configured
capacity, when the configured interval fires, when `flush()` is called, and when
the owning `LOGIT` closes or reconfigures. Cross-binding buffering parity and
backpressure rules remain Beta conformance targets.

## Thread Safety

Bindings should prevent concurrent emits from corrupting sink output. Python
Alpha serializes object-level configure/log/flush/close operations and verifies
that concurrent file and buffered file output preserves complete event lines
without drops.

## Redaction

Redaction is a pre-sink runtime step. When configured, the runtime must mask
matching payload fields, metadata fields, and message text before any sink sees
the event object. Python Alpha implements `redaction.keys`,
`redaction.patterns`, and `redaction.mask`; cross-binding parity is a Beta
conformance target.

## Alpha Failure Policy

Alpha sinks favor application continuity by default:

- Configuration errors should be raised before the `LOGIT` is used.
- Runtime sink setup failures should warn and leave other configured sinks
  active where possible.
- Per-event sink failures default to `warn` and must not corrupt other sinks.

Python Alpha implements explicit `warn`, `drop`, `raise`, `retry`, and
`fallback` policies. Cross-binding parity, richer backoff, and fallback sink
selection are Beta conformance targets.

## File Rotation

Python Alpha file-like sinks may rotate by size:

- `rotation.maxBytes` triggers rotation before an event would exceed the active
  file's byte budget.
- `rotation.maxFiles` keeps numbered backups beside the active file.
- Numbered backups use the source path plus a numeric suffix, such as
  `app.log.1` and `app.log.2`.

Age-based file rotation, compression, and cross-binding rotation parity are
beta-track.
