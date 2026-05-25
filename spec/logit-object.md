# LOGIT Object Specification

Status: v0.2 alpha draft

This document defines the shared `LOGIT` object contract for the v1 Alpha MVP languages: Python, C++, C, C#, Java, JavaScript, Go, and Kotlin.

## Definition

A `LOGIT` is a user-owned logger instance. It owns its configuration, enabled sinks, formatting choices, metadata, and runtime state. A project may create one `LOGIT` for the whole application or many named `LOGIT` objects for separate modules, services, or audit streams.

The concept must remain stable across languages even when syntax differs.

Canonical model:

```text
LOGIT AppLog = []
AppLog.localPath = "logs/app.log"
AppLog.level = DEBUG

AppLog(INFO) << "service started"
```

## Required Alpha Behaviors

- A user can create a blank `LOGIT` object with defaults.
- A user can create a `LOGIT` from an initial structure.
- A user can mutate basic settings during project startup.
- A user can emit at a level through the object.
- Console and file sinks can be enabled at the same time.
- Remote file paths are treated as secondary file destinations in Alpha.
- Named `LOGIT` objects can be loaded from config v2.
- Legacy v0.1 config loads as one `LOGIT` named `default`.

## Minimum Structure

| Field | Required | Default | Alpha Meaning |
|-------|----------|---------|---------------|
| `name` | No | `default` | Stable object identifier. |
| `path` | No | `null` | Primary local file destination. |
| `localPath` | No | `null` | Alias for `path`; preferred user-facing mutable property. |
| `pathMode` / `path_mode` | No | `file` | `file` appends to the configured path; `directory` creates `<LOGIT name>.log` inside the configured path. |
| `remotePath` | No | `null` | Secondary remote/network file destination. |
| `level` | No | `info` | Minimum severity threshold. |
| `enabled` | No | `true` | When false, events are dropped before sinks receive them. |
| `sinks` | No | `["console"]` or inferred from paths | Enabled destinations. |
| `timestamp` | No | `true` | Include a timestamp in rendered output. |
| `tag_level` | No | `true` | Include the textual level in text output. |
| `format` | No | `text` | `text` or `json`. |
| `metadata` | No | `{}` | Static event metadata, required in JSON output when non-empty. |
| `buffering` | No | `{}` | Optional sync/async batching settings where implemented. |
| `failurePolicy` / `failure_policy` | No | `warn` | Optional per-event sink failure behavior where implemented. |
| `redaction` | No | `{}` | Optional key and regex masking rules where implemented. |
| `rotation` | No | `{}` | Optional file rotation settings such as `maxBytes` and `maxFiles` where implemented. |

Advanced fields such as templates and age-based file rotation are beta-track
unless explicitly promoted.

Message payloads may be plain strings or structured values. Alpha bindings
should support structured maps/lists/arrays where the host language makes that
natural. The Python reference renders those values as compact JSON text before
file, JSON-lines, and SQLite sinks receive the event.

Static `LOGIT` metadata applies to every event. Bindings that support
per-message metadata must merge it with static metadata before structured sinks
receive the event. Per-message metadata wins when the same key exists in both
maps.

Python Alpha redaction masks configured `redaction.keys` case-insensitively and
recursively across payload and metadata objects. It also applies
`redaction.patterns` to rendered message text and string metadata values before
console, file, network-file, or SQLite sinks receive the event. Invalid regexes
fail during configuration.

Python Alpha buffering is synchronous by default. `buffering.mode = "async"`
enables process-local batched output; `buffering.capacity` controls the number
of records held before an automatic flush, and
`buffering.flushIntervalSeconds` controls the timer flush. Buffered records
also flush on explicit `flush()` and `close()`.

Python Alpha failure policy defaults to `warn`. `failurePolicy.mode` supports
`warn`, `drop`, `raise`, `retry`, and `fallback`. Retry mode uses
`retryAttempts` and `retryDelaySeconds`; fallback mode writes to
`fallbackPath`.

Python Alpha serializes `LOGIT` object configuration, logging, flushing, and
closing so concurrent threads cannot interleave or corrupt sink output.

## Direct Instantiation

Each Alpha language must expose a direct construction path. Native syntax may vary, but the user must be able to express:

- Blank declaration.
- Structure-fed declaration.
- Post-construction property assignment or equivalent setter calls.

Examples:

```python
LogIT = LOGIT()
LogIT.localPath = "logs/app.log"
LogIT.level = DEBUG
```

Python also exposes `LogitConfig` as a validated structure object:

```python
LogIT = LOGIT(LogitConfig(path="logs/app.log", level=DEBUG))
```

```cpp
auto LogIT = liblogit::LOGIT{};
LogIT.localPath = "logs/app.log";
LogIT.level = liblogit::Level::DEBUG;
```

```c
liblogit_logit LogIT = liblogit_logit_default();
liblogit_logit_set_local_path(&LogIT, "logs/app.log");
liblogit_logit_set_level(&LogIT, LIBLOGIT_DEBUG);
```

```csharp
var LogIT = new Logit();
LogIT.LocalPath = "logs/app.log";
LogIT.Level = Level.Debug;
```

```java
Logit logIT = new Logit();
logIT.setLocalPath("logs/app.log");
logIT.setLevel(Level.DEBUG);
```

```javascript
const logIT = new LOGIT();
logIT.localPath = "logs/app.log";
logIT.level = "debug";
```

```go
logIT := liblogit.New()
logIT.LocalPath = "logs/app.log"
logIT.Level = liblogit.Debug
```

```kotlin
val logIT = Logit()
logIT.localPath = "logs/app.log"
logIT.level = Level.DEBUG
```

## Config Registry

Config v2 defines a registry of named `LOGIT` objects:

```json
{
  "version": "0.2",
  "defaults": {
    "level": "info",
    "timestamp": true,
    "tag_level": true,
    "sinks": ["console"]
  },
  "logits": {
    "AppLog": {
      "path": "logs/app.log",
      "level": "debug",
      "sinks": ["console", "file"]
    }
  }
}
```

Loaders must apply `defaults` first, then the named object values. Object-level values win.

## Sink Inference

When no explicit `sinks` list is supplied:

- Console is enabled by default.
- A non-null `path` or `localPath` enables the file sink.
- A non-null `remotePath` enables the network sink.

When `sinks` is supplied, it is authoritative.

## Path Semantics

Alpha path-like destinations are explicit:

- `pathMode: "file"` treats `path`, `localPath`, and non-socket `remotePath`
  values as concrete files.
- `pathMode: "directory"` treats those values as directories and writes
  `<LOGIT name>.log` inside each directory.
- Missing parent directories are created where the binding supports local file
  creation.
- Relative paths are resolved by the host process current working directory.
- Empty strings are invalid; use `null` or omit the path to disable a sink.
- Socket-style remote paths such as `tcp://host:9000` are future transport
  targets and are not rewritten by directory mode.

Directory-mode filenames are deterministic. The Python Alpha reference
implementation replaces characters outside letters, numbers, `.`, `_`, and `-`
with `_`, and falls back to `default.log` when the name has no safe characters.

Python Alpha file-like sinks can rotate by size. `rotation.maxBytes` defines the
active-file byte limit, and `rotation.maxFiles` defines the number of numbered
backups retained beside the active log file.

## Environment Override Semantics

Bindings that support config loading should allow deployment-time overrides
without editing JSON. The Python Alpha reference implements this contract.

- Apply `defaults`, then named object values, then environment overrides.
- Global overrides use `LIBLOGIT_<FIELD>` and apply to every loaded `LOGIT`.
- Named overrides use `LIBLOGIT_<NAME>_<FIELD>` and beat global overrides.
- Convert `LOGIT` names to environment fragments by uppercasing ASCII letters
  and digits, replacing separator runs with `_`, and falling back to `DEFAULT`.
- Supported fields are `LEVEL`, `ENABLED`, `PATH`, `LOCAL_PATH`, `REMOTE_PATH`,
  `DATABASE_PATH`, and `PATH_MODE`.
- Empty path override values clear that destination. Empty non-path override
  values are invalid.
- `ENABLED` accepts `true`/`false`, `yes`/`no`, `on`/`off`, and `1`/`0`.
- Explicit `sinks` lists remain authoritative after overrides.

## Output Requirements

Alpha text output is deterministic when `timestamp` is false:

```text
INFO service started
```

Alpha JSON-lines output must be one JSON object per line with stable keys:

```json
{"level":"info","logger":"AppLog","message":"service started","metadata":{"component":"api"}}
```

Timestamp values may differ by language/runtime unless a conformance fixture fixes the clock.
