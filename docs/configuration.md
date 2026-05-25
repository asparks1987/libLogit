# libLogit Configuration

libLogit configuration is a portable JSON structure for named `LOGIT` objects.
The same shape is used by every alpha binding, even when each language exposes
different native syntax.

The bundled `logit.sample.json` uses the v2 registry shape. The legacy v1
sample remains available at `examples/config/v1-legacy.json` and
`liblogit/data/logit.v1.sample.json` for migration work.

## v2 Registry

```json
{
  "version": "0.2",
  "defaults": {
    "level": "info",
    "timestamp": true,
    "tag_level": true,
    "format": "text",
    "sinks": ["console"]
  },
  "logits": {
    "AppLog": {
      "path": "logs/app.log",
      "pathMode": "file",
      "databasePath": "logs/app-logit.sqlite",
      "level": "debug",
      "sinks": ["console", "file", "database"],
      "retention": {
        "mode": "records",
        "maxRecords": 10000
      }
    }
  }
}
```

`defaults` are applied first. Each named `LOGIT` can override only the fields it
needs.

Python users can validate and reuse the same shape before constructing a logger:

```python
from liblogit import LOGIT, LogitConfig

config = LogitConfig.from_structure({
    "name": "AppLog",
    "localPath": "logs/app.log",
    "level": "debug",
    "timestamp": False
})

LogIT = LOGIT(config)
LogIT.log("info", {"event": "configured"})
```

## Fields

| Field | Default | Meaning |
|-------|---------|---------|
| `name` | registry key or `default` | Stable logger identifier. |
| `path`, `localPath`, `local_path`, `file_location` | `null` | Primary local file destination. |
| `pathMode`, `path_mode` | `file` | Interprets local and remote path sinks as a concrete file path or as a directory that receives `<LOGIT name>.log`. |
| `remotePath`, `remote_path`, `network_path`, `network_file_location` | `null` | Secondary remote/network file destination. |
| `databasePath`, `database_path`, `database_location` | `null` | Local SQLite log store destination. |
| `level` | `info` | Minimum severity: `trace`, `debug`, `info`, `warn`, `warning`, `error`, `fatal`. |
| `enabled` | `true` | Drops events before sinks receive them when false. |
| `sinks` | inferred | `console`, `file`, `network`, `database`. |
| `timestamp` | `true` | Include timestamps in rendered output and stored events. |
| `tag_level` | `true` | Include textual level labels in text output. |
| `format` | `text` | `text` or `json`. |
| `metadata` | `{}` | Static metadata included in JSON output and database rows. |
| `buffering.mode` | `sync` | `sync` writes immediately; Python Alpha also supports `async`, `buffered`, or `batch` as batched mode aliases. |
| `buffering.capacity` | `100` | Maximum records kept in memory before an automatic batch flush. |
| `buffering.flushIntervalSeconds` | `1.0` | Maximum seconds before buffered records are flushed by a background timer. |
| `failurePolicy.mode` | `warn` | Per-event sink failure behavior: `warn`, `drop`, `raise`, `retry`, or `fallback`. |
| `failurePolicy.retryAttempts` | `1` for retry mode | Extra emit attempts after a sink failure. |
| `failurePolicy.retryDelaySeconds` | `0` | Delay between retry attempts. |
| `failurePolicy.fallbackPath` | `null` | File path used when `mode` is `fallback`. |
| `redaction.keys` | `[]` | Case-insensitive metadata or payload field names that are replaced before sinks receive an event. |
| `redaction.patterns` | `[]` | Python regular expressions applied to rendered message text and string metadata values. |
| `redaction.mask` | `[REDACTED]` | Replacement text used for key and pattern matches. |
| `retention.mode` | `records` | Alpha database retention mode: `records`, `age`, `bytes`, or `bounded`. |
| `retention.maxRecords` | `10000` | Maximum recent rows kept by the SQLite sink. |
| `retention.maxAgeSeconds` | `null` | Maximum age for retained SQLite rows. Older rows are evicted. |
| `retention.maxBytes` | `null` | Alpha logical payload byte budget for retained SQLite rows. |
| `rotation.maxBytes` | `null` | Python Alpha file-sink size limit that triggers rotation before the next write. |
| `rotation.maxFiles` | `1` | Number of numbered backup files kept beside the active log file when `rotation.maxBytes` is set. |

If `sinks` is omitted, libLogit starts with `console` and infers `file`,
`network`, and `database` from configured paths. If `sinks` is supplied, it is
authoritative.

Static `metadata` applies to every event emitted by that `LOGIT`. Python Alpha
also accepts per-message metadata:

```python
LogIT.log("info", "request handled", metadata={"request_id": "abc-123"})
LogIT("warn").with_metadata({"attempt": 2}) << "retry" << ENDL
```

When static and per-message metadata use the same key, the per-message value
wins. JSON-lines output and SQLite `metadata_json` both receive the merged map.

## Buffering And Flush

By default, Python Alpha writes synchronously: when `LOGIT.log(...)` returns,
console and file-like sinks have received the event. For lower overhead in
chatty paths, set `buffering.mode` to `async`, `buffered`, or `batch`:

```json
{
  "version": "0.2",
  "defaults": {
    "level": "debug",
    "sinks": ["file"],
    "buffering": {
      "mode": "async",
      "capacity": 25,
      "flushIntervalSeconds": 1.0
    }
  },
  "logits": {
    "BufferedApp": {
      "path": "logs/buffered-app.log"
    }
  }
}
```

Buffered mode flushes when the in-memory batch reaches `capacity`, when the
timer reaches `flushIntervalSeconds`, when `LogIT.flush()` is called, or when
`LogIT.close()` reconfigures or shuts down the logger. Buffered records are
process-local memory until flushed, so use `sync` for crash-critical audit
events.

## Thread Safety

Python Alpha serializes `LOGIT` object configuration, logging, flushing, and
closing. Multiple threads can write to the same `LOGIT` without interleaving
file lines or dropping buffered events.

## Failure Policies

Python Alpha defaults to `failurePolicy.mode: "warn"` so sink failures do not
take down the host application. Use a stricter or quieter policy when the
project needs one:

```json
{
  "version": "0.2",
  "defaults": {
    "level": "debug",
    "sinks": ["file"],
    "failurePolicy": {
      "mode": "fallback",
      "fallbackPath": "logs/fallback.log"
    }
  },
  "logits": {
    "ResilientApp": {
      "path": "logs/resilient-app.log"
    }
  }
}
```

`warn` writes a concise warning to standard error and continues. `drop`
silently discards the failed event for that sink. `raise` propagates the sink
exception back to the caller. `retry` replays the failed event according to
`retryAttempts` and `retryDelaySeconds`. `fallback` writes the event to
`fallbackPath` when the primary sink fails.

## Redaction

Python Alpha can mask configured fields and text patterns before events are
formatted for console, file, network-file, or SQLite sinks:

```json
{
  "version": "0.2",
  "defaults": {
    "level": "debug",
    "format": "json",
    "redaction": {
      "mask": "[MASKED]",
      "keys": ["password", "secret", "token"],
      "patterns": ["(?i)api[_-]?key=[^\\s&]+"]
    }
  },
  "logits": {
    "SecureApp": {
      "path": "logs/secure-app.jsonl"
    }
  }
}
```

`redaction.keys` match payload and metadata object keys case-insensitively and
recursively. `redaction.patterns` are Python regular expressions applied to
string payloads, rendered structured payloads, and string metadata values.
Invalid regular expressions fail during configuration so a project does not
start logging with a broken redaction policy.

## Path Rules

By default, `path` and `remotePath` are file paths. libLogit appends directly to
the configured file and creates missing parent directories. Relative paths are
resolved by the host runtime from the current working directory; use absolute
paths when a service or desktop launcher may start from a different directory.

When `pathMode` is `directory`, the configured local and remote paths are
directories. The Alpha Python reference sink writes a stable filename inside
that directory using the `LOGIT` name:

```json
{
  "version": "0.2",
  "defaults": {
    "level": "info",
    "timestamp": false,
    "sinks": ["file"],
    "pathMode": "directory"
  },
  "logits": {
    "AppLog": {
      "path": "logs/app"
    }
  }
}
```

The example above writes to `logs/app/AppLog.log`. Characters outside letters,
numbers, `.`, `_`, and `-` are replaced with `_` in generated filenames. Empty
paths are invalid; use `null` or omit the field to disable that destination.
Socket-style remote values such as `tcp://host:9000` are not affected by
`pathMode`.

## File Rotation

Python Alpha supports size-based rotation for file-like sinks:

```json
{
  "version": "0.2",
  "defaults": {
    "level": "debug",
    "timestamp": false,
    "sinks": ["file"],
    "rotation": {
      "maxBytes": 4096,
      "maxFiles": 3
    }
  },
  "logits": {
    "AppLog": {
      "path": "logs/app.log"
    }
  }
}
```

When the active file would exceed `rotation.maxBytes`, libLogit rotates before
writing the next event. Backups are kept as `app.log.1`, `app.log.2`, and so on
up to `rotation.maxFiles`; the highest numbered file is removed first. Age-based
file rotation and compression remain beta-track.

## Environment Overrides

The Alpha Python reference can override loaded config with environment
variables. Global variables apply to every loaded `LOGIT`; named variables use
the sanitized uppercase `LOGIT` name and win over the global value. For example,
`AppLog` reads `LIBLOGIT_APPLOG_LEVEL`, and `Audit.Log` reads
`LIBLOGIT_AUDIT_LOG_LEVEL`.

| Global variable | Named form | Meaning |
|-----------------|------------|---------|
| `LIBLOGIT_LEVEL` | `LIBLOGIT_<NAME>_LEVEL` | Override the minimum severity. |
| `LIBLOGIT_ENABLED` | `LIBLOGIT_<NAME>_ENABLED` | Override enabled state with `true`/`false`, `yes`/`no`, `on`/`off`, or `1`/`0`. |
| `LIBLOGIT_PATH` | `LIBLOGIT_<NAME>_PATH` | Override the local file path. |
| `LIBLOGIT_LOCAL_PATH` | `LIBLOGIT_<NAME>_LOCAL_PATH` | Alias for the local file path. |
| `LIBLOGIT_REMOTE_PATH` | `LIBLOGIT_<NAME>_REMOTE_PATH` | Override the remote/network path. |
| `LIBLOGIT_DATABASE_PATH` | `LIBLOGIT_<NAME>_DATABASE_PATH` | Override the SQLite database path. |
| `LIBLOGIT_PATH_MODE` | `LIBLOGIT_<NAME>_PATH_MODE` | Override `pathMode` with `file` or `directory`. |

Environment overrides are applied after `defaults` and named object values are
merged. Empty path override values clear that path, which can disable an
inferred sink without editing JSON. Empty `LEVEL`, `ENABLED`, or `PATH_MODE`
values are invalid. If `sinks` is supplied explicitly, it remains authoritative;
changing a path does not automatically add a sink that the list omitted.

## Legacy v1

Existing v0.1 style config still loads as one `LOGIT` named `default`:

```json
{
  "level": { "threshold": "info", "tag": true },
  "timestamp": true,
  "file_location": "logs/app.log",
  "network_file_location": null
}
```

## SQLite Log Store

The alpha Python reference implementation creates this table automatically when
the `database` sink is enabled:

```sql
SELECT created_at, logger, level, message
FROM logit_events
ORDER BY sequence DESC
LIMIT 25;
```

The store is bounded by `retention.maxRecords` by default; oldest rows are
removed first. The Alpha Python reference also supports `maxAgeSeconds` and
`maxBytes`. `maxBytes` is a logical event payload budget rather than a physical
SQLite file-size guarantee, because SQLite may retain page space until vacuumed.

## Failure Handling

- File paths that cannot be opened cause a warning and the SDK keeps running.
- Socket destinations (`tcp://` and `udp://`) that fail to initialize surface a
  warning and keep local sinks active.
- SQLite setup errors surface a warning and the SDK keeps any other configured
  sinks active.
- Per-event sink failures follow `failurePolicy`: warn, drop, raise, retry, or
  write to a fallback file.
