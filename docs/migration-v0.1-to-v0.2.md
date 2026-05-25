# Migrating From v0.1 Global Logging To v0.2 LOGIT Registries

libLogit v0.2 keeps the old Python global logger path working, but the v1
Alpha SDK is built around named `LOGIT` objects. A `LOGIT` owns its own level,
paths, sinks, formatter, metadata, and database retention settings.

Use this guide when moving a project from one global logger to one or more
named loggers.

## What Changed

| v0.1 shape | v0.2 Alpha shape |
|------------|------------------|
| One global config object. | A registry of named `LOGIT` objects. |
| `level.threshold` and `level.tag`. | `level` and `tag_level`. |
| `file_location`. | `path`, `localPath`, `local_path`, or `file_location`. |
| `network_file_location`. | `remotePath`, `remote_path`, `network_path`, or `network_file_location`. |
| Python-only global `LOG(level)`. | Per-object calls such as `AppLog(INFO)` or `AppLog.at("info")`. |

## Before: v0.1 Config

```json
{
  "level": {
    "threshold": "info",
    "tag": true
  },
  "timestamp": true,
  "file_location": "logs/app.log",
  "network_file_location": null
}
```

This still loads in Python as a single `LOGIT` named `default`:

```python
from liblogit import ENDL, LOG, init_from_config

init_from_config("logit.json")
LOG("info") << "old call path still works" << ENDL
```

## After: v0.2 Registry

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
      "level": "debug",
      "sinks": ["console", "file"]
    },
    "AuditLog": {
      "path": "logs/audit.jsonl",
      "format": "json",
      "metadata": {
        "component": "audit"
      },
      "sinks": ["file"]
    }
  }
}
```

Python can load the registry directly:

```python
from liblogit import INFO, load_logits

logs = load_logits("logit.json")
logs["AppLog"](INFO) << "new named logger"
logs["AuditLog"].log("info", "audit event")
```

Every Alpha binding follows the same concept, with idiomatic punctuation for
the language:

```javascript
const { loadLogits, levels } = require("@liblogit/liblogit");

const logs = loadLogits("logit.json");
logs.AppLog.at(levels.INFO).append("new named logger").commit();
```

## Field Mapping

| Old field | New field | Notes |
|-----------|-----------|-------|
| `level.threshold` | `level` | String value such as `debug`, `info`, `warn`, or `error`. |
| `level.tag` | `tag_level` | Keeps text output like `INFO message` when true. |
| `file_location` | `path` or `localPath` | Alpha bindings accept both legacy and new names. |
| `network_file_location` | `remotePath` | Alpha treats non-socket remote paths as files. |
| none | `format` | `text` or `json`. |
| none | `sinks` | `console`, `file`, `network`, and Python-only `database` in Alpha. |
| none | `databasePath` | Python Alpha SQLite store path. |
| none | `buffering.mode` | Python Alpha synchronous or async/batched output mode. |
| none | `buffering.capacity` | Python Alpha number of buffered records before batch flush. |
| none | `buffering.flushIntervalSeconds` | Python Alpha timer flush interval for buffered records. |
| none | `failurePolicy.mode` | Python Alpha per-event sink failure behavior. |
| none | `failurePolicy.retryAttempts` | Python Alpha retry count after sink failure. |
| none | `failurePolicy.fallbackPath` | Python Alpha fallback file path after sink failure. |
| none | `retention.maxRecords` | Python Alpha bounded SQLite store row count. |
| none | `retention.maxAgeSeconds` | Python Alpha bounded SQLite store age limit. |
| none | `retention.maxBytes` | Python Alpha logical payload byte budget. |
| none | `redaction.keys` | Python Alpha payload and metadata field masking. |
| none | `redaction.patterns` | Python Alpha regex masking for message text and string metadata values. |
| none | `redaction.mask` | Python Alpha replacement text for redaction matches. |
| none | `rotation.maxBytes` | Python Alpha size-based file rotation trigger. |
| none | `rotation.maxFiles` | Python Alpha number of file backups to retain. |

## Recommended Migration Steps

1. Copy the current config and add `"version": "0.2"`.
2. Move shared settings into `defaults`.
3. Create one entry under `logits` for each logging purpose, such as `AppLog`,
   `AuditLog`, or `RemoteLog`.
4. Replace global calls with object-bound calls in new code.
5. Keep the old Python `init_from_config` and `LOG(level)` path only where you
   need a compatibility bridge.
6. Run the Alpha verifier or the binding-specific tests listed in
   [docs/api/README.md](api/README.md).

## Compatibility Notes

- Python preserves the historic `libLogit.py` module name through a shim.
- v0.1 configs load as a single Python `default` logger.
- The non-Python Alpha bindings also accept legacy field names when loading
  config, but their primary examples use the v0.2 registry.
- Real remote transports and cross-binding buffering/redaction/failure-policy
  parity are beta-track features.
