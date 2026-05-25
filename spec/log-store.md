# Log Store Specification

Status: v0.1 alpha showcase draft

The log store is the database-backed part of libLogit. It gives desktop users a
bounded, SQL-readable history of events emitted through one or more `LOGIT`
objects.

## Alpha Goal

v1 Alpha must include a functional example of the final workflow:

1. A user imports the libLogit SDK in an alpha language.
2. The user declares or loads a `LOGIT`.
3. The `LOGIT` writes to console, file, and a local database sink.
4. The database sink keeps a configured amount of recent history.
5. The user can inspect stored logs with SQL or a minimal viewer window.

Alpha does not need to prove every operating system, hardware profile, or
language binding. That broad validation is beta-track.

## Default Store

SQLite is the default alpha store because it is embedded, portable, file-based,
and readable through standard SQL tools.

Config sketch:

```json
{
  "logits": {
    "AppLog": {
      "level": "debug",
      "sinks": ["console", "file", "database"],
      "path": "logs/app.log",
      "databasePath": "logs/app-logit.sqlite",
      "retention": {
        "mode": "records",
        "maxRecords": 10000
      }
    }
  }
}
```

## Event Table

Alpha table name: `logit_events`.

| Column | Type | Meaning |
|--------|------|---------|
| `id` | integer primary key | Monotonic local event id. |
| `created_at` | text | Timestamp string in ISO-compatible form. |
| `sequence` | integer | Per-store insertion sequence. |
| `logger` | text | `LOGIT` name. |
| `level` | text | Canonical level: `trace`, `debug`, `info`, `warn`, `error`, `fatal`. |
| `message` | text | Rendered message text. |
| `format` | text | `text` or `json`. |
| `metadata_json` | text nullable | Static/per-event metadata encoded as JSON. |
| `source` | text nullable | Optional process/module/source label. |

Indexes:

- `idx_logit_events_logger_created_at`
- `idx_logit_events_level_created_at`
- `idx_logit_events_sequence`

## Retention

Alpha retention modes:

- `records`: keep the newest `maxRecords` rows.
- `bytes`: keep retained event payload text under `maxBytes` where supported.
- `age`: keep rows newer than `maxAgeSeconds`.
- `bounded`: apply every configured limit without giving one limit a special
  mode name.

When more than one limit is configured, every limit applies. Eviction always
removes the oldest records first. This is the database equivalent of
round-robin logging: the store remains bounded while the newest events remain
available.

For Alpha, `maxBytes` is a logical budget over stored event text fields, not a
guarantee that the SQLite file on disk immediately shrinks below that byte
count. SQLite may keep free pages until vacuumed by external tooling.

## Viewer

The alpha viewer only needs to prove the workflow:

- Open a SQLite log store file.
- Show recent events in a desktop window.
- Filter by logger, level, time range, and text search.
- Refresh after new events are written.

The viewer may be implemented in the most practical local stack for Alpha.
Native-feeling viewers for every OS are beta-track.
