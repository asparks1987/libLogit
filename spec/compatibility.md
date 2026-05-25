# Compatibility Specification

Status: v0.2 alpha draft

libLogit must preserve the v0.1 global config workflow while moving toward named `LOGIT` objects.

## v0.1 Config

Existing v0.1 files use this shape:

```json
{
  "level": { "threshold": "info", "tag": true },
  "timestamp": true,
  "file_location": "logs/app.log",
  "network_file_location": null
}
```

All Alpha bindings that support config loading must treat this as one `LOGIT` named `default`.
Python's `load_logits(path)` returns a registry containing only that `default`
object for v0.1 files, and `get_logit("default")` returns the migrated object
after loading.

Mapping:

| v0.1 Field | v0.2 Field |
|------------|------------|
| `level.threshold` or `level` | `level` |
| `level.tag` | `tag_level` |
| `timestamp` | `timestamp` |
| `file_location` | `path` |
| `network_file_location` | `remotePath` |

## Public API Compatibility

The Python compatibility helpers remain valid:

```python
init_from_config("logit.json")
LOG("info") << "message" << ENDL
```

New code should prefer:

```python
LogIT = LOGIT()
LogIT.localPath = "logs/app.log"
LogIT("info") << "message" << ENDL
```

Other Alpha bindings may add compatibility shims where historical APIs already exist, but new bindings should start with the `LOGIT` object model.
