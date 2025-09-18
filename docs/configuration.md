# Shared Configuration Draft

The libLogit configuration is a compact JSON document that all language bindings load. Phase 1 constrains the file to four top-level fields so behaviour stays consistent while the foundational libraries mature.

The Python package bundles a ready-to-edit sample. Run
`liblogit.copy_sample_config(".")` to drop `logit.sample.json` into your
project and tweak the fields below.

## Allowed Fields
1. `level` — Severity settings. Accepts either a string threshold or an object containing `threshold` (string) and optional `tag` (boolean).
2. `timestamp` — Boolean toggle controlling whether logs include time and date metadata.
3. `file_location` — Optional string path for local file logging. Null or omission disables file output.
4. `network_file_location` — Optional string path for network/remote file output (e.g., UNC share). Null or omission disables the sink.

No other keys are permitted at this stage; schema validation rejects additional properties.

## Example
```json
{
  "level": { "threshold": "info", "tag": true },
  "timestamp": true,
  "file_location": "logs/app.log",
  "network_file_location": null
}
```

## Validation Rules
- `level.threshold` must be one of `trace`, `debug`, `info`, `warn`, `error`, `fatal`.
- `level.tag` defaults to `true` when omitted.
- `timestamp` defaults to `true` when omitted.
- `file_location` and `network_file_location` accept strings or null. Absolute and relative paths are both allowed for now.

## Upcoming Enhancements
- Expand network support to include protocol-based endpoints (HTTP, TCP) while retaining backwards compatibility.
- Introduce optional rotation and buffering settings once the baseline sinks are stable.
- Support environment variable interpolation for secrets and deployment-specific overrides.

## Failure Handling
- When a configured file path cannot be created or opened, libLogit logs a warning and continues emitting messages to the console-only handler.
- Socket destinations (`tcp://` and `udp://`) that fail to initialise also surface a warning and fall back to console output.
