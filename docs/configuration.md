# Shared Configuration Draft

This draft captures the baseline structure for the universal libLogit configuration. It will evolve through Sprint 0 as the schema is validated with prototypes and stakeholders.

## Goals
- Single file describes log levels, sinks, formatting, and metadata enrichment.
- Works across languages via generated bindings or thin loaders.
- Supports environment overrides for secrets and deployment-specific tweaks.

## Proposed Structure
```toml
level = "info"          # default minimum level applied to all sinks unless overridden
format = "default"      # named formatter preset (e.g., default, json, pretty)

[context]
include_process = true
include_thread = true
static = { app = "sample-service" }

[[sinks]]
name = "console"
type = "console"
level = "debug"
options = { style = "pretty" }

[[sinks]]
name = "file"
type = "file"
level = "info"
options = { path = "logs/app.log", rotation = { policy = "daily", retain = 7 } }

[[sinks]]
name = "remote"
type = "http"
level = "warn"
options = { endpoint = "https://logs.example.com/ingest", api_key_env = "LOGIT_REMOTE_KEY" }
```

## Open Questions
- Should we standardize on TOML for readability or prefer YAML/JSON for broader tooling support?
- How will per-language loaders map unknown sink types (plugin discovery vs. explicit registry)?
- Do we need global batching/buffering settings or should those live on each sink?

## Next Steps
1. Translate the structure above into `schema/logit.schema.json` (JSON Schema draft 2020-12).
2. Prototype configuration loader in Python to validate schema against real files.
3. Capture decisions and trade-offs in ADRs (`docs/adr/`).

Feedback from Sprint 0 will be folded into this document before finalizing the schema contract.
