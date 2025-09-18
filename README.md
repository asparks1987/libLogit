# libLogit

libLogit is a rich, multi-language logging helper library that aims to deliver a unified configuration experience and an expressive logging syntax (`LOG(DEBUG) << "message"`). Once configured in the main entry point of an application, every other module can emit logs without reconfiguring transports or formatters.

## Vision
- Provide a single configuration definition that applies across all supported languages (Python, C++, JavaScript/TypeScript, .NET, and more as demand grows).
- Support simultaneous logging to console, rotating disk files, and remote endpoints with minimal ceremony.
- Offer a familiar streaming syntax in C++ and idiomatic equivalents in other languages while sharing concepts like log levels, contextual metadata, and structured payloads.
- Ship sensible defaults and a pluggable architecture so teams can extend transports or formats without forking the core library.

## Target Capabilities
- Universal config file (likely TOML/YAML) describing sinks, formatting, and metadata.
- Language-specific SDKs that load the shared config and expose thin wrappers (`LOG(DEBUG) << "..."`, `log.debug("...")`, etc.).
- Output adapters for console, disk (plain and rotating files), and remote transports (HTTP, TCP, syslog, or message queues).
- Context propagation so per-request/user identifiers automatically enrich every log entry.
- Structured logging with optional JSON payloads and human-readable console formatting.
- Lightweight dependency footprint and zero-runtime configuration changes (hot reload optional later).

## Architecture Outline
- **Configuration Core**: Define schema, validation, environment overrides, and secrets handling. Distribute schema alongside SDKs.
- **Message Pipeline**: Normalize log events, apply formatting templates, attach context, and route to configured sinks.
- **Output Modules**: Console/STDOUT, file & rotation policy, remote transport abstraction; future modules (cloud logging providers, databases) plug in via unified interface.
- **Language Bindings**: Each language shares terminology and configuration but exposes idiomatic APIs and build artifacts.
- **Tooling**: CLI to bootstrap config, validate schema, and optionally tail logs across transports.

## Roadmap
### Phase 0 – Baseline & Planning (current)
- Audit existing Python prototype (`libLogit.py`) and capture gaps relative to the multi-language vision.
- Establish contribution guidelines, code formatting rules, and CI lint/test scaffolding.
- Document requirements and success metrics for v1.0.

### Phase 1 – Core Specification & Python MVP
- Finalize configuration schema, serialization format, and environment override strategy.
- Implement Python core library supporting config loading, level filtering, contextual metadata, and console/file sinks.
- Deliver canonical usage examples, including `LOG(DEBUG) << "..."`-style wrapper for Python (context manager or helper to mimic streaming).
- Add unit tests covering configuration parsing and sink behavior.

### Phase 2 – Advanced Outputs & Observability Enhancements
- Add rotating file sink, remote transport interface, and reference HTTP/TCP client implementations.
- Introduce structured logging (JSON payloads) and message formatting templates.
- Provide hooks for custom sinks and middleware (redaction, sampling).
- Expand test coverage with integration scenarios and benchmarking scripts.

### Phase 3 – Multi-Language SDK Rollout
- C++: Header-only facade mirroring `LOG(DEBUG) << "..."`, with bridge to shared configuration via generated bindings or lightweight runtime shim.
- JavaScript/TypeScript: Node package parsing shared config and exposing logger factory.
- .NET: NuGet package with dependency injection helpers.
- Align logging semantics (levels, contexts) across languages and document interoperability expectations.

### Phase 4 – Remote Logging & Deployment Tooling
- Harden remote transport (retry, buffering, offline queueing).
- Add optional agents/services for forwarding logs from restricted environments.
- Create CLI/GUI tooling to inspect config, validate connectivity, and tail remote streams.

### Phase 5 – Polishing, Documentation, and Release
- Author comprehensive documentation, migration guides, and quick-start templates per language.
- Package and publish language-specific artifacts (PyPI, vcpkg/Conan, npm, NuGet).
- Establish long-term maintenance plan, versioning strategy, and community support channels.

## Near-Term Tasks
- [ ] Document detailed requirements for the shared configuration schema.
- [ ] Refactor Python prototype into modular package structure (`liblogit/` with sinks, config, utils).
- [ ] Implement console and file sinks controlled by the new config system.
- [ ] Design `LOG(DEBUG) << "..."` API surface for Python, ensuring compatibility with async contexts.
- [ ] Draft contribution guide and decision record for language expansion priorities.

## Example Usage (Future State)
```toml
# logit.toml
level = "info"
outputs = ["console", "file", "remote"]

[outputs.console]
style = "pretty"

[outputs.file]
path = "logs/app.log"
rotation = "daily"

[outputs.remote]
protocol = "http"
endpoint = "https://logs.example.com/ingest"
api_key_env = "LOGIT_REMOTE_KEY"
```

```cpp
// main.cpp
#include "libLogit.h"

int main() {
    LogIt::initialize("logit.toml");
    LOG(DEBUG) << "Boot sequence started";
    LOG(INFO) << "Listening on port " << port;
    return 0;
}
```

```python
# main.py
from liblogit import LOG, init_from_config

init_from_config("logit.toml")
LOG("debug") << "Boot sequence started"
LOG("info") << {"event": "listening", "port": port}
```

## Current Status
The repository currently contains an initial Python-only prototype. The roadmap above outlines the steps required to evolve libLogit into the multi-language, multi-transport logging platform described in the project vision.
