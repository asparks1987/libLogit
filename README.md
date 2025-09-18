# libLogit

libLogit is a rich, multi-language logging helper library that delivers a unified JSON configuration and an expressive streaming syntax (`LOG(DEBUG) << "message"`). A single configuration file drives consistent behaviour across Python, C++, Java, Kotlin, and Ada bindings so every module logs with the same thresholds and outputs.

## Vision
- Provide a portable JSON configuration describing severity threshold, timestamping, and output targets that every language binding can load.
- Support simultaneous logging to console, a local file, and an optional network path without per-module reconfiguration.
- Offer a familiar streaming syntax in C++ and idiomatic equivalents in the other languages while sharing concepts such as level tagging.
- Ship sensible defaults with room for future extensibility (additional sinks, structured payloads, etc.).

## Target Capabilities
- Universal `logit.json` file with the following top-level keys: `level`, `timestamp`, `file_location`, `network_file_location`.
- Language-specific SDKs that parse the shared JSON, expose the `LOG(...) << ...` interface (or idiomatic equivalent), and honour level tagging + timestamp toggles.
- Pluggable writer backends (console, file, network) that can evolve to support rotation, buffering, and retry strategies.
- Consistent message formatting regardless of language binding.

## Architecture Outline
- **Configuration Core**: JSON schema validation, convenience loaders, and environment override hooks (future work).
- **Message Pipeline**: Normalize outgoing events, attach metadata (timestamps and level tags), and dispatch to enabled sinks.
- **Output Modules**: Console/STDOUT writer, append-only file writer, optional network writer (initially network path/file, future remote transport).
- **Language Bindings**: Python, C++, Java, Kotlin (JVM), and Ada stubs share the same vocabulary and config contract.
- **Tooling**: Sample configs, schema validators, and forthcoming CLI utilities.

## Roadmap Highlights
### Phase 1 – Core Specification & Python MVP (in progress)
- Finalize JSON configuration schema and helpers.
- Implement Python logging core driven by the shared config file.
- Establish reference implementations in C++, Java, Kotlin, and Ada that can load the config and emit basic logs.

### Future Phases
Refer to `docs/burndown.md` and `docs/issue-backlog.md` for the broader multi-phase roadmap, including advanced sinks, remote transport, and full multi-language releases.

## Configuration
```json
{
  "level": { "threshold": "info", "tag": true },
  "timestamp": true,
  "file_location": "logs/app.log",
  "network_file_location": "\\logserver\share\app.log"
}
```
- `level.threshold` (string): minimum severity to record (`trace`, `debug`, `info`, `warn`, `error`, `fatal`).
- `level.tag` (boolean, optional, default `true`): include the textual level label in the log output.
- `timestamp` (boolean): include ISO-8601 timestamp when true.
- `file_location` (string or null): append-only file path; omitted/null disables file logging.
- `network_file_location` (string or null): optional secondary path (e.g., network share). Future work will expand this to remote transports.
- Fallback behaviour: if the configured file or network destinations cannot be reached during initialisation, libLogit logs a warning and continues with console-only output.

## Python Quick Start
```python
from liblogit import init_from_config, LOG, ENDL

init_from_config("logit.json")
LOG("info") << "Boot sequence started" << ENDL
LOG("error") << "Subsystem failure" << ENDL
```

```cpp
#include "libLogit.h"

int main() {
    liblogit::Logger::configure_from_file("logit.json");
    LOG(INFO) << "Boot sequence started";
    LOG(ERROR) << "Subsystem failure";
    return 0;
}
```

```java
import dev.liblogit.LibLogIt;

public class Main {
    public static void main(String[] args) {
        LibLogIt.initFromConfig("logit.json");
        LibLogIt.LOG("info").append("Boot sequence started").commit();
        LibLogIt.LOG("error").append("Subsystem failure").commit();
    }
}
```

Further examples for Kotlin and Ada live under `languages/`.

## Current Status
Phase 1 scaffolding is underway. The shared JSON schema, Python implementation, and multi-language skeletons are being built out to honour the simplified configuration contract.

## Development
- Install dependencies: `pip install -e .` (use a virtual env).
- Run unit tests: `pytest`.
