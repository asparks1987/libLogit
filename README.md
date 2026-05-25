# libLogit

libLogit is a cross-language logging SDK built around one simple idea:
declare a `LOGIT`, point it at the places your logs should go, and start
writing useful output immediately.

```text
LOGIT AppLog = []
AppLog.localPath = "logs/app.log"
AppLog.remotePath = "\\\\logserver\\share\\app.log"
AppLog.level = DEBUG

AppLog(INFO) << "service started"
```

The exact syntax changes by language, but the contract stays the same. A
`LOGIT` is a user-owned logger object with its own level threshold, console
sink, local file sink, remote file destination, formatter, metadata, and runtime
state. Use one global application logger, or create many named loggers for
modules, jobs, services, audit trails, and diagnostics.

libLogit is currently in alpha development. The public v1 Alpha is intended to
be a functional showcase of the full product direction: import the SDK, declare
a `LOGIT`, emit logs, keep a bounded rotating local log store, and inspect those
logs through SQL or a simple viewer. v1 Beta is where the project hardens that
idea into truly OS-, hardware-, and language-agnostic logging.

## Terminology

Use **SDK** when talking about the whole product. The SDK includes the library
code, the public API, language bindings, config/schema, examples, package
metadata, and eventually the log viewer.

| Term | Meaning |
|------|---------|
| SDK | The installable libLogit product for a language/ecosystem. |
| API | The calls a user writes, such as `LOGIT()`, `.localPath`, `.level`, and `.log()`. |
| Binding | The language-specific SDK surface for Python, C++, C, C#, Java, JavaScript, Go, Kotlin, and future languages. |
| Sink | An output destination: console, file, remote path, SQLite/database store, or future transport. |
| Rotating store | A bounded log history that keeps the newest records and evicts or rolls older records. |
| Viewer | A desktop UI for searching, filtering, and reading stored logs. |

## Why libLogit

Most logging libraries are strong inside one ecosystem and awkward everywhere
else. libLogit is designed for projects that span more than one runtime and
still want one portable logging model.

- Start with a blank logger and set properties during project startup.
- Feed a structure into the logger when you prefer declarative setup.
- Load named `LOGIT` objects from one shared JSON config file.
- Write to console and file at the same time.
- Treat a remote path or network share as a secondary destination in Alpha.
- Keep level names, thresholds, text output, and JSON-lines output consistent
  across language bindings.
- Store logs in a SQL-queryable local database as the alpha desktop showcase
  matures.
- Inspect stored logs through standard SQL tooling and a built-in viewer.
- Use idiomatic syntax in each language instead of forcing one awkward API
  everywhere.

## What You Get

| Capability | Alpha behavior |
|------------|----------------|
| Direct `LOGIT` objects | Create a logger in code and mutate startup properties. |
| Structure-fed setup | Build from dictionaries, options, structs, or JSON-like objects. |
| Named config registry | Load many loggers from one `logit.json`. |
| Level filtering | `trace`, `debug`, `info`, `warn`, `error`, and `fatal`. |
| Console sink | Enabled by default for immediate feedback. |
| Local file sink | Enable by setting `path` or `localPath`. |
| Remote path sink | Enable by setting `remotePath`; Alpha treats non-socket targets as file paths. |
| Text formatter | Human-readable log lines with optional timestamp and level tag. |
| JSON-lines formatter | Stable event objects for machines and pipelines. |
| SQLite log store | Alpha showcase target for queryable rotating logs. |
| Viewer UI | Alpha showcase target for reading stored logs in a desktop window. |
| Redaction hooks | Mask configured keys and text patterns before sinks receive events. |
| Buffered output | Optional Python Alpha batched mode with capacity, timer, manual flush, and close-time flush. |
| Failure policies | Python Alpha can warn, drop, raise, retry, or fall back when a sink fails. |
| Thread-safe object calls | Python Alpha serializes configure/log/flush/close and verifies concurrent file output. |
| Shared fixtures | Conformance tests keep bindings from drifting. |

Beta will focus on hardening the language-agnostic story: a repeatable binding
template, more languages, broader CI, packaging, OS/hardware validation,
cross-binding runtime parity, and richer remote transports.

## Install

During alpha, the most reliable path is to install or vendor from this source
tree.

```bash
git clone https://github.com/asparks1987/libLogit.git
cd libLogit
python -m pip install -e .
```

For local development and release verification:

```bash
python -m pip install -e ".[dev]"
```

Python package metadata is present now. Native and managed bindings live under
`languages/` while their package artifacts are prepared for the alpha release.

C and C++ can also be consumed through CMake during Alpha:

```bash
cmake -S . -B build/liblogit -DCMAKE_BUILD_TYPE=Release
cmake --build build/liblogit
cmake --install build/liblogit --prefix /path/to/liblogit-install
```

Consumer projects can then use `find_package(libLogit CONFIG REQUIRED)` and
link `libLogit::c` or `libLogit::cpp`. The C++ target requires
`nlohmann/json.hpp` for config loading. The C++ installed include and migration
path are documented in [`docs/bindings/cpp.md`](docs/bindings/cpp.md).

## Python In 30 Seconds

Create a blank `LOGIT`, set the path and threshold, then log.

```python
from liblogit import LOGIT, DEBUG, INFO, ENDL

LogIT = LOGIT()
LogIT.localPath = "logs/app.log"
LogIT.level = DEBUG

LogIT(INFO) << "service started" << ENDL
```

Or feed the structure up front:

```python
from liblogit import LOGIT, INFO

AuditLog = LOGIT({
    "name": "AuditLog",
    "localPath": "logs/audit.log",
    "remotePath": r"\\logserver\share\audit.log",
    "level": "info",
    "timestamp": False,
    "format": "json",
    "metadata": {"component": "auth"}
})

AuditLog.log(INFO, {"event": "user_signed_in", "user_id": 42})
```

With `format = "json"` and `timestamp = false`, the output is a JSON-lines
event:

```json
{"level":"info","logger":"AuditLog","message":"{\"event\": \"user_signed_in\", \"user_id\": 42}","metadata":{"component":"auth"}}
```

## One Config, Many Loggers

`logit.json` can define a registry of named `LOGIT` objects. Defaults are
applied first, then each logger can override only what it needs.

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

```python
from liblogit import load_logits, INFO, ENDL

logs = load_logits("logit.json")

logs["AppLog"](INFO) << "service started" << ENDL
logs["AuditLog"].log(INFO, {"event": "user_signed_in"})
```

Legacy v0.1 global configs are still supported and load as a single `default`
logger.

## Language Quick Starts

The goal is not identical punctuation. The goal is the same durable object
model everywhere.

### C++

```cpp
#include <liblogit/logit.hpp>

int main() {
    auto LogIT = liblogit::LOGIT{};
    LogIT.localPath = "logs/cpp-app.log";
    LogIT.level = liblogit::Level::DEBUG;

    LogIT(liblogit::Level::INFO) << "C++ app started";

    auto logs = liblogit::LOGIT::load_from_file("logit.json");
    logs.at("AppLog")(liblogit::Level::INFO) << "configured C++ log";
}
```

The current C++ header uses `nlohmann/json.hpp` for config loading. The local
tooling setup verifies it with the vcpkg package.

### C

```c
#include <liblogit/logit.h>

int main(void) {
    liblogit_logit LogIT = liblogit_logit_default();
    liblogit_logit_set_local_path(&LogIT, "logs/c-app.log");
    liblogit_logit_set_level(&LogIT, LIBLOGIT_DEBUG);

    liblogit_builder builder = liblogit_logit_at(&LogIT, LIBLOGIT_INFO);
    liblogit_builder_append(&builder, "C app started");
    liblogit_builder_commit(&builder);
    return 0;
}
```

### C#

```csharp
using LibLogit;

var LogIT = new Logit
{
    LocalPath = "logs/csharp-app.log",
    Level = Level.Debug
};

LogIT.At(Level.Info).Append("C# app started").Commit();
```

### Java

```java
import dev.liblogit.Logit;

public final class Main {
    public static void main(String[] args) {
        Logit logIT = new Logit();
        logIT.setLocalPath("logs/java-app.log");
        logIT.level = Logit.Level.DEBUG;

        logIT.at(Logit.Level.INFO).append("Java app started").commit();
    }
}
```

### JavaScript

```javascript
const { LOGIT, levels } = require("./languages/javascript/src/liblogit");

const LogIT = new LOGIT();
LogIT.localPath = "logs/javascript-app.log";
LogIT.level = levels.DEBUG;

LogIT.at(levels.INFO).append("JavaScript app started").commit();
```

### Go

```go
package main

import "github.com/asparks1987/liblogit/languages/go/liblogit"

func main() {
    LogIT := liblogit.New()
    LogIT.SetLocalPath("logs/go-app.log")
    LogIT.Level = liblogit.Debug

    _ = LogIT.At(liblogit.Info).Append("Go app started").Commit()
}
```

### Kotlin

```kotlin
import dev.liblogit.KotlinLogit

fun main() {
    val LogIT = KotlinLogit()
    LogIT.localPath = "logs/kotlin-app.log"
    LogIT.level = "debug"

    (LogIT.at("info") shl "Kotlin app started").commit()
}
```

## Output Examples

Text output with timestamps and level tags:

```text
2026-05-24T12:00:00 INFO service started
2026-05-24T12:00:01 WARNING retry scheduled
```

Deterministic text output with `timestamp = false`:

```text
DEBUG alpha one
INFO beta two
```

JSON-lines output:

```json
{"level":"info","logger":"AuditLog","message":"user signed in","metadata":{"component":"audit"}}
```

## Configuration Fields

| Field | Default | Meaning |
|-------|---------|---------|
| `name` | `default` | Stable logger identifier. |
| `path` | `null` | Primary local file destination. |
| `localPath` | `null` | Preferred mutable alias for `path`. |
| `pathMode` | `file` | Use `file` for exact file paths or `directory` to write `<LOGIT name>.log` inside the configured path. |
| `remotePath` | `null` | Secondary remote/network file destination. |
| `databasePath` | `null` | Local SQLite log store destination. |
| `level` | `info` | Minimum severity threshold. |
| `enabled` | `true` | Drops all events when false. |
| `sinks` | `["console"]` or inferred from paths | Enabled destinations: `console`, `file`, `network`, `database`. |
| `timestamp` | `true` | Include a timestamp in rendered output. |
| `tag_level` | `true` | Include the textual level in text output. |
| `format` | `text` | `text` or `json`. |
| `metadata` | `{}` | Static event metadata included in JSON output. |
| `buffering.mode` | `sync` | `sync` writes immediately; Python Alpha also supports `async`, `buffered`, and `batch` aliases. |
| `buffering.capacity` | `100` | Number of records held before automatic batch flush. |
| `buffering.flushIntervalSeconds` | `1.0` | Maximum seconds before buffered records are flushed. |
| `failurePolicy.mode` | `warn` | Per-event sink behavior: `warn`, `drop`, `raise`, `retry`, or `fallback`. |
| `failurePolicy.retryAttempts` | `1` for retry mode | Extra attempts after a sink failure. |
| `failurePolicy.fallbackPath` | `null` | File path used when `mode` is `fallback`. |
| `redaction.keys` | `[]` | Case-insensitive payload or metadata field names to mask. |
| `redaction.patterns` | `[]` | Regular expressions applied to message text and string metadata values. |
| `redaction.mask` | `[REDACTED]` | Replacement text used for redaction matches. |
| `retention.maxRecords` | `10000` for database sinks | Maximum recent database rows to keep. |
| `retention.maxAgeSeconds` | `null` | Maximum age for retained database rows. |
| `retention.maxBytes` | `null` | Alpha logical event payload byte budget for retained database rows. |

If `sinks` is omitted, libLogit starts with `console` and infers `file` or
`network` when `localPath`, `path`, or `remotePath` are set, and `database`
when `databasePath` is set. If `sinks` is provided, it is authoritative.

With `pathMode: "directory"`, the Alpha Python reference sink treats local and
non-socket remote paths as directories. `name: "AppLog"` plus
`localPath: "logs/app"` writes `logs/app/AppLog.log`.

## Environment Overrides

The Alpha Python SDK can redirect or quiet logging at deploy time without
editing JSON. Global overrides affect every loaded `LOGIT`; named overrides use
the uppercase logger name and win over the global value.

| Variable | Example named override | Purpose |
|----------|------------------------|---------|
| `LIBLOGIT_LEVEL` | `LIBLOGIT_APPLOG_LEVEL=debug` | Change the minimum severity. |
| `LIBLOGIT_ENABLED` | `LIBLOGIT_APPLOG_ENABLED=false` | Enable or disable a logger. |
| `LIBLOGIT_PATH` | `LIBLOGIT_APPLOG_PATH=logs/app.log` | Change the local file path. |
| `LIBLOGIT_REMOTE_PATH` | `LIBLOGIT_APPLOG_REMOTE_PATH=//share/app.log` | Change the remote path. |
| `LIBLOGIT_DATABASE_PATH` | `LIBLOGIT_APPLOG_DATABASE_PATH=logs/app.sqlite` | Change the SQLite store path. |
| `LIBLOGIT_PATH_MODE` | `LIBLOGIT_APPLOG_PATH_MODE=directory` | Treat paths as files or directories. |

Empty path override values clear that destination. Explicit `sinks` lists remain
authoritative, so a path override redirects an enabled sink but does not add a
sink the config intentionally omitted.

## Buffering And Flush

The default Python Alpha behavior is synchronous and immediate. Use `buffering`
when a busy desktop app wants to batch log writes without changing the rest of
the `LOGIT` contract:

```python
BufferedLog = LOGIT(
    name="BufferedLog",
    path="logs/buffered.log",
    buffering={
        "mode": "async",
        "capacity": 25,
        "flushIntervalSeconds": 1.0,
    },
)

BufferedLog.log("info", "held briefly in memory")
BufferedLog.flush()
```

Buffered mode flushes on batch capacity, on the timer, on manual `flush()`, and
on `close()`. Choose `sync` for crash-critical logs where every call should be
visible immediately.

## Thread Safety

Python Alpha serializes `LOGIT` configuration, logging, flushing, and closing
at the object level. Concurrent threads can log to the same `LOGIT` without
interleaving file lines or dropping buffered records.

## Failure Policies

The default Python Alpha policy is `warn`: if one sink fails while writing an
event, libLogit writes a concise warning to standard error and keeps the app
running. Use `failurePolicy` when you want a different tradeoff:

```python
ResilientLog = LOGIT(
    name="ResilientLog",
    path="logs/primary.log",
    failurePolicy={
        "mode": "fallback",
        "fallbackPath": "logs/fallback.log",
    },
)

ResilientLog.log("error", "still captured if the primary sink fails")
```

Supported modes are `warn`, `drop`, `raise`, `retry`, and `fallback`. Retry
mode uses `retryAttempts` and `retryDelaySeconds`.

## Redaction

Use `redaction` when logs may include tokens, passwords, API keys, or other
values that should never reach console, files, or the SQLite store:

```python
SecureLog = LOGIT(
    name="SecureLog",
    path="logs/secure.jsonl",
    format="json",
    redaction={
        "mask": "[MASKED]",
        "keys": ["password", "secret", "token"],
        "patterns": [r"(?i)api[_-]?key=[^\s&]+"],
    },
)

SecureLog.log("info", {"event": "login", "password": "not-stored"})
```

`redaction.keys` masks matching object fields recursively. `redaction.patterns`
masks matching text in messages and string metadata values. The Python Alpha
validates regexes when the logger is configured, which catches broken policies
before events are written.

## SQL-Readable Log Store

The alpha Python reference can write to a bounded SQLite store:

```python
from liblogit import LOGIT, INFO

LogIT = LOGIT({
    "name": "DatabaseLog",
    "databasePath": "logs/app-logit.sqlite",
    "level": "debug",
    "sinks": ["console", "database"],
    "retention": {"mode": "records", "maxRecords": 1000}
})

LogIT.log(INFO, "database-backed log event")
```

Query it with any SQLite-capable SQL interface:

```sql
SELECT created_at, logger, level, message
FROM logit_events
ORDER BY sequence DESC
LIMIT 25;
```

Or use the alpha viewer:

```bash
liblogit-viewer logs/app-logit.sqlite
liblogit-viewer logs/app-logit.sqlite --print --level error
liblogit-viewer logs/app-logit.sqlite --print --since 2026-05-24T14:30:00 --until 2026-05-24T15:30:00
python -m liblogit.viewer logs/app-logit.sqlite
python -m liblogit.viewer logs/app-logit.sqlite --print --level error
```

This is the first slice of the Alpha desktop showcase. The viewer opens a
desktop window with logger, level, search, and time-window filters, and it sits
on top of the same `logit_events` table that SQL tools can query.

## Alpha Language Status

| Binding | Status | Verification |
|---------|--------|--------------|
| Python | Reference package with desktop viewer console script | `pytest`, sdist/wheel build, wheel metadata check, and installed-wheel smoke test |
| C++ | Header-only alpha object model with CMake install/export | CMake, `ctest`, installed consumer check, and shared fixtures |
| C | Struct/function binding with CMake static library target | CMake, `ctest`, and installed consumer check |
| C# | Managed Alpha binding with NuGet metadata | `dotnet run`, shared fixtures, and `dotnet pack` |
| Java | JVM Alpha binding with Maven metadata | `javac`/`java`, shared fixtures, and `mvn package` |
| JavaScript | Node Alpha binding with npm metadata | `node` shared fixtures, `npm pack --dry-run`, and installed-tarball smoke test |
| Go | Go module Alpha binding | `go test ./...` and package docs |
| Kotlin | JVM facade with verified Maven companion package | `kotlinc`/`kotlin` shared fixtures and `mvn package` after Java artifact install |

Alpha means the object model and core behavior are actively usable, but package
distribution, CI coverage, and some binding-specific conformance adapters are
still being completed.

## Roadmap

### v1 Alpha

- Finish the shared `LOGIT` object contract.
- Keep Python as the reference implementation.
- Support Python, C++, C, C#, Java, JavaScript, Go, and Kotlin.
- Provide direct construction, structure-fed construction, and config-loaded
  named loggers.
- Support console, local file, remote-path-as-file, text output, JSON-lines
  output, level filtering, timestamps, and static metadata.
- Add a functional desktop showcase: bounded SQLite log storage, configurable
  retention/rotation length, SQL-readable records, and a minimal viewer UI.
- Ship examples, API docs, package metadata, and CI for the alpha matrix.

### v1 Beta

- Make binding creation repeatable for additional languages.
- Expand conformance tests and CI across every supported binding.
- Harden package distribution for each ecosystem.
- Validate the SDK across desktop operating systems, CPU architectures, and
  representative hardware profiles.
- Add or finalize advanced features: real remote transports, cross-binding
  redaction/buffering/failure-policy parity, and broader viewer integrations.

## Development

Run the current verification matrix from a local checkout:

```bash
powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\verify-alpha.ps1
```

That script runs Python tests, static/schema/native-format checks, JavaScript,
C#, Go, Java, Kotlin, C, C++, npm pack dry-run, NuGet pack, Java/Kotlin Maven
package checks, the native CMake build, CTest, install, and a consumer
`find_package(libLogit)` check.
GitHub Actions runs the same Alpha matrix on pull requests, including the
static gate, schema validation, package checks, and native install checks.

For the static gate alone:

```bash
powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\verify-static.ps1
```

The static gate runs Python syntax checks, `ruff`, `mypy`, whitespace checks,
schema/config validation, and C/C++ formatting checks.

Contribution workflow, coding standards, and release expectations are in
[`CONTRIBUTING.md`](CONTRIBUTING.md). Issue labels and milestones are defined
in [`docs/project/issue-taxonomy.md`](docs/project/issue-taxonomy.md). The
accepted repository layout is recorded in
[`docs/adr/0002-repo-layout.md`](docs/adr/0002-repo-layout.md), and the
binding implementation strategy is recorded in
[`docs/adr/0003-binding-strategy.md`](docs/adr/0003-binding-strategy.md).

The full project plan, blockers, and readiness percentages live in
[`docs/burndown.md`](docs/burndown.md). The public Alpha API surface is in
[`docs/api/README.md`](docs/api/README.md), migration guidance is in
[`docs/migration-v0.1-to-v0.2.md`](docs/migration-v0.1-to-v0.2.md), and the
shared API contract is documented in [`spec/logit-object.md`](spec/logit-object.md).
Sink behavior is captured in [`spec/sinks.md`](spec/sinks.md).
Future binding work starts from
[`docs/bindings/new-language.md`](docs/bindings/new-language.md), with the
current beta intake list in
[`docs/project/beta-language-intake.md`](docs/project/beta-language-intake.md).
The Alpha release-candidate checklist is in
[`docs/releases/v1.0.0-alpha.1-checklist.md`](docs/releases/v1.0.0-alpha.1-checklist.md).
