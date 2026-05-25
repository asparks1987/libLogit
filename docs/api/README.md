# libLogit Alpha API Reference

This is the public API surface for the v1 Alpha SDK. The product is an SDK: it
includes the library code, language bindings, config/schema, examples, package
metadata, and the viewer. The API is the set of calls users write in their
projects.

## Shared Contract

Every Alpha binding exposes the same `LOGIT` concept:

| Field | Purpose |
|-------|---------|
| `name` | Stable logger name, usually the config registry key. |
| `localPath` / `path` | Local file output path. |
| `pathMode` / `path_mode` | `file` for exact paths; `directory` for `<LOGIT name>.log` inside a folder. |
| `remotePath` | Alpha remote path; non-socket paths are written like files. |
| `level` | Minimum severity: `trace`, `debug`, `info`, `warn`, `error`, `fatal`. |
| `timestamp` | Include timestamps in rendered text/JSON output when supported. |
| `tagLevel` / `tag_level` | Include level labels in text output. |
| `format` | `text` or `json`. |
| `metadata` | Static metadata for JSON output and database rows. |
| `buffering` | Optional batched mode with capacity, interval, manual flush, and close-time flush where implemented. |
| `failurePolicy` | Per-event sink behavior: warn, drop, raise, retry, or fallback where implemented. |
| `redaction` | Key and regex rules for masking values before sinks receive events. |
| `sinks` | `console`, `file`, `network`, and Python `database` in Alpha. |

Alpha event output is synchronous by default. Retry policy and real remote
transports are beta-track unless noted below.

## Python

Import from `liblogit`:

```python
from liblogit import INFO, LOGIT, LogitConfig, load_logits

LogIT = LOGIT({"name": "AppLog", "localPath": "logs/app.log", "level": "debug"})
LogIT(INFO) << "Python app started"

Configured = LOGIT(LogitConfig(path="logs/configured.log", level="debug"))
Configured.log("info", {"event": "configured"})

logs = load_logits("examples/config/v2-basic.json")
logs["AppLog"].log("info", "configured Python log")
```

Primary calls:

| API | Purpose |
|-----|---------|
| `LOGIT()` / `Logit()` | Blank logger with default settings. |
| `LOGIT({...})` | Structure-fed logger. |
| `LOGIT(LogitConfig(...))` | Logger created from the public Python config data model. |
| `LogitConfig.from_structure({...})` | Validate a v0.2 LOGIT object/defaults pair and return a reusable config object. |
| `Level.parse(value)` | Parse `trace`, `debug`, `info`, `warn`/`warning`, `error`, or `fatal` into an ordered level enum. |
| `LogEvent(...)` | Structured event data model used by Python formatters and persistence helpers. |
| `load_logits(path)` | Load a v0.2 registry into a `dict[str, Logit]`. |
| `get_logit(name="default")` | Read from the legacy global registry. |
| `init_from_config(path)` | Load a legacy v0.1 config into the global logger. |
| `LOG(level) << value << ENDL` | Legacy global streaming call. |
| `logit(level) << value` | Object-bound streaming call. Use `.with_metadata({...})` before commit to add per-message metadata. |
| `logit.log(level, message, metadata={...})` | Immediate object-bound call with optional per-message metadata. |
| `from liblogit.logit import Logit` | Stable object-oriented submodule import. |
| `copy_sample_config(path)` | Copy the bundled v0.2 sample config. |
| `liblogit-viewer database.sqlite` | Open the Alpha SQLite viewer. |

Python Alpha renders dict/list/tuple payloads as compact, stable JSON text in
text logs, JSON-lines messages, and SQLite message rows. Static `LOGIT`
metadata merges with per-message metadata; per-message keys win when both maps
contain the same key. Python Alpha also supports the SQLite `database` sink with
`databasePath`, `retention.maxRecords`, `retention.maxAgeSeconds`, and
`retention.maxBytes` logical payload budgets. File sinks support size-based
rotation through `rotation.maxBytes` and `rotation.maxFiles`. Redaction masks
configured `redaction.keys` recursively in payloads and metadata and applies
`redaction.patterns` to message text and string metadata values before console,
file, network-file, or SQLite sinks receive the event. Optional buffering uses
`buffering.mode = "async"` with `buffering.capacity` and
`buffering.flushIntervalSeconds`; buffered records flush on capacity, interval,
manual `flush()`, and `close()`. Python Alpha serializes object-level
configure/log/flush/close calls and covers concurrent file and buffered logging
with tests. `failurePolicy.mode` controls per-event sink failures with `warn`,
`drop`, `raise`, `retry`, and `fallback` modes. Loaded config can also be
overridden at runtime with `LIBLOGIT_*` variables such as
`LIBLOGIT_LEVEL`, `LIBLOGIT_PATH`, `LIBLOGIT_APPLOG_LEVEL`, and
`LIBLOGIT_APPLOG_PATH`; named overrides beat global overrides.

## C++

Include the installed header:

```cpp
#include <liblogit/logit.hpp>

auto LogIT = liblogit::LOGIT{};
LogIT.localPath = "logs/cpp-app.log";
LogIT.level = liblogit::Level::DEBUG;
LogIT(liblogit::Level::INFO) << "C++ app started";

auto configured = liblogit::LOGIT::load_from_file("examples/config/v2-basic.json");
configured.at("AppLog")(liblogit::Level::INFO) << "configured C++ log";
```

Primary calls:

| API | Purpose |
|-----|---------|
| `liblogit::LOGIT{}` | Blank logger with default settings. |
| `liblogit::LOGIT(nlohmann::json)` | Structure-fed logger. |
| `LOGIT::load_from_file(path)` | Load a v0.2 or legacy config registry. |
| `logit(Level::INFO) << "message"` | Streaming object-bound call. |
| `logit.log(Level::INFO, "message")` | Immediate call. |

The CMake package exports `libLogit::cpp`. Existing vendored users of the root
`libLogit.h` can migrate to installed consumers by including
`<liblogit/logit.hpp>` and linking the exported target; see
[`docs/bindings/cpp.md`](../bindings/cpp.md).

## C

Include the installed C header:

```c
#include <liblogit/logit.h>

liblogit_logit logit = liblogit_logit_default();
liblogit_logit_set_local_path(&logit, "logs/c-app.log");
liblogit_logit_set_level(&logit, LIBLOGIT_DEBUG);
liblogit_logit_log(&logit, LIBLOGIT_INFO, "C app started");
```

Primary calls:

| API | Purpose |
|-----|---------|
| `liblogit_logit_default()` | Blank logger with default settings. |
| `liblogit_logit_set_local_path(...)` | Configure local file output. |
| `liblogit_logit_set_remote_path(...)` | Configure Alpha remote path output. |
| `liblogit_logit_set_level(...)` | Set threshold. |
| `liblogit_logit_load_from_file(path, name, out)` | Load a named logger from config. |
| `liblogit_logit_at(...)` | Create a streaming builder. |
| `liblogit_builder_append(...)` | Append a fragment. |
| `liblogit_builder_commit(...)` | Emit the buffered message. |
| `liblogit_logit_log(...)` | Immediate call. |

The CMake package exports `libLogit::c`.

## C#

Use the `LibLogit` namespace:

```csharp
using LibLogit;

var LogIT = new Logit { LocalPath = "logs/csharp-app.log", Level = Level.Debug };
LogIT.At(Level.Info).Append("C# app started").Commit();

var configured = Logit.LoadLogits("examples/config/v2-basic.json");
configured["AppLog"].At("info").Append("configured C# log").Commit();
```

Primary calls:

| API | Purpose |
|-----|---------|
| `new Logit()` | Blank logger with default settings. |
| `new Logit(LogitOptions)` | Structure-fed logger. |
| `Logit.LoadLogits(path)` | Load a config registry. |
| `logit.At(Level.Info)` / `logit.At("info")` | Create a streaming builder. |
| `builder.Append(value).Commit()` | Emit the buffered message. |
| `logit.Log(level, message)` | Immediate call. |

Alpha package metadata targets `net10.0`.

## Java

Use `dev.liblogit.Logit`:

```java
Logit logIT = new Logit();
logIT.setLocalPath("logs/java-app.log");
logIT.level = Logit.Level.DEBUG;
logIT.at(Logit.Level.INFO).append("Java app started").commit();

Map<String, Logit> configured = Logit.loadLogits("examples/config/v2-basic.json");
configured.get("AppLog").at("info").append("configured Java log").commit();
```

Primary calls:

| API | Purpose |
|-----|---------|
| `new Logit()` | Blank logger with default settings. |
| `new Logit(Map<String, Object>)` | Structure-fed logger. |
| `Logit.loadLogits(path)` | Load a config registry. |
| `logit.setLocalPath(path)` | Configure local file output. |
| `logit.setRemotePath(path)` | Configure Alpha remote path output. |
| `logit.at(Level.INFO)` / `logit.at("info")` | Create a streaming builder. |
| `builder.append(value).commit()` | Emit the buffered message. |
| `logit.log(level, message)` | Immediate call. |

The companion `LibLogIt` class is a compatibility facade over `Logit`.

## JavaScript

Use the CommonJS package:

```javascript
const { LOGIT, loadLogits, levels } = require("@liblogit/liblogit");

const LogIT = new LOGIT();
LogIT.localPath = "logs/javascript-app.log";
LogIT.level = levels.DEBUG;
LogIT.at(levels.INFO).append("JavaScript app started").commit();

const configured = loadLogits("examples/config/v2-basic.json");
configured.AppLog.at("info").append("configured JavaScript log").commit();
```

Primary calls:

| API | Purpose |
|-----|---------|
| `new LOGIT()` / `new Logit()` | Blank logger with default settings. |
| `new LOGIT({...})` | Structure-fed logger. |
| `loadLogits(path)` | Load a config registry. |
| `logit.at(level)` | Create a streaming builder. |
| `builder.append(value).commit()` | Emit the buffered message. |
| `logit.log(level, message)` | Immediate call. |
| `levels.INFO` and peers | Level constants. |

The Alpha package is CommonJS and targets Node.js 18 or newer.

## Go

Use the `liblogit` package:

```go
logIT := liblogit.New()
logIT.SetLocalPath("logs/go-app.log")
logIT.Level = liblogit.Debug
_ = logIT.At(liblogit.Info).Append("Go app started").Commit()

configured, _ := liblogit.LoadLogits("examples/config/v2-basic.json")
_ = configured["AppLog"].At(liblogit.Info).Append("configured Go log").Commit()
```

Primary calls:

| API | Purpose |
|-----|---------|
| `liblogit.New()` | Blank logger with default settings. |
| `liblogit.FromOptions(options)` | Structure-fed logger. |
| `liblogit.LoadLogits(path)` | Load a config registry. |
| `logit.SetLocalPath(path)` | Configure local file output. |
| `logit.SetRemotePath(path)` | Configure Alpha remote path output. |
| `logit.At(level)` / `logit.AtString(level)` | Create a streaming builder. |
| `builder.Append(value).Commit()` | Emit the buffered message. |
| `logit.Log(level, message)` | Immediate call. |

## Kotlin

Use the Kotlin facade over the Java binding:

```kotlin
val LogIT = KotlinLogit()
LogIT.localPath = "logs/kotlin-app.log"
LogIT.level = "debug"
(LogIT.at("info") shl "Kotlin app started").commit()

val configured = LibLogItK.loadLogits("examples/config/v2-basic.json")
(configured.getValue("AppLog").at("info").append("configured Kotlin log")).commit()
```

Primary calls:

| API | Purpose |
|-----|---------|
| `KotlinLogit()` | Blank Kotlin wrapper. |
| `KotlinLogit(Logit())` | Wrap an existing Java `Logit`. |
| `LibLogItK.loadLogits(path)` | Load a config registry. |
| `logit.at("info")` | Create a Kotlin streaming builder. |
| `builder shl value` | Append a fragment. |
| `builder.commit()` | Emit the buffered message. |
| `logit.log(level, message)` | Immediate call through the wrapper. |

The Kotlin Maven artifact depends on the Java Alpha artifact.

## Verification Commands

Run the full local Alpha matrix:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\verify-alpha.ps1
```

Run only static/schema/native-format checks:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\verify-static.ps1
```

Install the Python development tools for that gate with
`python -m pip install -e ".[dev]"`.

Binding-specific commands are listed in
[tests/conformance/README.md](../../tests/conformance/README.md).
