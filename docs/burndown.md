# libLogit Burndown

This burndown takes libLogit from the current prototype into a cross-language logging SDK built around user-instantiated `LOGIT` objects.

## Product Goal

libLogit should let a developer add one SDK to their project, define one or more `LOGIT` objects by filling in a portable structure, and immediately start keeping useful logs. Each `LOGIT` owns the path, level threshold, sinks, formatting, metadata, buffering, database storage, retention/rotation length, and failure behavior needed to log for one application, module, service, or subsystem.

Terminology decision: call the full product an SDK. The SDK includes the library code, the public API, language bindings, config/schema, examples, package metadata, and the log viewer. Use "API" only for the calls users write, such as `LOGIT()`, `.localPath`, `.level`, and `.log()`. Use "binding" for a language-specific SDK surface.

The fully functional v1 alpha is intentionally smaller than the end-state vision, but it should be a working example of what the project can do: users should be able to include libLogit in their app, instantiate a `LOGIT`, emit logs, keep a bounded rotating local log store, and inspect those logs through SQL or a minimal viewer. Alpha does not need true support for every language, OS, desktop environment, and hardware profile. It needs to prove the `LOGIT` model and desktop logging workflow across the MVP language set: Python, C++, C, C#, Java, JavaScript, Go, and Kotlin.

The target mental model is:

```text
LOGIT LogIT = [
  PATH = "C:/logs/app.log",
  LEVEL = DEBUG
]

LogIT(DEBUG) << "This is a test log"
```

The exact syntax will vary by language, but the concept must not. A `LOGIT` means the same thing in Python, C++, C, C#, Java, JavaScript, Go, Kotlin, beta-track Ada, and any future binding.

Alpha database/viewer vocabulary:

- Rotating store: a bounded log history that keeps the newest records and evicts or rolls older records according to configured length, age, or size.
- SQLite log store: the alpha default local database because it is embedded, portable, queryable by standard SQL tools, and available on desktop systems without a server.
- Viewer: a minimal desktop window that can open the local log store, filter by logger/level/time/text, and show recent events.
- Remote path: an alpha-compatible file destination such as a network share. True remote transports are beta-track.

## Current State

- Python package exists in `liblogit/` with direct `LOGIT` objects, structure-fed initialization, a public `LogitConfig` data model, a stable `liblogit.logit` object submodule, explicit builder lifecycle coverage, a public `Level` enum, a public `LogEvent` model, deterministic structured payload rendering for dict/list/tuple messages, static plus per-message metadata merging, config v1/v2 loading, v0.1 migration through `load_logits()`, default registry retrieval, environment overrides for level/enabled/path/pathMode, level filtering, console output, file output with parent-directory creation, UTF-8 append behavior, and size-based rotation, `pathMode` file/directory path interpretation, network-path-as-file output, SQLite database output with record-count, age, and logical byte-budget retention, a SQLite viewer window with logger/level/search/time filters and `liblogit-viewer` console script, text/JSON formatting, Python redaction, buffering, per-event failure policies, object-level thread-safety, packaged schema/config data files, type marker, source distribution, wheel build, and fallback warnings.
- Legacy compatibility exists through `libLogit.py`.
- Config v1/v2 schemas exist under `schema/`, including the named `LOGIT` registry shape.
- Sample config exists at `liblogit/data/logit.sample.json`.
- Python tests exist in `tests/test_python_logging.py` and `tests/test_python_conformance.py`.
- C++ header exists in `libLogit.h` with a user-owned `LOGIT` object, config registry loading, object-bound streaming, the legacy `LOG(level)` macro, public `<liblogit/logit.hpp>` include wrapper, and CMake install/export support.
- C, C#, Java, JavaScript, Go, and Kotlin alpha bindings exist under `languages/` with local executable tests. C# has NuGet metadata, Java has Maven metadata, JavaScript has npm metadata, Go has module/package docs, and Kotlin has a verified companion Maven package.
- Documentation now covers the intended `LOGIT` object model, configuration, migration, public Alpha API surface, examples, contribution workflow, issue/milestone taxonomy, repo layout, binding strategy, future binding template, Ada beta-track disposition, beta language intake, packaging, draft alpha.1 release notes, and the alpha.1 promotion checklist. Hosted CI observation and final release-candidate promotion remain open.
- Product research exists at `docs/research/logging-feature-survey.md`, identifying structured events, sinks, thresholds, context, performance, formatting, and observability integration as the strongest market signals.
- Spec drafts now exist under `spec/` for the LOGIT object, levels, streaming API, compatibility, sink interface, and the alpha SQLite log store.
- Config v1/v2 schemas and config examples exist; the bundled package sample now uses the v2 registry shape while the legacy sample remains available for migration work.
- Python, JavaScript, C#, Java, C, C++, Go, and Kotlin have shared-fixture coverage.
- Local tooling for Python, Python lint/type checks, Node, .NET, Go, Java, Kotlin, Maven, C, C++, CMake, schema validation, and native formatting is installed. The full alpha language matrix, static verifier, Python wheel checks, npm dry-run, NuGet pack, Java Maven package, Kotlin Maven package, and native CMake install/consumer checks pass locally.

## Milestone Definitions

### v1 Alpha

The project reaches v1 alpha when:

- A formal `LOGIT` specification exists and is versioned.
- A user can include/import/install the library in each alpha language.
- A user can instantiate a `LOGIT` directly from a structure.
- A user can instantiate named `LOGIT` objects from a shared config file.
- Calling a `LOGIT` at a level emits logs according to that object's own configuration.
- Console and file logging work with deterministic text output and basic JSON-lines output.
- A bounded SQLite log store works as the alpha example of round-robin/rotating log retention.
- Stored logs can be read by standard SQL tooling.
- A minimal desktop viewer can open the alpha log store and filter recent events.
- The core event model, level semantics, and config behavior are consistent across the alpha languages.
- A conformance test suite proves that alpha bindings interpret the same config and emit equivalent output.
- The project ships documented alpha packages/artifacts for the alpha languages.

### v1 Beta

The project reaches v1 beta when:

- The alpha API has survived real usage and can be stabilized.
- The binding template is ready for a fast expansion into additional OOP languages.
- More languages can be added without redefining the `LOGIT` contract.
- The SDK is validated across desktop operating systems, CPU architectures, and representative hardware profiles.
- Optional runtime features such as Python redaction/buffering/failure policies and true remote transports are either implemented or clearly marked as post-beta work.
- CI can validate all supported languages through shared conformance fixtures.

## Latest Progress Snapshot

Updated 2026-05-25.

- v1 Alpha readiness: 99%. The `LOGIT` object contract, config schemas, config migration rules, Python reference package with `LogitConfig`, `Level`, `LogEvent`, `liblogit.logit`, deterministic structured payload rendering, static/per-message metadata merging, redaction hooks, async/batched buffering, failure policies, object-level thread-safety, builder lifecycle coverage, focused console/file sink tests, and Python file size rotation, all eight MVP language bindings, the public-facing README, contributor workflow, issue taxonomy, accepted repo layout, Kotlin shared fixtures, alpha packaging plan, Python SQLite rotating-store slice with record, age, and logical byte budgets, path rules, Python environment overrides, desktop viewer filters, C shared-fixture runner, local and hosted static/schema/native-format verification with Python lint/type checks, local alpha verification script, GitHub Actions alpha matrix, native C/C++ CMake install/export path, documented C++ root-header-to-installed migration path, hosted CI check script, NuGet metadata, npm metadata, Java Maven metadata/package, Go package docs, Kotlin Maven companion package, v2 bundled sample, migration guide, Alpha API docs, examples index, draft alpha.1 release notes, sink interface spec, and alpha.1 promotion checklist are locally verifiable. First hosted CI observation and final release-candidate promotion remain open.
- v1 Beta readiness: 51%. The language-agnostic contract, conformance direction, packaging plan, migration compatibility, first SQL-readable store, viewer direction, cross-language CI shape with static quality gates, Python package artifact, native package export path, documented C++ layout/migration path, cross-binding fixture coverage, desktop viewer shape, path-rule/environment-override vocabulary, richer SQLite retention policy, public Python config/level/event modules, early sink/formatter/logit module boundaries, explicit Python console/file sink behavior, static/per-message metadata merging, Python file size rotation, Python redaction semantics, Python buffering semantics, Python failure-policy semantics, Python thread-safety semantics, static verification command shape with Python lint/type gates, accepted repo governance, release documentation base, accepted binding implementation strategy, reusable future-binding template, Ada beta-track disposition, and prioritized beta language intake exist, but true OS/hardware/language agnostic logging, broader binding implementation, CI hardening, cross-binding runtime parity, and remaining advanced runtime features still need dedicated work.
- Burndown-weighted roadmap progress: 97.7% using `Done = 1`, `In progress = 0.5`, and `Not started = 0` across the tracked point table.

### Long-Term Vision

The long-term goal is support for every practical OOP language. That means native-feeling APIs for each language, not one awkward syntax forced everywhere. The shared promise is the `LOGIT` object model, not identical punctuation.

Candidate language families after beta include TypeScript, Swift, Objective-C, Rust with object-oriented API patterns, PHP, Ruby, Scala, Dart, Delphi/Object Pascal, and additional JVM/.NET languages.

## Target API Shape

Canonical pseudo-syntax:

```text
LOGIT AuditLog = [
  PATH = "C:/logs/audit.log",
  LEVEL = INFO,
  TIMESTAMP = true,
  FORMAT = "text",
  TAG_LEVEL = true,
  SINKS = [CONSOLE, FILE]
]

AuditLog(DEBUG) << "ignored below threshold"
AuditLog(INFO) << "user signed in"
```

Python target:

```python
from liblogit import Logit, DEBUG, INFO

AuditLog = Logit(path="C:/logs/audit.log", level=INFO)
AuditLog(INFO) << "user signed in"
```

C++ target:

```cpp
#include <liblogit/logit.hpp>

auto AuditLog = liblogit::LOGIT{
    .path = "C:/logs/audit.log",
    .level = liblogit::Level::INFO
};

AuditLog(liblogit::Level::INFO) << "user signed in";
```

JVM target:

```java
Logit auditLog = Logit.builder()
    .path("C:/logs/audit.log")
    .level(Level.INFO)
    .build();

auditLog.at(Level.INFO).append("user signed in").commit();
```

## Language Strategy

Alpha MVP targets eight languages. Python remains the reference implementation, C and C++ prove native embeddability and streaming syntax, C# and Java cover the largest managed OOP ecosystems, JavaScript and Go cover modern application/service stacks, and Kotlin validates an idiomatic JVM facade.

| Language | Alpha Role | Why It Belongs In Alpha |
|----------|------------|-------------------------|
| Python | Reference implementation | Existing package and tests already exist; fastest place to refine the model. |
| C++ | Canonical streaming syntax | Best match for `LogIT(DEBUG) << "message"` and useful for native consumers. |
| C | Native minimal API | Provides the lowest-level ABI surface and a practical bridge for other runtimes. |
| C# | .NET binding | Covers the primary OOP language in the .NET ecosystem and desktop/service users. |
| Java | Main JVM binding | Large OOP user base and a practical proving ground for builder-style APIs. |
| JavaScript | Web and Node binding | Gives immediate reach into frontend tooling, Node services, and package consumers. |
| Go | Service binding | Covers a major backend deployment language with an idiomatic object-style adapter. |
| Kotlin | JVM idiomatic layer | Shares much of Java's implementation effort while testing a more expressive syntax. |

Ada remains valuable as an exploratory binding because this repo already has Ada sketches, but it should not block v1 alpha. It moves to the beta expansion track unless there is a strong user need to keep it in the initial set.

The beta language expansion should use a repeatable playbook: choose a language, map the `LOGIT` object into that language's idioms, implement the conformance fixtures, publish a minimal package, then graduate it from experimental to supported.

## LOGIT Structure Contract

Every binding must support these core fields:

| Field | Required | Purpose |
|-------|----------|---------|
| `name` | No | Stable identifier when loaded from config. |
| `path` | No | Primary file destination. May be a file path or directory depending on `path_mode`. |
| `path_mode` | No | `file` or `directory`; controls whether libLogit appends to a file or creates named files inside a directory. |
| `level` | Yes | Minimum severity threshold: `trace`, `debug`, `info`, `warn`, `error`, `fatal`. |
| `enabled` | No | Fast toggle for disabling a `LOGIT` without code changes. |
| `sinks` | No | Destinations such as `console`, `file`, `network`, `memory`, or future custom sinks. |
| `timestamp` | No | Include or suppress timestamp metadata. |
| `timezone` | No | `local`, `utc`, or IANA timezone where supported. |
| `tag_level` | No | Include or suppress the level label in text output. |
| `format` | No | `text`, `json`, or named/custom template. |
| `template` | No | Text format template for advanced users. |
| `metadata` | No | Static key/value metadata such as app, module, component, tenant, or environment. |
| `redaction` | No | Rules for masking secrets, tokens, PII, or field patterns. |
| `rotation` | No | Max bytes, max files, daily/hourly policy, or disabled. |
| `buffering` | No | Sync/async mode, batch size, flush interval, and shutdown behavior. |
| `failure_policy` | No | What to do when a sink fails: warn, drop, raise, retry, or fallback. |
| `encoding` | No | File encoding, defaulting to UTF-8. |
| `newline` | No | Platform default, LF, or CRLF. |

## Config V2 Draft

The shared config should evolve from one global logger into a registry of named `LOGIT` objects:

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
      "path": "logs/audit.log",
      "level": "info",
      "format": "json",
      "metadata": {
        "component": "audit"
      }
    }
  }
}
```

Backward compatibility requirement:

- Existing v0.1 config files with `level`, `timestamp`, `file_location`, and `network_file_location` must still load.
- Loading v0.1 config should create one default `LOGIT` named `default`.
- Warnings should explain the migration path without breaking existing users.

## Architecture Target

The implementation should converge on these layers:

1. `spec/`
   - Versioned written contract for `LOGIT`, levels, event fields, sinks, formatters, error handling, and conformance expectations.
2. `schema/`
   - JSON Schema for config files and example payloads.
3. `liblogit/` reference implementation
   - Python implementation of the core model, parser, sinks, formatters, and user API.
4. `include/` or `cpp/`
   - C++ implementation with streaming syntax and direct `LOGIT` instantiation.
5. `languages/`
   - Language bindings/adapters that map native syntax to the shared contract.
6. `tests/conformance/`
   - Golden fixtures shared by all bindings.
7. `examples/`
   - Minimal real applications showing one-logit and multi-logit setup.

## Burndown Summary

Initial total scope: 210 story points after clarifying that Alpha must include a functional database-backed desktop showcase.

This total covers the road from the current repo through v1 alpha and into the first beta expansion pass. The v1 alpha cut line is the first working SDK milestone: include the SDK, instantiate a `LOGIT`, route logs through it in Python, C++, C, C#, Java, JavaScript, Go, and Kotlin, and demonstrate a bounded SQLite log store plus minimal viewer. Work beyond that cut line prepares the mad dash toward true OS, hardware, and language agnostic logging.

| Phase | Focus | Points | Exit Signal |
|-------|-------|--------|-------------|
| 0 | Repo baseline and project framing | 10 | Current state documented and tests reproducible. |
| 1 | `LOGIT` product specification | 20 | Versioned object contract approved. |
| 2 | Config v2 and migration | 22 | Named `LOGIT` config schema validates and v0.1 still works. |
| 3 | Python reference implementation | 30 | Python supports direct and config-loaded `LOGIT` objects. |
| 4 | Core sinks, formatting, and runtime behavior | 28 | Alpha has console/file/text/JSON behavior plus Python redaction, buffering, failure policies, and thread-safety coverage; beta track covers cross-binding runtime parity. |
| 4A | Desktop log store and viewer showcase | 20 | Alpha proves bounded SQL-readable logs and a minimal viewer window. |
| 5 | C++ implementation | 22 | C++ supports target streaming syntax. |
| 6 | Alpha language bindings and beta binding playbook | 24 | C, C#, Java, JavaScript, Go, and Kotlin consume the same contract; Ada and future languages have an expansion path. |
| 7 | Conformance and CI | 18 | Cross-language golden tests run in CI. |
| 8 | Documentation, packaging, and release | 16 | Release candidate is installable and documented. |

| Milestone | Included Work | Remaining Scope After Milestone |
|-----------|---------------|---------------------------------|
| v1 alpha | Phases 0-5, Phase 4A, C/C#/Java/JavaScript/Go/Kotlin alpha binding work, alpha conformance, alpha docs/packages. | True OS/hardware/language agnostic hardening, advanced runtime features, and broader packaging. |
| v1 beta | Binding playbook, additional language onboarding, expanded conformance, CI hardening, OS/hardware validation. | Long-tail OOP language support and ecosystem polish. |
| Long-term | Every practical OOP language has a native-feeling `LOGIT` API. | Ongoing maintenance and community-driven bindings. |

Target burndown:

```text
SP
190 |######################################
180 |####################################
160 |################################
138 |############################
108 |######################
 80 |################
 58 |############
 34 |#######
 16 |###
  0 +--------------------------------------> Phase
     S    0    1    2    3    4    5    6    7    8
```

| Point In Plan | Remaining SP |
|---------------|--------------|
| Start | 190 |
| Phase 0 complete | 180 |
| Phase 1 complete | 160 |
| Phase 2 complete | 138 |
| Phase 3 complete | 108 |
| Phase 4 complete | 80 |
| Phase 5 complete | 58 |
| Phase 6 complete | 34 |
| Phase 7 complete | 16 |
| Phase 8 complete | 0 |

## Phase 0 - Repo Baseline And Framing

| ID | Step | Deliverable | Acceptance Criteria | Points | Depends On | Status |
|----|------|-------------|---------------------|--------|------------|--------|
| P0-01 | Capture current implementation inventory. | Current-state inventory in this burndown. | Repo state is described accurately, including Python package, schema, C++ header, language sketches, and tests. | 2 | None | Done |
| P0-02 | Run current test suite and document baseline. | Test results in release notes or development notes. | `pytest` result is known; failures are triaged or captured. | 2 | None | Done |
| P0-03 | Decide canonical repo layout. | Layout ADR. | New directories for spec, conformance, examples, and bindings are accepted. | 2 | P0-01 | Done |
| P0-04 | Define contribution workflow. | `CONTRIBUTING.md`. | Local setup, test commands, style expectations, and release branching are documented. | 2 | P0-01 | Done |
| P0-05 | Add issue labels and milestone taxonomy. | Backlog labels. | Work can be tracked by phase, binding, spec, sink, test, and docs. | 2 | P0-01 | Done |

## Phase 1 - LOGIT Product Specification

| ID | Step | Deliverable | Acceptance Criteria | Points | Depends On | Status |
|----|------|-------------|---------------------|--------|------------|--------|
| S1-01 | Define `LOGIT` terminology. | `spec/logit-object.md`. | Terms such as `LOGIT`, event, sink, formatter, threshold, path, and registry have unambiguous meanings. | 3 | P0-01 | Done |
| S1-02 | Define severity semantics. | `spec/levels.md`. | Every binding maps `trace`, `debug`, `info`, `warn`, `error`, and `fatal` consistently. | 2 | S1-01 | Done |
| S1-03 | Define the minimum `LOGIT` structure. | Spec table and examples. | Required and optional fields are listed with defaults and validation rules. | 4 | S1-01 | Done |
| S1-04 | Define direct-instantiation behavior. | Spec examples for Python, C++, C, C#, Java, JavaScript, Go, Kotlin, and the future binding template. | Users can create a `LOGIT` without a config file. | 3 | S1-03 | Done |
| S1-05 | Define config-loaded registry behavior. | Registry section in spec. | Named `LOGIT` objects can be loaded and retrieved from config. | 3 | S1-03 | Done |
| S1-06 | Define streaming behavior. | `spec/streaming-api.md`. | Commit rules, destructor/finalizer behavior, explicit flush, empty messages, and exceptions are specified. | 2 | S1-04 | Done |
| S1-07 | Define compatibility policy. | `spec/compatibility.md`. | v0.1 global config migration and deprecation messaging are explicit. | 3 | S1-03 | Done |

## Phase 2 - Config V2 And Migration

| ID | Step | Deliverable | Acceptance Criteria | Points | Depends On | Status |
|----|------|-------------|---------------------|--------|------------|--------|
| C2-01 | Draft config v2 schema. | `schema/logit.v2.schema.json`. | Schema supports `version`, `defaults`, and `logits` map. | 4 | S1-03, S1-05 | Done |
| C2-02 | Preserve v0.1 schema. | `schema/logit.v1.schema.json`. | Existing config contract remains versioned and testable. | 2 | C2-01 | Done |
| C2-03 | Add schema examples. | `examples/config/*.json`. | Valid and invalid config fixtures cover simple, multi-logit, JSON format, rotation, and migration cases. | 3 | C2-01 | Done |
| C2-04 | Define path rules. | Schema and docs update. | File path, directory path, relative path, Windows path, POSIX path, and empty path behavior are defined. | 3 | S1-03 | Done |
| C2-05 | Add config migration rules. | Migration spec and tests. | v0.1 config loads as `default`; v2 config loads named objects. | 4 | C2-01, C2-02 | Done |
| C2-06 | Define environment override rules. | Spec and parser tests. | Overrides can change level, enabled state, and path without editing config. | 2 | C2-01 | Done |
| C2-07 | Update sample config. | `liblogit/data/logit.sample.json`. | Bundled sample shows config v2 while legacy sample remains available. | 2 | C2-05 | Done |
| C2-08 | Document configuration. | `docs/configuration.md`. | User-facing docs explain direct objects, config files, defaults, and migration. | 2 | C2-07 | Done |

## Phase 3 - Python Reference Implementation

| ID | Step | Deliverable | Acceptance Criteria | Points | Depends On | Status |
|----|------|-------------|---------------------|--------|------------|--------|
| PY-01 | Split Python package into modules. | `liblogit/config.py`, `levels.py`, `logit.py`, `events.py`, `sinks/`, `formatters/`. | Public imports still work and internals are no longer packed into `__init__.py`. | 4 | P0-03 | In progress |
| PY-02 | Implement `Level` enum. | `liblogit/levels.py`. | String parsing, aliases, ordering, and formatting are covered by tests. | 2 | S1-02 | Done |
| PY-03 | Implement `LogitConfig` data model. | `liblogit/config.py`. | Direct structure validation matches the spec and schema. | 3 | C2-01 | Done |
| PY-04 | Implement `LogEvent` data model. | `liblogit/events.py`. | Event includes level, message, timestamp, logger name, metadata, and rendered fields. | 3 | S1-03 | Done |
| PY-05 | Implement `Logit` object. | `liblogit/logit.py`. | `Logit(path=..., level=...)` can be called as `Logit(DEBUG) << "message"`. | 5 | PY-02, PY-03, PY-04 | Done |
| PY-06 | Implement builder/stream lifecycle. | `LogBuilder` tests. | Explicit commit, `ENDL`, destructor fallback, empty messages, and exceptions behave as specified. | 3 | S1-06, PY-05 | Done |
| PY-07 | Implement registry loader. | `load_logits(path)` and `get_logit(name)`. | Multi-logit config returns named `Logit` objects with defaults applied. | 4 | C2-05, PY-05 | Done |
| PY-08 | Maintain compatibility API. | Existing `init_from_config`, `LOG`, and `ENDL`. | Current tests pass and legacy users receive equivalent behavior. | 3 | PY-07 | Done |
| PY-09 | Add structured payload support. | JSON serialization tests. | Dict/list payloads render correctly in text and JSON formats. | 2 | PY-04, PY-06 | Done |
| PY-10 | Add type hints and public exports. | Typed public API. | `py.typed` is included and public objects are documented. | 1 | PY-05 | Done |

## Phase 4 - Sinks, Formatting, And Runtime Behavior

Alpha requirement: console sink, file sink, text formatter, JSON-lines formatter, level filtering, timestamp toggle, static metadata, and basic failure handling. Metadata merging, redaction, rotation, async buffering, richer failure policies, and thread-safety are important; Python now covers metadata merging, redaction, size rotation, async/batched buffering, warn/drop/raise/retry/fallback failure policies, and concurrent file/buffered output.

| ID | Step | Deliverable | Acceptance Criteria | Points | Depends On | Status |
|----|------|-------------|---------------------|--------|------------|--------|
| RT-01 | Define sink interface. | `liblogit/sinks/base.py` and spec update. | All sinks accept `LogEvent`, support close/flush where relevant, and report failures consistently. | 3 | PY-04 | Done |
| RT-02 | Implement console sink. | Console sink tests. | Console output honors level, timestamp, level tag, metadata, and format. | 2 | RT-01 | Done |
| RT-03 | Implement file sink. | File sink tests. | File paths are created safely and logs append with configured encoding/newline. | 3 | RT-01, C2-04 | Done |
| RT-04 | Implement directory path mode. | Path mode tests. | Directory mode creates stable filenames from `LOGIT` name or configured filename rule. | 2 | RT-03 | Done |
| RT-05 | Implement text formatter. | Golden format tests. | Text output is deterministic when timestamp is fixed. | 2 | PY-04 | Done |
| RT-06 | Implement alpha JSON-lines formatter. | JSON golden tests. | JSON logs are valid one-line JSON objects with stable field names. | 3 | PY-04 | Done |
| RT-07 | Implement metadata merging. | Metadata tests. | Static LOGIT metadata and per-message metadata combine with deterministic precedence. | 2 | PY-04 | Done |
| RT-08 | Implement redaction hooks. | Redaction tests. | Configured patterns and keys are masked before any sink receives an event. | 3 | PY-04 | Done |
| RT-09 | Implement rotation. | Rotation tests. | Max-size and max-files rotation work for file sink. | 3 | RT-03 | Done |
| RT-10 | Implement buffering and flush. | Buffering tests. | Sync mode writes immediately; async/batched mode flushes on interval, manual flush, and shutdown. | 3 | RT-01 | Done |
| RT-11 | Implement failure policies. | Failure policy tests. | `warn`, `drop`, `raise`, `retry`, and `fallback` behaviors are covered. | 3 | RT-01 | Done |
| RT-12 | Add thread-safety guarantees. | Concurrency tests. | Concurrent logging does not interleave or corrupt messages. | 2 | RT-03, RT-10 | Done |

## Phase 4A - Desktop Log Store And Viewer Showcase

Alpha requirement: prove the product direction with a SQL-readable bounded log store and a minimal desktop viewer. SQLite is the default alpha store because it is embedded, local-first, and readable from standard SQL tooling. True database server targets, cloud transports, and deep UI polish are beta-track.

| ID | Step | Deliverable | Acceptance Criteria | Points | Depends On | Status |
|----|------|-------------|---------------------|--------|------------|--------|
| DB-01 | Define SQLite log store schema. | `spec/log-store.md` and SQL schema. | Events have stable columns for id, timestamp, logger, level, message, metadata, sink/source, and sequence. | 2 | S1-03, PY-04 | Done |
| DB-02 | Define retention and rotation policy. | Config fields and tests for length, max bytes, max age, and eviction order. | Users can choose a bounded history policy and the oldest records are removed deterministically. | 3 | DB-01, C2-04 | Done |
| DB-03 | Implement Python SQLite sink. | Python `sqlite`/database sink tests. | A `LOGIT` can write to a local SQLite store while console/file sinks still work. | 5 | DB-01, DB-02, RT-01 | Done |
| DB-04 | Add SQL interface examples. | Example SQL queries and docs. | Users can inspect logs through standard SQLite/SQL tools without the viewer. | 2 | DB-03 | Done |
| UI-01 | Build minimal desktop log viewer. | Viewer prototype. | A user can open the alpha SQLite store in a window and see recent log rows. | 5 | DB-03 | Done |
| UI-02 | Add viewer filters. | Level/logger/time/text filters. | Viewer can filter by logger, level, time window, and message search. | 3 | UI-01 | Done |

## Phase 5 - C++ Implementation

| ID | Step | Deliverable | Acceptance Criteria | Points | Depends On | Status |
|----|------|-------------|---------------------|--------|------------|--------|
| CPP-01 | Move C++ code into a durable layout. | `include/liblogit/` and `cpp/` or documented header-only layout. | Build instructions are clear and current `libLogit.h` consumers have a migration path. | 3 | P0-03 | Done |
| CPP-02 | Implement C++ `Level`. | C++ level tests. | Parsing and ordering match the spec and Python behavior. | 2 | S1-02 | Done |
| CPP-03 | Implement C++ `LOGIT` object. | C++ API and examples. | User can instantiate `LOGIT` with path and level structure. | 5 | S1-04, CPP-02 | Done |
| CPP-04 | Implement C++ streaming call syntax. | C++ streaming tests. | `AuditLog(Level::INFO) << "message"` emits once and honors threshold. | 4 | CPP-03, S1-06 | Done |
| CPP-05 | Implement config v2 loader. | C++ config tests. | Named `LOGIT` objects load from the same fixtures as Python. | 3 | C2-03, CPP-03 | Done |
| CPP-06 | Implement C++ sinks and formatters. | Console/file/text/json parity tests. | C++ output matches golden fixtures where timestamps are fixed. | 3 | RT-01, CPP-03 | Done |
| CPP-07 | Add CMake build. | `CMakeLists.txt`. | Library, examples, and tests build on Windows, Linux, and macOS. | 2 | CPP-01 | Done |

## Phase 6 - Alpha Language Bindings And Beta Expansion

| ID | Step | Deliverable | Acceptance Criteria | Points | Depends On | Status |
|----|------|-------------|---------------------|--------|------------|--------|
| LANG-01 | Define binding compliance checklist. | `tests/conformance/README.md`. | Each binding has required features, optional features, and skipped-feature rules. | 3 | S1-01, C2-03 | Done |
| LANG-02 | Decide implementation strategy. | ADR for per-language implementation vs shared native core. | Tradeoffs for portability, packaging, and API ergonomics are captured. | 3 | LANG-01 | Done |
| LANG-03 | Add C binding. | C `LOGIT` API. | C can instantiate direct and config-loaded `LOGIT` objects through an idiomatic struct/function API. | 3 | LANG-01, C2-05 | Done |
| LANG-04 | Add C# binding. | C# `Logit` API. | C# can instantiate direct and config-loaded `LOGIT` objects. | 3 | LANG-01, C2-05 | Done |
| LANG-05 | Update Java binding. | Java `Logit` API. | Java can instantiate direct and config-loaded `LOGIT` objects. | 3 | LANG-01, C2-05 | Done |
| LANG-06 | Add JavaScript binding. | JavaScript `Logit` API. | JavaScript can instantiate direct and config-loaded `LOGIT` objects in Node-compatible environments. | 3 | LANG-01, C2-05 | Done |
| LANG-07 | Add Go binding. | Go `Logit` API. | Go can instantiate direct and config-loaded `LOGIT` objects with idiomatic structs and methods. | 3 | LANG-01, C2-05 | Done |
| LANG-08 | Update Kotlin facade. | Kotlin idiomatic API. | Kotlin supports direct construction and `shl` streaming behavior. | 2 | LANG-05 | Done |
| LANG-09 | Move Ada to beta-track binding. | Ada gap list and beta plan. | Existing Ada sketches are preserved, but Ada does not block v1 alpha. | 1 | LANG-01 | Done |
| LANG-10 | Define future binding template. | `docs/bindings/new-language.md`. | Adding TypeScript, Swift, PHP, Ruby, Scala, Dart, Rust-style APIs, or other OOP languages has a documented path. | 2 | LANG-02 | Done |
| LANG-11 | Add alpha binding examples. | `examples/c`, `examples/cpp`, `examples/csharp`, `examples/java`, `examples/javascript`, `examples/go`, `examples/kotlin`. | Each alpha binding example logs one direct `LOGIT` and one config-loaded `LOGIT`. | 3 | LANG-03, LANG-04, LANG-05, LANG-06, LANG-07, LANG-08 | Done |
| LANG-12 | Add package metadata plan. | Packaging ADR. | NuGet/npm/Maven/Gradle/Go/C/C++/Python distribution requirements are known before alpha release; Ada and future language packaging is beta-track. | 1 | LANG-02 | Done |
| LANG-13 | Build beta language intake list. | Prioritized beta language backlog. | Candidate OOP languages are ranked by user demand, implementation cost, ecosystem fit, and package distribution difficulty. | 1 | LANG-10 | Done |

## Phase 7 - Conformance, Quality, And CI

| ID | Step | Deliverable | Acceptance Criteria | Points | Depends On | Status |
|----|------|-------------|---------------------|--------|------------|--------|
| QA-01 | Create golden fixtures. | `tests/conformance/fixtures/`. | Fixtures include configs, messages, fixed clocks, and expected output. | 4 | C2-03, RT-05, RT-06 | In progress |
| QA-02 | Add Python conformance runner. | Python conformance tests. | Python passes all required fixtures. | 2 | PY-07, QA-01 | Done |
| QA-03 | Add C++ conformance runner. | C++ conformance tests. | C++ passes required fixtures. | 3 | CPP-06, QA-01 | Done |
| QA-04 | Add managed and service binding conformance runners. | C#, Java, JavaScript, Go, and Kotlin tests. | Managed/service bindings pass required fixtures. | 3 | LANG-04, LANG-05, LANG-06, LANG-07, LANG-08, QA-01 | Done |
| QA-05 | Add Ada conformance runner. | Ada tests or documented manual runner. | Ada passes required fixtures or has explicit temporary gaps. | 2 | LANG-09, QA-01 | Done |
| QA-06 | Add CI matrix. | GitHub Actions workflow. | CI runs Python tests, schema validation, C++ build/tests, and available binding tests. | 3 | QA-02, QA-03 | Done |
| QA-07 | Add static checks. | Lint/type/test commands. | Python type checks, formatting checks, schema validation, and C++ formatting are automated. | 1 | QA-06 | Done |

## Phase 8 - Documentation, Packaging, And Release

| ID | Step | Deliverable | Acceptance Criteria | Points | Depends On | Status |
|----|------|-------------|---------------------|--------|------------|--------|
| REL-01 | Rewrite README around `LOGIT`. | README quick start. | First screen shows direct `LOGIT` creation and logging call. | 2 | PY-05, CPP-03 | Done |
| REL-02 | Add migration guide. | `docs/migration-v0.1-to-v0.2.md`. | Existing users can move from global `LOG` to named `LOGIT` objects. | 2 | C2-05, PY-08 | Done |
| REL-03 | Add API docs. | `docs/api/`. | Public API is documented for every alpha MVP language. | 3 | PY-10, CPP-04, LANG-11 | Done |
| REL-04 | Add tutorial examples. | `examples/README.md`. | Alpha examples cover single logit, multiple logits, and text logs; beta examples cover JSON logs, rotation, and failure policy. | 2 | RT-11, LANG-11 | Done |
| REL-05 | Prepare Python package release. | PyPI-ready package. | Metadata, files, type hints, samples, and tests are packaged correctly. | 2 | QA-02, REL-01 | Done |
| REL-06 | Prepare C++ distribution. | CMake install/export or header-only release artifact. | C++ consumers can install or vendor the library cleanly. | 2 | CPP-07, QA-03 | Done |
| REL-07 | Prepare alpha binding distribution plan. | NuGet/npm/Maven/Gradle/Go module checklist. | C#, JavaScript, Java, Kotlin, and Go artifacts have naming, versioning, and publication steps. | 1 | LANG-12, QA-04 | Done |
| REL-08 | Cut alpha release candidate. | v1.0.0-alpha.1. | Release notes list features, limitations, migration notes, and known beta-track gaps. | 2 | REL-05, REL-06, QA-06 | In progress |

## Cross-Cutting Decisions To Make Early

- Language agnostic starts as a shared specification with idiomatic native bindings; shared core or generated bindings are beta decisions. See ADR 0003.
- The v1 alpha MVP language set is Python, C++, C, C#, Java, JavaScript, Go, and Kotlin.
- Whether config files remain JSON-only or the structure can later support TOML/YAML frontends that compile to the same `LOGIT` model.
- Whether `PATH=C:/logs` means a file path, directory path, or either depending on `path_mode`.
- Whether a `LOGIT` should be immutable after construction or allow runtime reconfiguration.
- Whether fatal logs should only emit or also trigger process-level behavior such as flush-and-exit hooks.
- How much automatic behavior is acceptable in destructors/finalizers across languages.
- How remote logging should evolve beyond network file paths.

## Blockers And Constraints

| Date | Area | Blocker | Action Taken | Status |
|------|------|---------|--------------|--------|
| 2026-05-23 | Go binding verification | `go` is not available on PATH in the current local environment. | Go was installed on 2026-05-24 and `go test ./...` now passes. | Resolved |
| 2026-05-23 | C/C++ binding verification | `gcc` and MSVC `cl` are not available on PATH in the current local environment. | LLVM/clang and MSVC Build Tools were installed on 2026-05-24; C smoke compile and C++ header syntax check now pass. | Resolved |
| 2026-05-23 | C# release target | Local .NET install has SDK/runtime 10.0 but not .NET 8 runtime. | Alpha NuGet package now targets `net10.0`; beta should revisit multi-targeting after the API is steadier. | Documented |
| 2026-05-23 | Kotlin binding verification | `kotlinc` is not available on PATH in the current local environment. | Kotlin compiler was installed on 2026-05-24 and Kotlin facade compile/test now passes. | Resolved |
| 2026-05-23 | JVM packaging verification | `mvn` is not available on PATH in the current local environment. | Maven was installed on 2026-05-24 and reports version successfully. | Resolved |
| 2026-05-24 | Tooling | Go, LLVM/clang, CMake, Ninja, MSVC Build Tools, Maven, Kotlin compiler, vcpkg, nlohmann-json, Node, Python, pytest, and Python `jsonschema` were installed. | Local verification can now use installed tools with refreshed PATH; older missing-tool blockers above can be closed on the next burndown cleanup. | Resolved |
| 2026-05-24 | C++ source | `clang++ -fsyntax-only` found a malformed character literal in `libLogit.h` around line 269. | Fixed newline write in `Logger::write_to_file`; syntax check passes. | Resolved |
| 2026-05-24 | Kotlin source | `kotlinc` found the existing Kotlin facade referenced legacy `LibLogIt` and used an invalid `operator fun shl` declaration. | Reworked Kotlin facade around the new Java `Logit` object and an infix `shl`; compile/test passes. | Resolved |
| 2026-05-24 | Python file sink cleanup | A quick README sample run on Windows showed a direct `LOGIT` file sink can keep its file handle open long enough to block temporary directory cleanup. | Added close-after-emit file handler behavior plus handler cleanup tolerance; Python regression tests pass. | Resolved |
| 2026-05-24 | Windows verification script | Direct `.\scripts\verify-alpha.ps1` execution is blocked by the local PowerShell execution policy. | Verified with `powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\verify-alpha.ps1`; CI runs in GitHub Actions and is unaffected. | Documented |
| 2026-05-24 | CMake package export | The installed C++ CMake target referenced `nlohmann_json::nlohmann_json` before the package config loaded that dependency. | Added `find_dependency(nlohmann_json CONFIG)` to the generated package config and added an installed-consumer verification step. | Resolved |
| 2026-05-24 | Sandboxed local verification | The workspace sandbox blocked .NET from reading user NuGet config, Go from using its build cache, and CMake/Ninja from executing the user-installed Ninja path. | Re-ran the full Alpha verifier outside the sandbox after approval; the matrix passed. GitHub hosted CI is not expected to share this local sandbox restriction. | Resolved |
| 2026-05-24 | Java package build | Maven package build surfaced a broken legacy `LibLogIt.java` sketch that the direct Java tests did not compile. | Replaced the legacy class with a compatibility facade over the Alpha `Logit` object; `javac`, Java tests, and `mvn package` pass. | Resolved |
| 2026-05-24 | npm package verification | Windows PowerShell blocked `npm.ps1`, and the sandbox blocked npm cache writes. | Local verifier now calls `npm.cmd` on Windows with a temporary cache, and final verification ran outside the sandbox. | Resolved |
| 2026-05-24 | Kotlin Maven package build | Maven package initially failed because the companion POM did not include Kotlin stdlib. | Added the `kotlin-stdlib` dependency, set JVM target 17, install the Java artifact before Kotlin package verification, and confirmed `mvn package` passes. | Resolved |
| 2026-05-24 | Generated pytest cleanup | A focused sandboxed pytest run created `build/pytest-cache` and `build/pytest-tmp` owned by the sandbox identity; this user context cannot delete those directories even after approval. | Added `.gitignore` entries for generated build/cache/log output and kept the verifier on removable temp directories outside the workspace. | Documented |
| 2026-05-24 | Python type checking | `mypy`, `pyright`, and `ruff` were not available in the local toolchain. | Installed `mypy` and `ruff`; `scripts/verify-static.ps1` now runs Python syntax, lint, type, whitespace, schema/config, and C/C++ formatting checks. | Resolved |
| 2026-05-24 | Hosted CI observation | GitHub CLI is installed but not authenticated in this environment, so hosted Actions status cannot be observed locally. | Added `scripts/check-hosted-ci.ps1` and `docs/releases/v1.0.0-alpha.1-checklist.md`; after push and `gh auth login`, run the script and record the hosted workflow URL before final RC promotion. | Documented |

## Verification Log

| Date | Command | Result |
|------|---------|--------|
| 2026-05-23 | `python -m pytest` using bundled Python | 14 passed |
| 2026-05-23 | `node languages/javascript/test/liblogit.test.js` using bundled Node | Passed with shared fixtures |
| 2026-05-23 | `dotnet run --project languages/csharp/LibLogit.Tests/LibLogit.Tests.csproj` | Passed with shared fixtures |
| 2026-05-23 | `javac` + `java dev.liblogit.LogitTest` | Passed with shared fixtures |
| 2026-05-23 | Go scaffold verification | Blocked: `go` not available on PATH |
| 2026-05-24 | `python -m pytest` using installed Python | 14 passed |
| 2026-05-24 | `node languages/javascript/test/liblogit.test.js` using installed Node | Passed with shared fixtures |
| 2026-05-24 | `dotnet run --project languages/csharp/LibLogit.Tests/LibLogit.Tests.csproj` | Passed with shared fixtures |
| 2026-05-24 | `go test ./...` in `languages/go` | Passed |
| 2026-05-24 | `javac` + `java dev.liblogit.LogitTest` using installed Java | Passed with shared fixtures |
| 2026-05-24 | `clang` C smoke compile | Passed |
| 2026-05-24 | `cl` through `vcvars64.bat` | Available |
| 2026-05-24 | `kotlinc -version`, `mvn --version`, `cmake --version`, `ninja --version` | Available |
| 2026-05-24 | `clang++ -std=c++20 -fsyntax-only` against `libLogit.h` | Passed |
| 2026-05-24 | `kotlinc` + `kotlin dev.liblogit.LibLogItKTestKt` | Passed |
| 2026-05-24 | `clang -std=c17 -Wall -Wextra languages/c/liblogit.c languages/c/test_logit.c` | Passed |
| 2026-05-24 | `clang++ -std=c++20 -Wall -Wextra languages/cpp/test_logit.cpp` | Passed direct, config-loaded, JSON, and shared fixture behavior |
| 2026-05-24 | C and C++ basic examples compiled and ran with `clang`/`clang++` | Passed |
| 2026-05-24 | Full local alpha matrix: Python, C++, C, C#, Java, JavaScript, Go, Kotlin | Passed |
| 2026-05-24 | `python -m pytest tests/test_python_logging.py tests/test_python_conformance.py` | 15 passed |
| 2026-05-24 | `kotlinc` + `kotlin dev.liblogit.LibLogItKTestKt` | Passed with shared fixtures |
| 2026-05-24 | Kotlin basic example compiled and ran | Passed |
| 2026-05-24 | Full local alpha matrix after Python lifecycle and Kotlin fixture updates | Passed |
| 2026-05-24 | `python -m pytest tests/test_python_logging.py tests/test_python_conformance.py` after SQLite sink work | 17 passed |
| 2026-05-24 | `python examples/python/database.py` | Passed; wrote direct and config-loaded SQLite events |
| 2026-05-24 | `python -m pytest tests/test_python_logging.py tests/test_python_conformance.py` after viewer query module | 18 passed |
| 2026-05-24 | `python -m liblogit.viewer logs/app-logit.sqlite --print --level info --limit 2` | Passed |
| 2026-05-24 | Full local alpha matrix after SQLite store and viewer query module | Passed |
| 2026-05-24 | `clang -std=c17 -Wall -Wextra languages/c/liblogit.c languages/c/test_logit.c` after C config loader | Passed direct/config/shared-scenario behavior |
| 2026-05-24 | C basic example with direct and config-loaded logging | Passed |
| 2026-05-24 | Full local alpha matrix after C config loader | Passed |
| 2026-05-24 | `powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\verify-alpha.ps1` | Passed Python, JavaScript, C#, Go, Java, Kotlin, C, and C++ checks |
| 2026-05-24 | Schema validation using Python `jsonschema` against alpha config examples | Passed; valid v1/v2 examples accepted and invalid extra-key example rejected |
| 2026-05-24 | `git diff --check -- .github/workflows/alpha-matrix.yml scripts/verify-alpha.ps1 docs/burndown.md` | Passed with CRLF warning only |
| 2026-05-24 | CMake native package path: configure, build, `ctest`, install, and compile installed examples | Passed |
| 2026-05-24 | Installed CMake consumer using `find_package(libLogit CONFIG REQUIRED)` with `libLogit::c` and `libLogit::cpp` | Passed |
| 2026-05-24 | `powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\verify-alpha.ps1` after native package work | Passed Python, JavaScript, C#, Go, Java, Kotlin, C, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-24 | `git diff --check` over native package, CI, verifier, README, ADR, and burndown changes | Passed with CRLF warnings only |
| 2026-05-24 | `pytest tests/test_python_logging.py tests/test_python_conformance.py tests/test_python_packaging.py` with workspace temp/cache | 19 passed |
| 2026-05-24 | `setup.py sdist bdist_wheel` for Python package | Passed; generated sdist and wheel with expected setuptools deprecation warning for direct `setup.py` invocation |
| 2026-05-24 | Python wheel metadata inspection | Passed; wheel contains `py.typed`, viewer module, sample config, schema/config data files, and `liblogit-viewer` entry point |
| 2026-05-24 | Installed-wheel smoke test using `pip install --no-index --find-links dist --target build/package-smoke liblogit==0.1.0` | Passed |
| 2026-05-24 | Full Alpha verifier after Python packaging work | Passed outside sandbox: Python, JavaScript, C#, Go, Java, Kotlin, C, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-24 | `dotnet pack languages/csharp/LibLogit/LibLogit.csproj --configuration Release` | Passed; produced `LibLogit.0.1.0-alpha.0.nupkg` |
| 2026-05-24 | `npm.cmd pack --dry-run` in `languages/javascript` | Passed; package includes README, package metadata, and `src/liblogit.js` |
| 2026-05-24 | `mvn -B -DskipTests package` in `languages/java` | Passed; produced `liblogit-0.1.0-alpha.0.jar` |
| 2026-05-24 | Full Alpha verifier after managed/package metadata work | Passed outside sandbox: Python, JavaScript, npm pack, C#, NuGet pack, Go, Java, Maven package, Kotlin, C, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-24 | `mvn -B -DskipTests install` in `languages/java` | Passed; installed the Java Alpha artifact for Kotlin package resolution |
| 2026-05-24 | `mvn -B -DskipTests package` in `languages/kotlin` | Passed; produced `liblogit-kotlin-0.1.0-alpha.0.jar` |
| 2026-05-24 | Full Alpha verifier after Kotlin Maven package work | Passed outside sandbox: Python, JavaScript, npm pack, C#, NuGet pack, Go, Java install, Kotlin Maven package, C, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-24 | `clang -std=c17 -Wall -Wextra languages/c/liblogit.c languages/c/test_logit.c` after C shared fixture runner | Passed; C now enumerates and runs the JSON conformance fixtures |
| 2026-05-24 | Full Alpha verifier after C shared fixture runner | Passed outside sandbox: Python, JavaScript, npm pack, C#, NuGet pack, Go, Java install, Kotlin Maven package, C shared fixtures, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-24 | `python -m pytest tests/test_python_logging.py tests/test_python_conformance.py tests/test_python_packaging.py` after viewer time filters | 19 passed |
| 2026-05-24 | Full Alpha verifier after viewer filter polish | Passed outside sandbox: Python, JavaScript, npm pack, C#, NuGet pack, Go, Java install, Kotlin Maven package, C shared fixtures, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-24 | Changed Alpha examples: JavaScript, C#, Java, and Go direct plus config-loaded examples | Passed locally; Go example verified from `languages/go` module context |
| 2026-05-24 | `python -m pytest tests/test_python_logging.py tests/test_python_conformance.py tests/test_python_packaging.py` after v2 bundled sample update | 19 passed |
| 2026-05-24 | Full Alpha verifier after release docs, v2 sample, and example updates | Passed outside sandbox: Python, JavaScript, npm pack, C#, NuGet pack, Go, Java install, Kotlin Maven package, C shared fixtures, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-24 | Bundled `liblogit/data/logit.sample.json` validated against `schema/logit.v2.schema.json` | Passed with installed Python `jsonschema` outside sandbox |
| 2026-05-24 | `python -m pytest tests/test_python_logging.py tests/test_python_conformance.py tests/test_python_packaging.py` after `pathMode` work | 21 passed |
| 2026-05-24 | Config schema validation after `v2-directory-path.json` and `pathMode` schema additions | Passed v1 legacy, all v2 valid examples, bundled v2 sample, and invalid-extra-key rejection |
| 2026-05-24 | Full Alpha verifier after `pathMode` behavior and schema/docs updates | Passed outside sandbox: Python, JavaScript, npm pack, C#, NuGet pack, Go, Java install, Kotlin Maven package, C shared fixtures, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-24 | `setup.py sdist bdist_wheel` after package data updates | Passed; sdist and wheel include `logit.v1.sample.json` and `v2-directory-path.json` |
| 2026-05-24 | `python -m pytest tests/test_python_logging.py tests/test_python_conformance.py tests/test_python_packaging.py` after environment override support | 25 passed |
| 2026-05-24 | Full Alpha verifier after Python environment override support | Passed outside sandbox: Python, JavaScript, npm pack, C#, NuGet pack, Go, Java Maven install, Kotlin Maven package, C shared fixtures, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-24 | `python -m pytest tests/test_python_logging.py tests/test_python_conformance.py tests/test_python_packaging.py` after SQLite age/logical-byte retention | 27 passed |
| 2026-05-24 | Config schema validation after retention schema expansion | Passed all v2 config examples and bundled v2 sample with installed Python `jsonschema` |
| 2026-05-24 | Full Alpha verifier after SQLite age/logical-byte retention | Passed outside sandbox: Python, JavaScript, npm pack, C#, NuGet pack, Go, Java Maven install, Kotlin Maven package, C shared fixtures, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-24 | `powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\verify-static.ps1` | Passed Python syntax, whitespace diff, parser-backed schema/config validation, and C/C++ clang-format checks inside sandbox |
| 2026-05-24 | Full Alpha verifier after static/schema/native-format gate | Passed outside sandbox: Python, static checks with JSON Schema validation, JavaScript, npm pack, C#, NuGet pack, Go, Java Maven install, Kotlin Maven package, C shared fixtures, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-24 | `powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\verify-static.ps1` after Python lint/type tooling | Passed outside sandbox: Python syntax, `ruff`, `mypy --explicit-package-bases`, whitespace diff, JSON Schema validation, and C/C++ clang-format checks |
| 2026-05-24 | Full Alpha verifier after adding Python lint/type tooling to static gate | Passed outside sandbox: Python, `ruff`, `mypy`, JSON Schema validation, C/C++ clang-format, JavaScript, npm pack, C#, NuGet pack, Go, Java Maven install, Kotlin Maven package, C shared fixtures, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-24 | `python -m pytest tests/test_python_logging.py tests/test_python_conformance.py tests/test_python_packaging.py` after explicit v0.1 `load_logits()` migration coverage | 28 passed |
| 2026-05-24 | `powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\verify-static.ps1` after migration spec/test update | Passed `ruff`, `mypy`, JSON Schema validation, whitespace diff, and C/C++ clang-format checks |
| 2026-05-24 | Full Alpha verifier after marking config migration and Python registry/compatibility gates done | Passed outside sandbox: Python 28 tests, `ruff`, `mypy`, JSON Schema validation, C/C++ clang-format, JavaScript, npm pack, C#, NuGet pack, Go, Java Maven install, Kotlin Maven package, C shared fixtures, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-24 | `powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\verify-static.ps1` after contribution/layout/taxonomy docs | Passed `ruff`, `mypy`, JSON Schema validation, whitespace diff, and C/C++ clang-format checks |
| 2026-05-24 | Full Alpha verifier after contribution workflow, layout ADR, and issue taxonomy docs | Passed outside sandbox: Python 28 tests, `ruff`, `mypy`, JSON Schema validation, C/C++ clang-format, JavaScript, npm pack, C#, NuGet pack, Go, Java Maven install, Kotlin Maven package, C shared fixtures, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-24 | GitHub Actions static-gate parity update | Added hosted `static` job for Python syntax, `ruff`, `mypy`, whitespace, JSON Schema/config validation, and C/C++ clang-format checks |
| 2026-05-24 | Full Alpha verifier after hosted static-gate parity update | Passed outside sandbox: Python 28 tests, `ruff`, `mypy`, JSON Schema validation, C/C++ clang-format, JavaScript, npm pack, C#, NuGet pack, Go, Java Maven install, Kotlin Maven package, C shared fixtures, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-24 | Hosted CI release-candidate check helper | Added `scripts/check-hosted-ci.ps1` and alpha.1 promotion checklist; GitHub CLI auth is required before the hosted run can be observed |
| 2026-05-24 | `powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\verify-static.ps1` after hosted CI helper/checklist work | Passed outside sandbox: Python syntax, `ruff`, `mypy --explicit-package-bases`, whitespace diff, JSON Schema validation, and C/C++ clang-format checks |
| 2026-05-24 | `powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\check-hosted-ci.ps1` | Blocked as documented: GitHub CLI is installed but not authenticated in this environment |
| 2026-05-24 | Full Alpha verifier after release-candidate checklist and hosted CI helper work | Passed outside sandbox: Python 28 tests, `ruff`, `mypy`, JSON Schema validation, C/C++ clang-format, JavaScript, npm pack, C#, NuGet pack, Go, Java Maven install, Kotlin Maven package, C shared fixtures, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-24 | Binding strategy and beta intake documentation | Added ADR 0003, the reusable new-binding template, Ada beta-track gap plan, beta language intake ranking, and refreshed conformance status table |
| 2026-05-24 | `powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\verify-static.ps1` after binding strategy and beta intake docs | Passed outside sandbox: Python syntax, `ruff`, `mypy --explicit-package-bases`, whitespace diff, JSON Schema validation, and C/C++ clang-format checks |
| 2026-05-24 | Python `Level`, `LogEvent`, structured payload, and formatter module slice | Added `liblogit/levels.py`, `liblogit/events.py`, `liblogit/formatters.py`, and `liblogit/sinks/base.py`; public imports still work and dict/list/tuple payloads render deterministically |
| 2026-05-24 | `python -m pytest tests/test_python_logging.py tests/test_python_conformance.py tests/test_python_packaging.py` after Python event/level/payload work | 31 passed |
| 2026-05-24 | `powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\verify-static.ps1` after Python event/level/payload work | Passed outside sandbox: Python syntax, `ruff`, `mypy` across 12 source files, whitespace diff, JSON Schema validation, and C/C++ clang-format checks |
| 2026-05-24 | Full Alpha verifier after Python event/level/payload work | Passed outside sandbox: Python 31 tests, static checks with `ruff`/`mypy`, JSON Schema validation, C/C++ clang-format, JavaScript, npm pack, C#, NuGet pack, Go, Java Maven install, Kotlin Maven package, C shared fixtures, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-24 | Python `LogitConfig`, `liblogit.logit`, builder lifecycle, and sink spec slice | Added public `liblogit/config.py`, stable object submodule `liblogit/logit.py`, explicit builder lifecycle tests, and `spec/sinks.md` |
| 2026-05-24 | `python -m pytest tests/test_python_logging.py tests/test_python_conformance.py tests/test_python_packaging.py` after `LogitConfig` and builder lifecycle work | 36 passed |
| 2026-05-24 | `powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\verify-static.ps1` after `LogitConfig` and builder lifecycle work | Passed outside sandbox: Python syntax, `ruff`, `mypy` across 14 source files, whitespace diff, JSON Schema validation, and C/C++ clang-format checks |
| 2026-05-24 | Full Alpha verifier after `LogitConfig` and builder lifecycle work | Passed outside sandbox: Python 36 tests, static checks with `ruff`/`mypy`, JSON Schema validation, C/C++ clang-format, JavaScript, npm pack, C#, NuGet pack, Go, Java Maven install, Kotlin Maven package, C shared fixtures, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-24 | Final static verifier after `LogitConfig`, sink spec, API docs, and burndown updates | Passed outside sandbox: Python syntax, `ruff`, `mypy` across 14 source files, whitespace diff, JSON Schema validation, and C/C++ clang-format checks |
| 2026-05-24 | Python console/file sink behavior and text-level labels | Added focused tests for console threshold/timestamp/level labels, console JSON metadata, file parent directory creation, UTF-8 append behavior, and `FATAL` text output |
| 2026-05-24 | `python -m pytest tests/test_python_logging.py tests/test_python_conformance.py tests/test_python_packaging.py` after console/file sink tests | 39 passed |
| 2026-05-24 | Full Alpha verifier after console/file sink tests | Passed outside sandbox: Python 39 tests, static checks with `ruff`/`mypy`, JSON Schema validation, C/C++ clang-format, JavaScript, npm pack, C#, NuGet pack, Go, Java Maven install, Kotlin Maven package, C shared fixtures, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-24 | Final static verifier after level spec and console/file sink burndown updates | Passed outside sandbox: Python syntax, `ruff`, `mypy` across 14 source files, whitespace diff, JSON Schema validation, and C/C++ clang-format checks |
| 2026-05-24 | Python per-message metadata merging | Added `log(..., metadata={...})`, streaming `.with_metadata({...})`, deterministic static/event metadata precedence, JSON-lines coverage, SQLite `metadata_json` coverage, and invalid metadata validation |
| 2026-05-24 | `python -m pytest tests/test_python_logging.py tests/test_python_conformance.py tests/test_python_packaging.py` after metadata merging | 41 passed |
| 2026-05-24 | Full Alpha verifier after metadata merging | Passed outside sandbox: Python 41 tests, static checks with `ruff`/`mypy`, JSON Schema validation, C/C++ clang-format, JavaScript, npm pack, C#, NuGet pack, Go, Java Maven install, Kotlin Maven package, C shared fixtures, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-24 | Final static verifier after metadata docs and burndown updates | Passed outside sandbox: Python syntax, `ruff`, `mypy` across 14 source files, whitespace diff, JSON Schema validation, and C/C++ clang-format checks |
| 2026-05-24 | Python size-based file rotation | Added `rotation.maxBytes` and `rotation.maxFiles` to Python config/runtime, schema, docs, and examples; file sinks rotate to numbered backups before a write exceeds the active-file byte limit |
| 2026-05-24 | `python -m pytest tests/test_python_logging.py tests/test_python_conformance.py tests/test_python_packaging.py` after file rotation | 43 passed |
| 2026-05-24 | Full Alpha verifier after file rotation | Passed outside sandbox: Python 43 tests, static checks with `ruff`/`mypy`, JSON Schema validation including `v2-rotation.json`, C/C++ clang-format, JavaScript, npm pack, C#, NuGet pack, Go, Java Maven install, Kotlin Maven package, C shared fixtures, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-24 | Final static verifier after file-rotation burndown update | Passed outside sandbox: Python syntax, `ruff`, `mypy` across 14 source files, whitespace diff, JSON Schema validation, and C/C++ clang-format checks |
| 2026-05-24 | Python redaction hooks | Added `redaction.keys`, `redaction.patterns`, and `redaction.mask` to Python config/runtime, schema, bundled sample, examples, docs, and sink spec; messages, structured payload fields, static metadata, and per-message metadata are masked before console, file, network-file, or SQLite sinks receive events |
| 2026-05-24 | `python -m pytest tests/test_python_logging.py tests/test_python_conformance.py tests/test_python_packaging.py` after redaction | 46 passed |
| 2026-05-24 | Static verifier after redaction | Passed outside sandbox: Python syntax, `ruff`, `mypy` across 14 source files, whitespace diff, JSON Schema validation including `v2-redaction.json`, and C/C++ clang-format checks |
| 2026-05-24 | Full Alpha verifier after redaction | Passed outside sandbox: Python 46 tests, static checks with `ruff`/`mypy`, JSON Schema validation including `v2-redaction.json`, C/C++ clang-format, JavaScript, npm pack, C#, NuGet pack, Go, Java Maven install, Kotlin Maven package, C shared fixtures, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-24 | Final static verifier after redaction burndown update | Passed outside sandbox: Python syntax, `ruff`, `mypy` across 14 source files, whitespace diff, JSON Schema validation, and C/C++ clang-format checks |
| 2026-05-24 | Python async/batched buffering | Added `buffering.mode`, `buffering.capacity`, and `buffering.flushIntervalSeconds` to Python config/runtime, schema, examples, docs, and sink spec; sync mode writes immediately, async mode flushes on capacity, timer interval, manual `flush()`, and `close()` |
| 2026-05-24 | `python -m pytest tests/test_python_logging.py tests/test_python_conformance.py tests/test_python_packaging.py` after buffering | 49 passed |
| 2026-05-24 | Static verifier after buffering | Passed outside sandbox: Python syntax, `ruff`, `mypy` across 14 source files, whitespace diff, JSON Schema validation including `v2-buffering.json`, and C/C++ clang-format checks |
| 2026-05-24 | Full Alpha verifier after buffering | Passed outside sandbox: Python 49 tests, static checks with `ruff`/`mypy`, JSON Schema validation including `v2-buffering.json`, C/C++ clang-format, JavaScript, npm pack, C#, NuGet pack, Go, Java Maven install, Kotlin Maven package, C shared fixtures, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-24 | Final static verifier after buffering burndown update | Passed outside sandbox: Python syntax, `ruff`, `mypy` across 14 source files, whitespace diff, JSON Schema validation, and C/C++ clang-format checks |
| 2026-05-25 | Python failure policies | Added `failurePolicy.mode`, `retryAttempts`, `retryDelaySeconds`, and `fallbackPath` to Python config/runtime, schema, bundled sample, examples, docs, and sink spec; per-event sink failures now support warn, drop, raise, retry, and fallback file output |
| 2026-05-25 | `python -m pytest tests/test_python_logging.py tests/test_python_conformance.py tests/test_python_packaging.py` after failure policies | 51 passed |
| 2026-05-25 | Static verifier after failure policies | Passed outside sandbox: Python syntax, `ruff`, `mypy` across 14 source files, whitespace diff, JSON Schema validation including `v2-failure-policy.json`, and C/C++ clang-format checks |
| 2026-05-25 | Full Alpha verifier after failure policies | Passed outside sandbox: Python 51 tests, static checks with `ruff`/`mypy`, JSON Schema validation including `v2-failure-policy.json`, C/C++ clang-format, JavaScript, npm pack, C#, NuGet pack, Go, Java Maven install, Kotlin Maven package, C shared fixtures, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-25 | Final static verifier after failure-policy burndown update | Passed outside sandbox: Python syntax, `ruff`, `mypy` across 14 source files, whitespace diff, JSON Schema validation, and C/C++ clang-format checks |
| 2026-05-25 | Python thread-safety guarantees | Added object-level locking around Python `LOGIT` configure/log/flush/close and concurrent file plus buffered logging tests that assert complete, non-interleaved event output |
| 2026-05-25 | `python -m pytest tests/test_python_logging.py tests/test_python_conformance.py tests/test_python_packaging.py` after thread-safety work | 53 passed |
| 2026-05-25 | Static verifier after thread-safety work | Passed outside sandbox: Python syntax, `ruff`, `mypy` across 14 source files, whitespace diff, JSON Schema validation, and C/C++ clang-format checks |
| 2026-05-25 | Full Alpha verifier after thread-safety work | Passed outside sandbox: Python 53 tests, static checks with `ruff`/`mypy`, JSON Schema validation, C/C++ clang-format, JavaScript, npm pack, C#, NuGet pack, Go, Java Maven install, Kotlin Maven package, C shared fixtures, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-25 | Final static verifier after thread-safety burndown update | Passed outside sandbox: Python syntax, `ruff`, `mypy` across 14 source files, whitespace diff, JSON Schema validation, and C/C++ clang-format checks |
| 2026-05-25 | C++ durable layout documentation | Added `docs/bindings/cpp.md` covering the root `libLogit.h` vendoring path, installed `<liblogit/logit.hpp>` include, `libLogit::cpp` CMake target, dependency notes, and migration path for existing C++ users |
| 2026-05-25 | Static verifier after C++ durable layout documentation | Passed outside sandbox: Python syntax, `ruff`, `mypy` across 14 source files, whitespace diff, JSON Schema validation, and C/C++ clang-format checks |
| 2026-05-25 | Full Alpha verifier after C++ durable layout documentation | Passed outside sandbox: Python 53 tests, static checks with `ruff`/`mypy`, JSON Schema validation, C/C++ clang-format, JavaScript, npm pack, C#, NuGet pack, Go, Java Maven install, Kotlin Maven package, C shared fixtures, C++, CMake, CTest, install, and installed-consumer checks |
| 2026-05-25 | Final static verifier after C++ layout burndown update | Passed outside sandbox: Python syntax, `ruff`, `mypy` across 14 source files, whitespace diff, JSON Schema validation, and C/C++ clang-format checks |

## Risk Register

| Risk | Impact | Mitigation |
|------|--------|------------|
| Operator streaming syntax cannot be identical in every language. | API may feel inconsistent. | Define canonical semantics and idiomatic equivalents per binding. |
| Config schema grows too fast. | Harder to implement all bindings. | Mark advanced fields optional and gate them through conformance levels. |
| Per-language implementations drift. | Users see different behavior. | Golden fixtures and conformance runners are required before release. |
| Destructor/finalizer commits behave differently. | Messages may emit unpredictably. | Prefer explicit commit/flush and document best-effort finalizer behavior. |
| File and path rules differ by OS. | Cross-platform bugs. | Add Windows/POSIX path fixtures and normalize only where the spec says to. |
| Async logging adds complexity. | Race conditions or lost logs. | Ship sync first; add async behind explicit config and shutdown tests. |

## Suggested First Sprint

Start with the path that unlocks the whole project:

1. Write `spec/logit-object.md`.
2. Draft `schema/logit.v2.schema.json`.
3. Refactor Python internals into modules.
4. Implement Python `Logit(path=..., level=...)`.
5. Preserve `init_from_config` and `LOG` as compatibility shims.
6. Add tests proving direct `LOGIT` creation and v0.1 config migration.

Exit criteria for the first sprint:

- `Logit(path="logs/app.log", level="debug")("debug") << "message"` works in Python.
- Existing tests still pass.
- A v2 config with two named `LOGIT` objects loads.
- README or docs show the new object-centered direction.
