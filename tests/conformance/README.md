# libLogit Conformance Fixtures

These fixtures define behavior that every v1 Alpha MVP binding must satisfy.

Alpha MVP bindings:

- Python
- C++
- C
- C#
- Java
- JavaScript
- Go
- Kotlin

## Fixture Shape

Each fixture is JSON with:

- `name`: stable fixture id.
- `description`: human-readable goal.
- `config`: config v2 payload using relative file paths.
- `messages`: ordered log calls with logger, level, and fragments.
- `expected_files`: expected file contents after all messages are committed.

Bindings may adapt path separators internally, but rendered file contents must match exactly for fixtures with `timestamp` disabled.

## Required Alpha Fixture Behavior

- Load config v2.
- Apply defaults before object overrides.
- Drop events below threshold.
- Render text output as `LEVEL message` when `tag_level` is true and `timestamp` is false.
- Render JSON-lines as valid one-line JSON objects with canonical keys.
- Preserve message order within a sink.

## Binding Compliance Checklist

Each Alpha MVP binding must document support for the following items before it can be called Alpha-ready:

| Capability | Required | Skip Rule |
|------------|----------|-----------|
| Blank `LOGIT` declaration | Yes | None. |
| Structure-fed `LOGIT` declaration | Yes | None. |
| Mutable local path assignment | Yes | None. |
| Mutable remote path assignment | Yes | May treat remote paths as file paths in Alpha. |
| Level parsing and filtering | Yes | None. |
| Console sink | Yes | None. |
| File sink | Yes | None. |
| Text formatter | Yes | None. |
| JSON-lines formatter | Yes | None. |
| Config v2 named registry loading | Yes | None. |
| v0.1 legacy config migration | Yes | May be deferred only if the binding has no prior released users, but the gap must be documented. |
| Shared fixture runner | Yes | None. |
| Package metadata | Yes | May be marked draft until release candidate. |
| Rotation/redaction/async buffering/failure policy/thread safety | No | Python Alpha has size rotation, redaction, buffering, failure policies, and thread-safety coverage; cross-binding parity remains beta-track. |

## Current Binding Status

| Binding | Runtime Test | Fixture Coverage | Status |
|---------|--------------|------------------|--------|
| Python | `python -m pytest` | Shared fixtures via `tests/test_python_conformance.py` | Passing local Alpha matrix |
| JavaScript | `node languages/javascript/test/liblogit.test.js` | Shared fixtures plus local behavior tests | Passing local Alpha matrix |
| C# | `dotnet run --project languages/csharp/LibLogit.Tests/LibLogit.Tests.csproj` | Shared fixtures plus local behavior tests | Passing local Alpha matrix |
| C++ | `clang++ -std=c++20 -Wall -Wextra languages/cpp/test_logit.cpp` | Shared fixtures plus local direct/config/json behavior | Passing local Alpha matrix |
| C | `clang -std=c17 -Wall -Wextra languages/c/liblogit.c languages/c/test_logit.c` | Shared fixtures plus local direct behavior | Passing local Alpha matrix |
| Java | `javac` + `java dev.liblogit.LogitTest` | Shared fixtures plus local behavior tests | Passing local Alpha matrix |
| Go | `go test ./...` | Shared fixtures plus local behavior tests | Passing local Alpha matrix |
| Kotlin | `kotlinc` + `kotlin dev.liblogit.LibLogItKTestKt` | Shared fixtures plus local behavior test | Passing local Alpha matrix |
