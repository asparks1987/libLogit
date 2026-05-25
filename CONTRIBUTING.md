# Contributing to libLogit

Thanks for helping libLogit become a dependable cross-language logging SDK.
The project is in v1 Alpha, so contributions should protect the shared `LOGIT`
contract while keeping each binding idiomatic for its language.

## Project Shape

libLogit is an SDK. The SDK includes the library code, public API, language
bindings, schemas, examples, package metadata, verification tooling, and the
SQLite viewer.

The v1 Alpha language set is Python, C++, C, C#, Java, JavaScript, Go, and
Kotlin. Ada and additional languages are beta-track unless the burndown says
otherwise.

Important starting points:

- `README.md` is the public landing page.
- `docs/burndown.md` is the live roadmap, blocker log, and readiness snapshot.
- `spec/` defines the shared contract all bindings follow.
- `tests/conformance/` contains cross-language fixture expectations.
- `scripts/verify-alpha.ps1` is the release-candidate verification gate.

## Local Setup

Install the Python package and development tools:

```powershell
python -m pip install -e ".[dev]"
```

The full Alpha verifier also expects the language toolchains listed in
`docs/burndown.md`: Python, Node.js/npm, .NET, Go, Java, Maven, Kotlin, LLVM
clang/clang-format, CMake, Ninja, and nlohmann-json for C++ config loading.

On Windows, run PowerShell scripts with the execution-policy bypass used by the
docs:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\verify-alpha.ps1
```

## Verification

Run the full local Alpha matrix before proposing release-facing changes:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\verify-alpha.ps1
```

Run the static gate for faster feedback:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\verify-static.ps1
```

The static gate runs Python syntax checks, `ruff`, `mypy`, whitespace diff
checks, JSON Schema/config validation, and C/C++ `clang-format` checks.

Binding-specific commands are listed in `docs/api/README.md` and
`tests/conformance/README.md`.

## Coding Standards

- Preserve the shared `LOGIT` model: logger name, level threshold, sinks,
  local/remote paths, config loading, and deterministic output semantics.
- Prefer idiomatic APIs in each language over forcing identical punctuation.
- Keep Alpha behavior synchronous and explicit unless the burndown promotes a
  beta feature into Alpha.
- Keep generated output, package caches, build directories, and local logs out
  of source control.
- Update tests and docs with behavior changes. A runtime change without a
  verifier update should be treated as incomplete.
- Use `clang-format` for C/C++ files and the Python static gate for Python.

## Documentation

Public-facing documentation should help a new user answer three questions:

1. What is a `LOGIT`?
2. How do I add the SDK to my project?
3. How do I verify it works in my language?

Use "SDK" for the full product, "API" for calls users write, and "binding" for
language-specific SDK surfaces.

## Branches and Pull Requests

- Use short topic branches. In Codex sessions, prefer the `codex/` prefix.
- Keep changes scoped to one behavior, binding, or documentation slice.
- Reference the matching burndown item or issue label in the PR description.
- Include the verification command and result in the PR description.
- Do not mark release-candidate work complete until `scripts/verify-alpha.ps1`
  passes and any blockers are documented in `docs/burndown.md`.

## Issue and Milestone Taxonomy

Use `docs/project/issue-taxonomy.md` for labels, phases, and milestone rules.

## Release Notes

Draft release notes live in `docs/releases/`. When changing user-visible
behavior, update the draft Alpha notes or document why the change is internal.
