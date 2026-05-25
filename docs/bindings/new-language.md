# New Binding Template

Use this template when adding a Beta-track language binding. The goal is not to
copy another language's punctuation. The goal is to make the same `LOGIT`
contract feel native in the new ecosystem.

## Intake Decision

Before implementation, record:

- Target language and package ecosystem.
- Primary user group: desktop app, service, tooling, game, embedded, or other.
- Expected package artifact and install command.
- Runtime dependencies and minimum supported runtime version.
- Whether the binding will be a native implementation, a wrapper over an
  existing binding, or a wrapper over a shared core.
- Known platform limits.

## Required Public Surface

Every new binding needs:

| Capability | Requirement |
|------------|-------------|
| Blank `LOGIT` | A user can create an empty logger with defaults. |
| Structure-fed `LOGIT` | A user can pass a map, object, struct, builder, or config object. |
| Mutable path | A user can set local and Alpha remote paths after construction. |
| Level threshold | Trace/debug/info/warn/error/fatal parse and compare like the spec. |
| Console sink | Logs can write to standard output/error where appropriate. |
| File sink | Logs can append to local paths and create parent directories. |
| Text output | Deterministic text output matches conformance fixtures. |
| JSON-lines output | One event renders as one valid JSON object per line. |
| Config registry | Named `LOGIT` objects load from config v2. |
| Compatibility note | v0.1 migration is implemented or explicitly documented as not applicable. |
| Package metadata | Artifact name, version, license, README, and repository links are present. |

## Repository Layout

Place the binding under `languages/<language>/` and include these pieces when
the ecosystem supports them:

- Source package or module.
- Local tests.
- Shared fixture runner.
- One direct example and one config-loaded example under `examples/<language>/`.
- Package metadata.
- Language-specific README with install, quick start, verifier command, and
  Alpha limitations.

## Verification

Add or update:

- A local test command for the binding.
- A shared conformance runner using `tests/conformance/fixtures/`.
- The binding row in `tests/conformance/README.md`.
- `scripts/verify-alpha.ps1` if the binding becomes Alpha-supported.
- `.github/workflows/alpha-matrix.yml` or a Beta workflow when hosted CI should
  run it.

## Documentation

Before calling the binding ready, document:

- Install or vendoring path.
- Direct `LOGIT` quick start.
- Config-loaded example.
- API table in `docs/api/README.md` or a binding-specific API page.
- Known gaps, especially remote transports, rotation, redaction, buffering,
  thread safety, and platform-specific path behavior.

## Exit Criteria

A new binding is Beta-ready when it can be installed by a normal user, pass the
shared fixtures, run in hosted CI, and explain its differences without changing
the shared `LOGIT` contract.
