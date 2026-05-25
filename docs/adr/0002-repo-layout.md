# ADR 0002: Canonical v1 Alpha Repository Layout

- Status: Accepted
- Deciders: libLogit maintainers
- Date: 2026-05-24

## Context

libLogit started as a compact prototype, but v1 Alpha now spans a Python
reference package, native C/C++ surfaces, managed/service bindings, shared
config schemas, conformance fixtures, examples, release notes, and a desktop
SQLite viewer. Contributors need a stable map of where work belongs before the
repository becomes the public GitHub landing page.

The layout must support two goals at the same time:

- A new user should quickly find install, config, examples, and API docs.
- A maintainer should know where to add a binding, fixture, spec, release note,
  or verification script without inventing a parallel structure.

## Decision

v1 Alpha accepts this canonical layout:

| Path | Purpose |
|------|---------|
| `.github/workflows/` | Hosted CI and release verification workflows. |
| `cmake/` | CMake package config helpers for native C/C++ install/export. |
| `docs/` | Roadmap, configuration docs, release notes, migration guidance, project governance, and research. |
| `docs/adr/` | Architecture decision records. |
| `docs/api/` | Public Alpha API reference across all supported bindings. |
| `docs/project/` | Issue labels, milestones, and contributor-facing project management docs. |
| `docs/releases/` | Draft and final release notes. |
| `examples/` | Runnable examples for config files and each Alpha language binding. |
| `include/liblogit/` | Public native C/C++ include wrappers for installed consumers. |
| `languages/` | Non-Python language bindings, each in a language-specific subdirectory. |
| `liblogit/` | Python reference package and SQLite viewer. |
| `schema/` | Versioned JSON Schemas for config compatibility and v2 registry configs. |
| `scripts/` | Local verification and release helper scripts. |
| `spec/` | Versioned shared contract for `LOGIT`, levels, streaming, compatibility, and log stores. |
| `tests/` | Python tests and shared conformance fixtures. |

The Python package may remain concentrated in `liblogit/__init__.py` for Alpha
while the public API is still moving. Splitting it into modules remains a
tracked Python cleanup task and should preserve public imports.

The root `libLogit.h` remains as a compatibility and vendoring header for C++
Alpha users. Public installed includes live under `include/liblogit/`.

## Alternatives Considered

A monorepo package layout with every language at the root was rejected because
it would make the public landing page noisy and package tooling harder to
reason about.

A shared native core with thin bindings remains attractive for Beta, but Alpha
keeps native-feeling bindings under `languages/` so the `LOGIT` contract can be
validated across ecosystems before consolidation.

## Consequences

The repository has one accepted place for each kind of contribution. Release
verification can rely on this layout, documentation can link stable paths, and
future language work can be added without reshaping the Alpha surface.

The tradeoff is that Alpha keeps several implementation styles side by side.
The conformance suite and API docs carry the burden of proving they still share
one product contract.
