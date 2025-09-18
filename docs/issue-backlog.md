# Prioritized Issue Backlog

This backlog translates the near-term checklist into actionable issues with clear priorities, ownership placeholders, and success criteria.

## Legend
- **P0** – Critical path for the v1 foundation; must be completed before other work.
- **P1** – Important enhancements that unblock richer functionality.
- **P2** – Supporting tasks that institutionalize practices and prepare for multi-language scale.

## Issues
### P0 — Define Shared Configuration Contract
- **Summary**: Formalize the shared configuration schema covering sinks, formatting, metadata, and environment overrides.
- **Outcome**: Validated schema file (`schema/logit.schema.json` or equivalent) plus docs describing each field.
- **Checklist**:
  - Draft schema structure (levels, outputs, rotation, remote settings).
  - Add validation utilities and unit tests in Python prototype.
  - Document schema usage in `docs/configuration.md`.
- **Owner**: _TBD_
- **Dependencies**: None.

### P0 — Restructure Python Prototype Into Package
- **Summary**: Split `libLogit.py` into a package (`liblogit/`) with modules for config, sinks, and helpers.
- **Outcome**: Installable package with setup metadata aligned to new structure.
- **Checklist**:
  - Create package scaffolding (`liblogit/config.py`, `liblogit/sinks/__init__.py`, ...).
  - Update imports, packaging metadata, and entry points.
  - Add minimal automated tests to verify package import and logging bootstrap.
- **Owner**: _TBD_
- **Dependencies**: Define Shared Configuration Contract.

### P0 — Implement Config-Driven Console & File Sinks
- **Summary**: Wire the new configuration loader to console and file sinks with level filtering.
- **Outcome**: Demonstrable example showing simultaneous console/file output driven by shared config.
- **Checklist**:
  - Implement sink abstractions and default implementations.
  - Ensure file sink supports rotation configuration placeholders (even if stubbed).
  - Provide sample config + docs verifying behavior.
- **Owner**: _TBD_
- **Dependencies**: Define Shared Configuration Contract, Restructure Python Prototype.

### P1 — Design Streaming API Surface for Python
- **Summary**: Provide ergonomic `LOG(DEBUG) << "..."` style logging that maps to Python idioms.
- **Outcome**: `LOG(level)` callable returning an object that supports `<<` or context-managed patterns.
- **Checklist**:
  - Investigate operator overloading / context manager approaches.
  - Prototype performance implications and thread-safety.
  - Document usage patterns and edge cases (async, structured payloads).
- **Owner**: _TBD_
- **Dependencies**: Implement Config-Driven Console & File Sinks.

### P1 — Remote Transport Interface Spike
- **Summary**: Draft interface for remote transports (HTTP/TCP/syslog) and provide stub implementation.
- **Outcome**: Interface definition plus TODO tests covering contract expectations.
- **Checklist**:
  - Define transport interface (connect, send, flush, shutdown).
  - Implement mock remote transport for tests.
  - Capture design decisions in ADR (`docs/adr/0001-remote-transport.md`).
- **Owner**: _TBD_
- **Dependencies**: Implement Config-Driven Console & File Sinks.

### P2 — Contribution & Governance Guide
- **Summary**: Outline coding standards, branching strategy, and review expectations.
- **Outcome**: `CONTRIBUTING.md` + `docs/decision-records/README.md` for ongoing ADRs.
- **Checklist**:
  - Draft contribution walkthrough (setup, tests, style).
  - Document roadmap alignment and release cadence.
  - Establish template for future issues/PRs.
- **Owner**: _TBD_
- **Dependencies**: None.

### P2 — Language Expansion Decision Record
- **Summary**: Prioritize next language targets and prerequisites for each.
- **Outcome**: ADR capturing rationale for C++ first, followed by JS/.NET.
- **Checklist**:
  - Evaluate integration complexity, tooling requirements, and packaging considerations.
  - Document interoperability constraints and shared config reuse strategy.
  - Define success metrics per language (e.g., sample app logging end-to-end).
- **Owner**: _TBD_
- **Dependencies**: Contribution & Governance Guide.
