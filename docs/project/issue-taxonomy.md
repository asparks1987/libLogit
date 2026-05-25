# Issue Labels and Milestones

This taxonomy keeps libLogit work traceable from GitHub issues to the burndown
and release notes. Labels should describe the type, phase, binding, and urgency
of the work without overfitting to one implementation detail.

## Priority Labels

| Label | Meaning |
|-------|---------|
| `priority:p0` | Blocks the current Alpha release gate or a failing verifier. |
| `priority:p1` | Important Alpha/Beta work that should be scheduled soon. |
| `priority:p2` | Useful follow-up, cleanup, or polish. |

## Type Labels

| Label | Meaning |
|-------|---------|
| `type:bug` | Incorrect behavior, regression, or broken verification. |
| `type:feature` | New SDK capability or binding behavior. |
| `type:docs` | README, API docs, specs, release notes, or examples. |
| `type:test` | Unit, conformance, static, CI, or packaging verification. |
| `type:packaging` | PyPI, CMake, NuGet, npm, Maven, Go module, or release artifact work. |
| `type:research` | Evaluation, spike, or design comparison. |
| `type:governance` | Contribution, labels, milestones, ADRs, or project process. |

## Area Labels

| Label | Meaning |
|-------|---------|
| `area:spec` | Shared `LOGIT`, level, streaming, compatibility, or store contract. |
| `area:config` | JSON Schema, examples, migration, path rules, or environment overrides. |
| `area:runtime` | Sinks, formatters, filtering, buffering, failure handling, or event flow. |
| `area:store` | SQLite database sink, retention, SQL examples, or viewer data model. |
| `area:viewer` | Desktop log viewer and filters. |
| `area:ci` | GitHub Actions, local verifiers, static checks, or toolchain setup. |
| `area:release` | Release notes, release candidate, packaging gates, or publication checklist. |

## Binding Labels

| Label | Meaning |
|-------|---------|
| `binding:python` | Python reference package or viewer. |
| `binding:c` | C binding. |
| `binding:cpp` | C++ binding and CMake package. |
| `binding:csharp` | C#/.NET binding. |
| `binding:java` | Java binding and Maven metadata. |
| `binding:javascript` | JavaScript/Node package. |
| `binding:go` | Go module. |
| `binding:kotlin` | Kotlin facade and Maven metadata. |
| `binding:future` | Ada or beta-track language expansion work. |

## Phase Labels

| Label | Meaning |
|-------|---------|
| `phase:0-project` | Repo framing, contribution workflow, ADRs, labels, and tooling. |
| `phase:1-spec` | Shared product specification. |
| `phase:2-config` | Config v2 and migration. |
| `phase:3-python` | Python reference implementation. |
| `phase:4-runtime` | Sinks, formatting, and runtime behavior. |
| `phase:4a-store-viewer` | SQLite store and desktop viewer showcase. |
| `phase:5-native` | C/C++ implementation and distribution. |
| `phase:6-bindings` | Alpha bindings and beta binding playbook. |
| `phase:7-quality` | Conformance, static checks, and CI. |
| `phase:8-release` | Documentation, packaging, and release candidate work. |

## Status Labels

| Label | Meaning |
|-------|---------|
| `status:needs-triage` | Needs owner, phase, priority, and acceptance criteria. |
| `status:blocked` | Cannot proceed; blocker must also be recorded in `docs/burndown.md`. |
| `status:ready` | Clear scope and ready for implementation. |
| `status:in-progress` | Actively being worked. |
| `status:needs-verification` | Code/docs are present but release gate evidence is missing. |
| `status:beta-track` | Explicitly outside v1 Alpha unless promoted by maintainers. |

## Milestones

| Milestone | Scope |
|-----------|-------|
| `v1.0.0-alpha.1` | First working SDK milestone for Python, C++, C, C#, Java, JavaScript, Go, Kotlin, SQLite store, viewer, docs, and local verifier. |
| `v1-beta-contract` | Stabilize the `LOGIT` contract, binding template, and language intake rules. |
| `v1-beta-runtime` | OS/hardware validation plus redaction, buffering, failure policies, and real remote transports. |
| `post-beta` | Package ecosystem expansion, additional viewers, and non-core integrations. |

## Issue Template Checklist

Every issue should include:

- Goal and user impact.
- Affected phase and binding labels.
- Acceptance criteria tied to files, tests, docs, or release gates.
- Verification command or reason verification is not possible yet.
- Blocker note in `docs/burndown.md` when the issue is blocked.

## Release Gate Rule

Before an issue can be marked complete for Alpha, at least one authoritative
verification path must be recorded: a test, static check, package command,
schema validation, rendered doc review, or explicit blocker entry in
`docs/burndown.md`.
