# Ada Beta-Track Binding Plan

Ada is valuable for libLogit because it pushes the SDK toward precise contracts,
explicit failure behavior, and long-lived native applications. It is not part
of the v1 Alpha MVP language set. The Alpha release is intentionally focused on
Python, C++, C, C#, Java, JavaScript, Go, and Kotlin.

## Current State

The repository preserves an Ada sketch under `languages/ada/`:

- `liblogit.ads` defines a minimal package, `Log_Level`, `Config`, and logging
  procedures.
- `liblogit.adb` implements basic config loading, level filtering, console
  output, file output, and network-path-as-file output for the legacy v0.1
  shape.

The sketch is useful reference material, but it is not Alpha-supported because
it does not yet implement the v2 named `LOGIT` registry, the shared fixture
runner, package metadata, or the current Alpha object model.

## Beta Requirements

To promote Ada into the supported binding set, add:

| Area | Required Work |
|------|---------------|
| Object model | Expose a user-owned `LOGIT` record or controlled type instead of only global package state. |
| Config v2 | Load named `LOGIT` objects with defaults and object overrides. |
| Compatibility | Keep or document migration from the existing v0.1-style Ada config parser. |
| Fixtures | Add an Ada conformance runner for `tests/conformance/fixtures/`. |
| Packaging | Decide whether the first artifact is Alire, source vendoring, or a GitHub release archive. |
| Verification | Add a local Ada build/test command and hosted CI job after tooling is available. |
| Docs | Add install, direct object, config-loaded object, and limitations examples. |

## Conformance Disposition

Ada does not block v1 Alpha. Its current temporary gaps are explicit:

- No Alpha v2 registry loader.
- No shared fixture runner.
- No package metadata.
- No hosted CI coverage.
- No documented install path for users.

Those gaps are acceptable only because Ada is beta-track. If Ada becomes a
supported Beta binding, the same conformance and packaging gates used by the
Alpha MVP languages apply.
