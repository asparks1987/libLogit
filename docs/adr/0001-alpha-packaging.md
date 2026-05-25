# ADR 0001: v1 Alpha Packaging Plan

- Status: Accepted
- Deciders: libLogit maintainers
- Date: 2026-05-24

## Context

v1 Alpha is useful only if a project can add libLogit and start logging without
copying implementation files by hand. The alpha language set is Python, C++,
C, C#, Java, JavaScript, Go, and Kotlin. Each ecosystem expects a different
installation shape, but every package must expose the same `LOGIT` object
contract, examples, license metadata, and conformance status.

The first release can be source-first while package publication is prepared.
The packaging plan must make it clear which artifacts are release blockers and
which are beta hardening work.

## Decision

v1 Alpha will publish or document one installable artifact per supported
ecosystem, with the Python package remaining the reference package.

| Language | Alpha artifact | Public package target | Alpha release requirement |
|----------|----------------|-----------------------|---------------------------|
| Python | `liblogit` package from repository root | PyPI | Ship source distribution and wheel with `py.typed`, sample config, schema/config data files, README, release docs, conformance fixtures, release verification scripts, and the `liblogit-viewer` console script; the RC verifier checks artifact filenames/internal metadata, installs the built wheel, and smoke-tests console, local file, remote-path-as-file, SQLite, and viewer CLI behavior. |
| C++ | Header-only `libLogit.h`, public `<liblogit/logit.hpp>` wrapper, and CMake `libLogit::cpp` target | GitHub release archive first; package manager recipes later | CMake configure/build/test/install works, installed consumers can `find_package(libLogit CONFIG REQUIRED)`, the native package config reports version `1.0.0`, and `nlohmann/json.hpp` is documented as a dependency. |
| C | `languages/c/liblogit.h`, `languages/c/liblogit.c`, public `<liblogit/logit.h>` wrapper, and CMake `libLogit::c` static target | GitHub release archive first; package manager recipes later | CMake configure/build/test/install works, installed consumers can link `libLogit::c`, the native package config reports version `1.0.0`, and the C API surface is documented. |
| C# | `LibLogit` project with NuGet metadata | NuGet | Package metadata exists, Alpha targets `net10.0`, and `dotnet pack` produces `LibLogit.1.0.0-alpha.1.nupkg`; the RC verifier builds the package from a temp copy and checks exact nupkg naming, `.nuspec` metadata, packaged README, and `lib/net10.0/LibLogit.dll`. Beta should revisit multi-targeting and XML/API docs. |
| Java | `dev.liblogit` Maven project | Maven Central or GitHub Packages | Maven coordinates exist and `mvn package` produces `liblogit-1.0.0-alpha.1.jar`; the RC verifier builds the jar from a temp copy and checks runtime classes plus embedded POM/properties metadata. Sources jar and javadocs remain release polish. |
| Kotlin | Kotlin facade over Java binding with companion Maven descriptor | Maven Central or GitHub Packages | Companion coordinates depend on the Java binding; Java `mvn install` plus Kotlin `mvn package` produces the Alpha jar, while sources jar, javadocs, and publication signing remain release polish. |
| JavaScript | Node-compatible package with npm metadata | npm | `package.json` exports `LOGIT`, `Logit`, `loadLogits`, and level constants; `npm pack --dry-run` validates the Alpha package and `scripts/verify-javascript-package-smoke.js` checks the packed tarball name/version, installs it into a fresh consumer, verifies installed package metadata, and logs to console, local file, remote-path-as-file, and config-loaded file output. The local RC verifier runs the same release artifact smoke directly before Python artifact checks. |
| Go | Go module under `languages/go` | Go module proxy via Git tag | Module path, package docs, examples, and `go test ./...` are present. |

Every alpha package must include:

- A one-screen direct `LOGIT` quick start.
- A config-loaded example using `examples/config/v2-basic.json`.
- The shared capability table: direct object, structure-fed object, local path,
  remote path, level filtering, console sink, file sink, text formatter,
  JSON-lines formatter, config registry, and fixture status.
- A clear alpha warning that cross-binding redaction/buffering/failure-policy
  parity and real remote transports are beta-track unless explicitly
  implemented.
- A command that maintainers can run locally to verify that binding.

## Release Gates

The v1 Alpha release candidate cannot be cut until:

- Python package metadata builds a wheel and source distribution, the source
  distribution includes release docs, conformance fixtures, and release
  verification scripts, artifact filenames/internal metadata match the Alpha
  release name, and the wheel installs into an isolated environment that can
  immediately log through the Alpha sinks and query the SQLite store through
  the viewer CLI.
- C and C++ CMake install instructions and vendoring fallback notes are copied into release notes.
- C#, Java, Kotlin, JavaScript, and Go each have package metadata and a local
  package verification command; the RC verifier additionally inspects the C#
  NuGet artifact, inspects the Java Maven jar, and installs the packed npm
  tarball into a fresh consumer project to prove immediate JavaScript logging
  before Python artifact checks.
- The full local alpha matrix passes from a clean checkout.
- Known blockers are listed in `docs/burndown.md`.

## Alternatives Considered

One package containing every binding was rejected for Alpha. It would be hard
to install cleanly in language-specific tooling and would force consumers to
download runtimes they do not need.

A shared native core with thin bindings remains attractive for Beta, but it is
not required for Alpha because the current priority is proving the `LOGIT`
contract across idiomatic APIs.

## Consequences

This keeps Alpha practical: users can install or vendor only the binding they
need, while maintainers keep one conformance contract. The tradeoff is that
package metadata must be maintained in several ecosystems. The Beta binding
template should reduce that overhead before more languages are added.
