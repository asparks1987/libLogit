# ADR 0003: Binding Implementation Strategy

- Status: Accepted
- Deciders: libLogit maintainers
- Date: 2026-05-24

## Context

libLogit's long-term goal is language-agnostic logging, but the Alpha goal is
different: prove that the `LOGIT` object model is useful across the MVP
language set without forcing every ecosystem through an unnatural API.

The current Alpha supports Python, C++, C, C#, Java, JavaScript, Go, and
Kotlin. Those languages have different packaging systems, object models,
runtime assumptions, memory models, and error-handling conventions. A single
shared binary core could reduce duplicated logging logic, but it would also add
FFI, build, distribution, and platform risk before the public contract has
settled.

## Decision

v1 Alpha uses idiomatic per-language bindings backed by one shared contract:

- The `LOGIT` object spec defines required fields and behavior.
- The config schemas define portable structure-fed initialization.
- The conformance fixtures define observable output.
- The API docs explain each language's native spelling of the same concept.
- The local and hosted verification scripts keep the bindings from drifting.

Bindings may share code where it is natural, such as Kotlin wrapping the Java
binding, but Alpha does not require every language to call into one native core.

Beta will re-evaluate a shared native core or generated binding layer after
real Alpha feedback answers which parts of the contract are stable enough to
centralize.

## Alternatives Considered

A shared C or C++ core with thin bindings was considered because it could give
one implementation of sinks, retention, formatting, and future remote
transports. It was deferred because Alpha still needs fast API iteration and
because packaging a native core into npm, NuGet, Maven, PyPI, Go, and JVM
tooling adds release risk.

A generated-code approach was also considered. It remains attractive for Beta
after the binding template, config schema, and conformance fixtures are more
complete.

## Consequences

Alpha can ship a useful SDK sooner and let each ecosystem feel natural. The
tradeoff is duplicated implementation effort. Conformance fixtures, release
gates, and the future binding template are mandatory because they are the guard
rails that keep per-language code behaving like one SDK.

For Beta, the default path is:

1. Keep the shared spec and conformance suite as the source of truth.
2. Add new bindings through the documented template.
3. Promote shared implementation pieces only where they reduce drift without
   making install or packaging harder for users.
