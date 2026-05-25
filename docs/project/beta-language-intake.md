# Beta Language Intake

This list ranks likely post-Alpha bindings by user value, ecosystem fit,
implementation cost, and package distribution difficulty. It is a planning
guide, not a promise that every language ships in the first Beta.

## Ranking Criteria

| Criterion | Meaning |
|-----------|---------|
| User demand | Likely number of users who would benefit from native-feeling logging. |
| Ecosystem fit | How naturally `LOGIT` maps to common language patterns. |
| Implementation cost | Expected effort to build, test, and maintain the binding. |
| Distribution difficulty | Package publishing, binary compatibility, runtime requirements, and CI cost. |

## Priority List

| Priority | Language | Why It Matters | Main Risk | Suggested First Artifact |
|----------|----------|----------------|-----------|--------------------------|
| 1 | TypeScript | Builds directly on the JavaScript Alpha binding and gives typed Node users a better API. | Keeping JS and TS docs/packages aligned. | npm package with generated declarations or source TypeScript. |
| 2 | Rust-style API | Strong desktop/service demand and good fit for structured logging concepts. | Ownership and async expectations may push beyond Alpha semantics. | Crate with sync file/console sinks and fixture runner. |
| 3 | Swift | Important for macOS desktop apps and future Apple-platform reach. | Package and platform testing require macOS hosted coverage. | Swift Package Manager module. |
| 4 | PHP | Useful for broad web hosting adoption and simple file logging. | Runtime versions and deployment environments vary widely. | Composer package. |
| 5 | Ruby | Good ergonomics for DSL-like `LOGIT` setup and scripting users. | Smaller modern demand than TypeScript/Rust/Swift. | RubyGem. |
| 6 | Scala | Extends JVM coverage for teams that want functional or typed APIs. | Java/Kotlin bindings may already satisfy many JVM users. | Maven/Gradle artifact wrapping Java binding. |
| 7 | Dart | Gives Flutter desktop/mobile projects a route into the SDK vision. | Non-desktop targets may need separate path and storage rules. | pub.dev package focused on desktop first. |
| 8 | Objective-C | Complements Swift for older macOS/iOS codebases. | Demand may be limited and packaging is less uniform. | Source package or CocoaPods proof of concept. |
| 9 | Delphi/Object Pascal | Relevant for Windows desktop applications. | Toolchain availability and CI setup may be difficult. | Source vendoring package with examples. |
| 10 | Ada | Valuable for high-assurance/native systems and already has sketches. | Needs modernized object model, v2 config, packaging, and Ada CI tooling. | Alire or source-vendored package after fixture runner exists. |

## Intake Workflow

1. Open a binding proposal using `docs/bindings/new-language.md`.
2. Confirm the target package artifact and minimum runtime version.
3. Implement the direct `LOGIT` object and config-loaded object.
4. Add a shared fixture runner.
5. Add package metadata and one direct plus one config-loaded example.
6. Add hosted CI only after local verification is stable.

## Beta Cut Line

The first Beta should prefer fewer well-verified bindings over many partial
ones. A binding should not count toward Beta readiness until it has install
instructions, conformance coverage, and a clear maintenance owner.
