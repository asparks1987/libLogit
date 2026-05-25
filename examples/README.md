# libLogit Examples

These examples are intentionally small. Each Alpha MVP binding demonstrates the
same two startup paths:

- Direct `LOGIT` construction inside application code.
- Config-loaded named `LOGIT` construction from `examples/config/v2-basic.json`.

The Python example also demonstrates the Alpha SQLite log store and SQL access.

## Config Examples

| File | Purpose |
|------|---------|
| `config/v1-legacy.json` | v0.1 compatibility shape for migration tests. |
| `config/v2-basic.json` | One named text logger, `AppLog`. |
| `config/v2-buffering.json` | Python Alpha async/batched buffering with interval and manual flush support. |
| `config/v2-multi-logit.json` | Multiple named loggers with text and JSON output. |
| `config/v2-database.json` | Python Alpha SQLite database sink with record retention. |
| `config/v2-failure-policy.json` | Python Alpha sink failure policy with fallback file output. |
| `config/v2-redaction.json` | Python Alpha key and regex redaction before console/file/database sinks. |
| `config/v2-rotation.json` | Python Alpha size-based file rotation. |
| `config/invalid-extra-key.json` | Negative schema example. |

## Binding Examples

| Binding | File | What It Shows |
|---------|------|---------------|
| Python | `python/database.py` | Direct SQLite-backed `LOGIT`, config-loaded database `LOGIT`, and SQL reads. |
| C++ | `cpp/basic.cpp` | Direct object, file output, and config-loaded `AppLog`. |
| C | `c/basic.c` | Direct struct API, builder API, and config-loaded `AppLog`. |
| C# | `csharp/Basic/Program.cs` | Direct `Logit` plus config-loaded `AppLog`. |
| Java | `java/Basic.java` | Direct `Logit` plus config-loaded `AppLog`. |
| JavaScript | `javascript/basic.js` | Direct `LOGIT` plus config-loaded `AppLog`. |
| Go | `go/basic.go` | Direct `Logit` plus config-loaded `AppLog`. |
| Kotlin | `kotlin/basic.kt` | Direct `KotlinLogit` plus config-loaded `AppLog`. |

## Running Examples Locally

From the repository root:

```powershell
python examples/python/database.py
node examples/javascript/basic.js
dotnet run --project examples/csharp/Basic/Basic.csproj
javac -cp languages/java/src/main/java -d build/java-example examples/java/Basic.java languages/java/src/main/java/dev/liblogit/Logit.java
java -cp build/java-example Basic
```

Run the Go example from its module:

```powershell
Push-Location languages/go
go run ../../examples/go/basic.go
Pop-Location
```

Native C and C++ examples are built by the Alpha verifier through CMake. The
Kotlin example requires the Java classpath plus the Kotlin compiler.

## Beta-Track Example Ideas

The examples do not yet try to prove every future runtime feature. These are
tracked for Beta:

- Real remote transports beyond network-path-as-file.
- Cross-binding redaction parity.
- Cross-binding async buffering parity.
- Cross-binding retry/fallback parity.
- Age-based file rotation and compression.
- Additional language templates beyond the Alpha MVP set.
