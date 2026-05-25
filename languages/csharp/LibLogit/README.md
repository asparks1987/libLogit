# LibLogit for .NET

Alpha .NET binding for the libLogit `LOGIT` object model.

```csharp
using LibLogit;

var LogIT = new Logit
{
    LocalPath = "logs/csharp-app.log",
    Level = Level.Debug,
    Timestamp = false
};

LogIT.At(Level.Info).Append("C# app started").Commit();
```

This package is part of the libLogit v1 Alpha SDK. The Alpha supports direct
`LOGIT` objects, config-loaded named loggers, console/file output, text output,
JSON-lines output, and shared conformance fixtures. Advanced runtime behavior
such as async buffering, redaction, retry policy, and real remote transports is
beta-track.
