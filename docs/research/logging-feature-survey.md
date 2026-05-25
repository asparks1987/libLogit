# Logging Feature Survey

Research date: 2026-05-22

This survey looks at established logging libraries, logging standards, and log-management products to identify the feature set libLogit should treat as most sought after. "Sought after" here means a feature appears repeatedly across successful libraries, platform documentation, and observability tooling expectations.

## Scope

Reviewed references include:

- OpenTelemetry logging specification and log data model.
- Python logging ecosystem: standard logging, Loguru, structlog.
- JVM logging ecosystem: Log4j 2, Logback-style concepts, SLF4J-style abstraction patterns.
- .NET logging ecosystem: Serilog and NLog.
- JavaScript logging ecosystem: Pino and Winston.
- C++ logging ecosystem: spdlog.
- Go logging ecosystem: zap and zerolog.
- Log-management and observability platforms: Elastic Logs, Grafana Loki, Datadog Log Management, and Splunk log correlation guidance.

## Top Feature Signals

| Rank | Feature | Evidence Pattern | libLogit Implication |
|------|---------|------------------|----------------------|
| 1 | Structured logging | OpenTelemetry formalizes a log data model; Pino and zerolog default to JSON; Serilog, NLog, structlog, Elastic, Loki, and Datadog all center structured fields or metadata. | `LOGIT` should emit structured events internally from the start, even when rendering text. Basic JSON output should be alpha scope. |
| 2 | Multiple sinks/transports/appenders | Log4j, Serilog, Winston, spdlog, NLog, Loguru, and platform shippers all model logs as routed to one or more destinations. | `LOGIT` needs a sink model, not one hard-coded file path. Alpha should include console and file; beta adds OTLP/HTTP/syslog/custom sinks. |
| 3 | Level thresholds and per-sink filtering | Serilog highlights minimum levels and per-sink restrictions; Log4j uses logger/appender filters; Winston transports can each have levels; spdlog supports runtime/compile-time level filtering. | `LOGIT` needs object-level threshold plus sink-level threshold overrides. |
| 4 | Context enrichment | OpenTelemetry emphasizes trace/span/resource correlation; Serilog enrichers add event properties; Pino child loggers attach fields; structlog builds context dictionaries; Loki labels and structured metadata identify service/source. | `LOGIT` should support static metadata in alpha and context-bound metadata in beta. |
| 5 | Low overhead and async behavior | Pino markets low overhead and worker-thread transports; Log4j and spdlog provide async loggers; zap and zerolog focus on allocation/performance. | Python Alpha supports batched buffering; Beta should add cross-binding parity and backpressure rules. |
| 6 | Configurable formatting | Log4j layouts, Serilog output templates, Winston formats, NLog layouts, spdlog patterns, and Loguru formats are core concepts. | `LOGIT` should separate event creation from rendering. Alpha should provide text and JSON; beta can add templates and custom formatters. |
| 7 | File management: rotation, retention, compression | Loguru, spdlog, Python handlers, Log4j rolling appenders, and many platform setups expect bounded files. | Rotation/retention is important, but can follow after alpha if alpha warns clearly that files are append-only. |
| 8 | Redaction/filtering/security | Datadog documents filtering and obfuscation; Pino exposes redaction; platform pipelines commonly mask secrets before storage. | Python Alpha includes field and regex redaction before sinks receive events; Beta should add cross-binding parity. Do not ship remote/cloud sinks without redaction strategy. |
| 9 | Observability integration | OpenTelemetry bridges existing loggers to a uniform data model and trace correlation; Elastic and Splunk document routing/collection/correlation flows. | libLogit should be OpenTelemetry-friendly: stable field names, trace/span fields, service metadata, and future OTLP sink. |
| 10 | Extensibility | Winston custom transports, NLog custom targets/layouts, Log4j plugins, spdlog custom sinks/format flags, structlog processors. | `LOGIT` needs extension points: custom sinks, custom formatters, and event processors. These can start as internal interfaces in alpha. |
| 11 | Developer ergonomics | Loguru emphasizes simple setup; Pino and spdlog show tiny quick starts; Serilog has fluent configuration; users repeatedly want less boilerplate. | The direct object API must be the hero: include library, instantiate `LOGIT`, log. Config files should be optional, not mandatory. |
| 12 | Exception/backtrace support | OpenTelemetry defines exception log conventions; Loguru, spdlog, and language logging stacks expose stack/backtrace support. | Alpha should support exception payloads where the host language makes that natural; beta should standardize exception fields. |

## Feature Frequency Matrix

Legend: `Core` means the feature is central or heavily documented; `Yes` means supported; `Partial` means supported through extension, platform, or adjacent tooling.

| Product/Library | Structured | Sinks | Per-Sink Levels | Context | Async/Perf | Formatting | Rotation | Redaction | OTel/Platform Fit |
|-----------------|------------|-------|-----------------|---------|------------|------------|----------|-----------|-------------------|
| OpenTelemetry Logs | Core | Yes | Partial | Core | Yes | Partial | No | Partial | Core |
| Log4j 2 | Yes | Core | Core | Core | Core | Core | Core | Partial | Yes |
| Serilog | Core | Core | Core | Core | Yes | Core | Yes | Partial | Yes |
| NLog | Core | Core | Core | Core | Yes | Core | Yes | Partial | Yes |
| Pino | Core | Core | Yes | Core | Core | Yes | Partial | Core | Yes |
| Winston | Yes | Core | Core | Yes | Partial | Core | Partial | Partial | Yes |
| spdlog | Partial | Core | Yes | Partial | Core | Core | Core | Partial | Partial |
| Loguru | Yes | Core | Yes | Yes | Yes | Core | Core | Partial | Partial |
| structlog | Core | Partial | Yes | Core | Yes | Core | Partial | Partial | Yes |
| zap | Core | Yes | Yes | Core | Core | Yes | Partial | Partial | Yes |
| zerolog | Core | Yes | Yes | Core | Core | Core | Partial | Partial | Yes |
| Elastic Logs | Core | Core | Core | Core | Core | Core | Core | Core | Core |
| Grafana Loki | Core | Core | Core | Core | Core | Partial | Core | Partial | Core |
| Datadog Logs | Core | Core | Core | Core | Core | Core | Core | Core | Core |

## Recommended v1 Alpha Feature Set

Alpha should be small, but it should not feel toy-like. The most credible alpha is:

1. Direct `LOGIT` object instantiation in Python, C++, Java, and Kotlin.
2. Named `LOGIT` objects loaded from shared JSON config.
3. Severity enum and threshold filtering.
4. Object-level metadata: `name`, `service`, `component`, `environment`, and arbitrary static key/value fields.
5. Console sink and append-only file sink.
6. Basic sink-level thresholds.
7. Text formatter for humans.
8. JSON-lines formatter for machines.
9. Stable internal `LogEvent` structure with timestamp, severity, body/message, attributes, resource metadata, and optional trace/span fields.
10. Disabled-level fast path so ignored logs do minimal work.
11. Basic exception/error support in each alpha language where idiomatic.
12. Compatibility bridge for existing v0.1 config into one default `LOGIT`.

Alpha can defer:

- Async/buffered sinks.
- Rotation/retention/compression.
- Regex and field redaction.
- OTLP/HTTP/syslog/database sinks.
- Dynamic config reload.
- Full template engine.
- Rich per-request context propagation.
- Complete exception semantic-convention mapping.

## Recommended v1 Beta Feature Set

Beta should be the first expansion push:

1. Binding template for new OOP languages.
2. OpenTelemetry-friendly field names and an optional OTLP exporter.
3. Async/buffered sink mode with flush, shutdown, queue limits, and backpressure policy.
4. Rotation, retention, and compression for file sinks.
5. Redaction processors for field names and regex matches.
6. Context propagation helpers: child loggers, scoped context, thread-local/context-local storage where available.
7. Custom sink, formatter, and processor interfaces.
8. Config overlays from environment variables.
9. Conformance fixtures for every supported language.
10. Packaging playbooks for the next set of languages.

## Product Positioning Takeaways

- libLogit should not try to beat every logger on depth in v1 alpha. It should win on the portable `LOGIT` object contract.
- The distinctive promise is "configure a logging object once, then use the same object model across languages."
- The alpha library must still respect modern logging expectations: structured events, JSON output, sink routing, level thresholds, metadata, and performance-aware disabled logs.
- The endgame is not identical syntax everywhere. It is identical semantics with idiomatic APIs.

## Source Notes

- OpenTelemetry Logs specification: https://opentelemetry.io/docs/specs/otel/logs/
- OpenTelemetry Logs data model: https://opentelemetry.io/docs/specs/otel/logs/data-model/
- Serilog configuration basics: https://github.com/serilog/serilog/wiki/Configuration-Basics
- Apache Log4j async loggers: https://logging.apache.org/log4j/2.x/manual/async.html
- Apache Log4j architecture and appenders/filters concepts: https://logging-log4j.staged.apache.org/log4j/2.x/manual/architecture.html
- Pino README: https://github.com/pinojs/pino
- Winston README: https://github.com/winstonjs/winston
- spdlog README: https://github.com/gabime/spdlog
- NLog project overview: https://nlog-project.org/
- structlog documentation: https://www.structlog.org/en/stable/
- Loguru logger API: https://loguru.readthedocs.io/en/stable/api/logger.html
- zap README: https://github.com/uber-go/zap
- zerolog README: https://github.com/rs/zerolog
- Grafana Loki labels: https://grafana.com/docs/loki/latest/get-started/labels/
- Elastic log monitoring: https://www.elastic.co/docs/solutions/observability/logs
- Datadog log pipelines: https://docs.datadoghq.com/logs/log_configuration/pipelines/
- Datadog log data security: https://docs.datadoghq.com/data_security/logs/
