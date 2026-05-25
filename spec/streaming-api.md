# Streaming API Specification

Status: v0.2 alpha draft

The streaming API lets users build one log message by appending fragments before committing it.

Canonical shape:

```text
LogIT(INFO) << "user " << user_id << " signed in"
```

## Required Semantics

- Calling a `LOGIT` with a level creates a message builder bound to that `LOGIT`.
- Appending fragments preserves order.
- The builder emits at most once.
- Explicit commit/flush must be available in every Alpha binding.
- Empty builders should not emit by default.
- Structured values should render predictably; Alpha requires dict/map/list/array support where natural in the language.
- Structured values render as compact JSON text with deterministic object-key
  ordering where the language runtime can provide it. For example,
  `{"b": 2, "a": 1}` renders as `{"a":1,"b":2}` in the Python reference
  implementation.

## Commit Triggers

Bindings should support the most idiomatic explicit commit mechanism:

| Language | Alpha Commit Shape |
|----------|--------------------|
| Python | `<< ENDL`, `.commit()`, destructor best effort |
| C++ | stream destructor best effort, explicit `.commit()` where available |
| C | `liblogit_commit(builder)` |
| C# | `.Commit()` |
| Java | `.commit()` |
| JavaScript | `.commit()` |
| Go | `.Commit()` |
| Kotlin | `.commit()` or idiomatic operator helper |

Destructor/finalizer emission is best effort only. Documentation must prefer explicit commit for deterministic output.

## Errors

- Unknown levels must error before writing.
- Sink failures follow the Alpha failure policy: warn and continue where possible.
- Configuration errors must not be silently ignored.
