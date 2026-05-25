"""Unit tests for the liblogit Python API."""

import gc
import json
import logging
import os
import re
import sqlite3
import tempfile
import threading
import time
from pathlib import Path

import pytest

import liblogit
from liblogit.viewer import load_events


@pytest.fixture(autouse=True)
def clear_liblogit_environment(monkeypatch):
    """Keep developer-machine LIBLOGIT_* variables from leaking into tests."""

    for key in list(os.environ):
        if key.startswith("LIBLOGIT_"):
            monkeypatch.delenv(key, raising=False)


def write_config(tmp_path, data):
    """Helper that writes *data* to logit.json in *tmp_path* and returns the path."""

    path = tmp_path / "logit.json"
    path.write_text(json.dumps(data), encoding="utf-8")
    return path


def test_logging_to_file(tmp_path):
    config = {
        "level": {"threshold": "info", "tag": True},
        "timestamp": False,
        "file_location": str(tmp_path / "app.log"),
        "network_file_location": None,
    }
    cfg_path = write_config(tmp_path, config)

    liblogit.init_from_config(cfg_path)
    liblogit.LOG("info") << "hello world" << liblogit.ENDL

    log_contents = Path(config["file_location"]).read_text(encoding="utf-8").strip()
    assert "hello world" in log_contents
    assert log_contents.startswith("INFO")


def test_direct_logit_logs_to_console_and_file(tmp_path, capsys):
    log_path = tmp_path / "direct.log"
    app_log = liblogit.Logit(path=log_path, level=liblogit.DEBUG, timestamp=False)

    app_log(liblogit.DEBUG) << "direct message" << liblogit.ENDL
    app_log.flush()

    assert "DEBUG direct message" in log_path.read_text(encoding="utf-8")
    assert "DEBUG direct message" in capsys.readouterr().err


def test_console_sink_honors_text_level_timestamp_and_threshold(capsys):
    app_log = liblogit.LOGIT(
        name="ConsoleText",
        level="error",
        timestamp=True,
        tag_level=True,
        sinks=["console"],
    )

    app_log.log("debug", "hidden")
    app_log.log("fatal", "visible")
    app_log.flush()

    lines = capsys.readouterr().err.splitlines()
    assert len(lines) == 1
    assert lines[0].endswith("FATAL visible")
    assert re.match(r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}[+-]\d{4} FATAL visible", lines[0])


def test_console_sink_json_includes_metadata_and_structured_payload(capsys):
    app_log = liblogit.LOGIT(
        name="ConsoleJson",
        level="debug",
        timestamp=False,
        format="json",
        metadata={"component": "console"},
        sinks=["console"],
    )

    app_log.log("info", {"b": 2, "a": 1})
    app_log.flush()

    event = json.loads(capsys.readouterr().err)
    assert event == {
        "level": "info",
        "logger": "ConsoleJson",
        "message": '{"a":1,"b":2}',
        "metadata": {"component": "console"},
    }


def test_per_message_metadata_merges_static_metadata_in_json_and_database(tmp_path):
    json_path = tmp_path / "events.jsonl"
    database_path = tmp_path / "events.sqlite"
    app_log = liblogit.LOGIT(
        name="ContextLog",
        path=json_path,
        databasePath=database_path,
        level="debug",
        timestamp=False,
        format="json",
        metadata={"component": "api", "environment": "test"},
        sinks=["file", "database"],
    )

    app_log.log(
        "info",
        "request handled",
        metadata={"component": "worker", "request_id": "abc-123"},
    )
    app_log("warn").with_metadata({"request_id": "def-456", "attempt": 2}) << "retry" << liblogit.ENDL
    app_log.flush()

    events = [json.loads(line) for line in json_path.read_text(encoding="utf-8").splitlines()]
    assert events == [
        {
            "level": "info",
            "logger": "ContextLog",
            "message": "request handled",
            "metadata": {
                "component": "worker",
                "environment": "test",
                "request_id": "abc-123",
            },
        },
        {
            "level": "warn",
            "logger": "ContextLog",
            "message": "retry",
            "metadata": {
                "attempt": 2,
                "component": "api",
                "environment": "test",
                "request_id": "def-456",
            },
        },
    ]

    with sqlite3.connect(database_path) as connection:
        rows = connection.execute(
            "SELECT level, message, metadata_json FROM logit_events ORDER BY sequence"
        ).fetchall()

    assert rows == [
        (
            "info",
            "request handled",
            '{"component":"worker","environment":"test","request_id":"abc-123"}',
        ),
        (
            "warn",
            "retry",
            '{"attempt":2,"component":"api","environment":"test","request_id":"def-456"}',
        ),
    ]


def test_per_message_metadata_requires_mapping(tmp_path):
    app_log = liblogit.LOGIT(path=tmp_path / "events.log", level="debug", sinks=["file"])

    with pytest.raises(liblogit.LogConfigurationError, match="metadata must be an object"):
        app_log.log("info", "bad metadata", metadata=["not", "a", "mapping"])

    with pytest.raises(liblogit.LogConfigurationError, match="metadata must be an object"):
        app_log("info").with_metadata(["not", "a", "mapping"])


def test_redaction_masks_messages_metadata_console_file_and_database(tmp_path, capsys):
    log_path = tmp_path / "redacted.jsonl"
    database_path = tmp_path / "redacted.sqlite"
    app_log = liblogit.LOGIT(
        name="RedactedLog",
        path=log_path,
        databasePath=database_path,
        level="debug",
        timestamp=False,
        format="json",
        metadata={"component": "api", "nested": {"password": "static-pass"}, "token": "static-token"},
        sinks=["console", "file", "database"],
        redaction={
            "mask": "[MASKED]",
            "keys": ["password", "token"],
            "patterns": [r"secret=[^\s]+"],
        },
    )

    app_log.log(
        "info",
        {"action": "login", "password": "open-sesame", "nested": {"token": "abc-123"}},
        metadata={"note": "secret=from-metadata", "token": "event-token"},
    )
    app_log.log("warn", "secret=from-text", metadata={"safe": "value"})
    app_log.flush()

    file_events = [json.loads(line) for line in log_path.read_text(encoding="utf-8").splitlines()]
    console_events = [json.loads(line) for line in capsys.readouterr().err.splitlines()]
    assert console_events == file_events
    assert file_events[0] == {
        "level": "info",
        "logger": "RedactedLog",
        "message": '{"action":"login","nested":{"token":"[MASKED]"},"password":"[MASKED]"}',
        "metadata": {
            "component": "api",
            "nested": {"password": "[MASKED]"},
            "note": "[MASKED]",
            "token": "[MASKED]",
        },
    }
    assert file_events[1] == {
        "level": "warn",
        "logger": "RedactedLog",
        "message": "[MASKED]",
        "metadata": {
            "component": "api",
            "nested": {"password": "[MASKED]"},
            "safe": "value",
            "token": "[MASKED]",
        },
    }

    with sqlite3.connect(database_path) as connection:
        rows = connection.execute(
            "SELECT level, message, metadata_json FROM logit_events ORDER BY sequence"
        ).fetchall()

    assert rows[0][0:2] == (
        "info",
        '{"action":"login","nested":{"token":"[MASKED]"},"password":"[MASKED]"}',
    )
    assert json.loads(rows[0][2]) == file_events[0]["metadata"]
    assert rows[1][0:2] == ("warn", "[MASKED]")
    assert json.loads(rows[1][2]) == file_events[1]["metadata"]

    output_blob = log_path.read_text(encoding="utf-8") + "\n".join(
        row[1] + str(row[2]) for row in rows
    )
    assert "open-sesame" not in output_blob
    assert "abc-123" not in output_blob
    assert "event-token" not in output_blob
    assert "from-text" not in output_blob


def test_redaction_applies_to_streaming_fragments(tmp_path):
    log_path = tmp_path / "stream.log"
    app_log = liblogit.LOGIT(
        path=log_path,
        level="debug",
        timestamp=False,
        sinks=["file"],
        redaction={"keys": ["password"], "patterns": [r"token=[^\s]+"]},
    )

    app_log("info") << {"ok": True, "password": "stream-secret"} << " token=abc" << liblogit.ENDL
    app_log.flush()

    content = log_path.read_text(encoding="utf-8")
    assert content.strip() == 'INFO {"ok":true,"password":"[REDACTED]"} [REDACTED]'
    assert "stream-secret" not in content
    assert "token=abc" not in content


def test_redaction_config_validates_rules(tmp_path):
    with pytest.raises(liblogit.LogConfigurationError, match="redaction.patterns"):
        liblogit.LOGIT(path=tmp_path / "events.log", redaction={"patterns": ["("]})
    with pytest.raises(liblogit.LogConfigurationError, match="redaction.keys"):
        liblogit.LOGIT(path=tmp_path / "events.log", redaction={"keys": "password"})
    with pytest.raises(ValueError, match="redaction.mask"):
        liblogit.LogitConfig(path=tmp_path / "events.log", redaction={"keys": ["token"], "mask": ""})


def test_sync_buffering_mode_writes_immediately(tmp_path):
    log_path = tmp_path / "sync.log"
    app_log = liblogit.LOGIT(
        path=log_path,
        level="debug",
        timestamp=False,
        sinks=["file"],
        buffering={"mode": "sync"},
    )

    app_log.log("info", "immediate")

    assert log_path.read_text(encoding="utf-8").strip() == "INFO immediate"


def test_async_buffering_flushes_on_capacity_manual_flush_interval_and_close(tmp_path):
    capacity_path = tmp_path / "capacity.log"
    capacity_log = liblogit.LOGIT(
        path=capacity_path,
        level="debug",
        timestamp=False,
        sinks=["file"],
        buffering={"mode": "async", "capacity": 2, "flushIntervalSeconds": 60},
    )

    capacity_log.log("info", "first")
    assert not capacity_path.exists()
    capacity_log.log("info", "second")
    assert capacity_path.read_text(encoding="utf-8").splitlines() == [
        "INFO first",
        "INFO second",
    ]
    capacity_log.close()

    manual_path = tmp_path / "manual.log"
    manual_log = liblogit.LOGIT(
        path=manual_path,
        level="debug",
        timestamp=False,
        sinks=["file"],
        buffering={"mode": "async", "capacity": 10, "flushIntervalSeconds": 60},
    )

    manual_log.log("info", "held")
    assert not manual_path.exists()
    manual_log.flush()
    assert manual_path.read_text(encoding="utf-8").strip() == "INFO held"
    manual_log.close()

    interval_path = tmp_path / "interval.log"
    interval_log = liblogit.LOGIT(
        path=interval_path,
        level="debug",
        timestamp=False,
        sinks=["file"],
        buffering={"mode": "async", "capacity": 10, "flushIntervalSeconds": 0.05},
    )

    interval_log.log("info", "timer")
    deadline = time.monotonic() + 2
    while time.monotonic() < deadline:
        if interval_path.exists() and "INFO timer" in interval_path.read_text(encoding="utf-8"):
            break
        time.sleep(0.02)
    else:
        pytest.fail("buffered log did not flush on interval")
    interval_log.close()

    close_path = tmp_path / "close.log"
    close_log = liblogit.LOGIT(
        path=close_path,
        level="debug",
        timestamp=False,
        sinks=["file"],
        buffering={"mode": "async", "capacity": 10, "flushIntervalSeconds": 60},
    )

    close_log.log("info", "shutdown")
    assert not close_path.exists()
    close_log.close()
    assert close_path.read_text(encoding="utf-8").strip() == "INFO shutdown"


def test_buffering_config_validates_rules(tmp_path):
    with pytest.raises(liblogit.LogConfigurationError, match="buffering.mode"):
        liblogit.LOGIT(path=tmp_path / "events.log", buffering={"mode": "later"})
    with pytest.raises(liblogit.LogConfigurationError, match="buffering.capacity"):
        liblogit.LOGIT(path=tmp_path / "events.log", buffering={"mode": "async", "capacity": 0})
    with pytest.raises(ValueError, match="buffering.flushIntervalSeconds"):
        liblogit.LogitConfig(
            path=tmp_path / "events.log",
            buffering={"mode": "async", "flushIntervalSeconds": 0},
        )


def test_failure_policy_warn_drop_raise_retry_and_fallback(monkeypatch, tmp_path, capsys):
    class AlwaysFailingFileHandler(logging.Handler):
        def __init__(self, filename, rotation=None):
            super().__init__()

        def emit(self, record):
            raise OSError("sink exploded")

    monkeypatch.setattr(liblogit, "_CloseAfterEmitFileHandler", AlwaysFailingFileHandler)
    warn_log = liblogit.LOGIT(
        name="FailureWarn",
        path=tmp_path / "warn.log",
        level="debug",
        timestamp=False,
        sinks=["file"],
        failurePolicy={"mode": "warn"},
    )

    warn_log.log("info", "warned")
    assert "libLogit sink failure: sink exploded" in capsys.readouterr().err
    warn_log.close()

    drop_log = liblogit.LOGIT(
        name="FailureDrop",
        path=tmp_path / "drop.log",
        level="debug",
        timestamp=False,
        sinks=["file"],
        failurePolicy={"mode": "drop"},
    )

    drop_log.log("info", "dropped")
    assert "sink exploded" not in capsys.readouterr().err
    drop_log.close()

    raise_log = liblogit.LOGIT(
        name="FailureRaise",
        path=tmp_path / "raise.log",
        level="debug",
        timestamp=False,
        sinks=["file"],
        failurePolicy={"mode": "raise"},
    )

    with pytest.raises(OSError, match="sink exploded"):
        raise_log.log("info", "raised")
    raise_log.close()

    fallback_path = tmp_path / "fallback.log"
    fallback_log = liblogit.LOGIT(
        name="FailureFallback",
        path=tmp_path / "primary.log",
        level="debug",
        timestamp=False,
        sinks=["file"],
        failurePolicy={"mode": "fallback", "fallbackPath": fallback_path},
    )

    fallback_log.log("info", "fallback event")
    assert fallback_path.read_text(encoding="utf-8").strip() == "INFO fallback event"
    fallback_log.close()

    attempts = []

    class FlakyFileHandler(logging.Handler):
        def __init__(self, filename, rotation=None):
            super().__init__()
            self.filename = Path(filename)

        def emit(self, record):
            attempts.append(record.getMessage())
            if len(attempts) == 1:
                raise OSError("transient sink failure")
            with self.filename.open("a", encoding="utf-8") as stream:
                stream.write(self.format(record) + "\n")

    monkeypatch.setattr(liblogit, "_CloseAfterEmitFileHandler", FlakyFileHandler)
    retry_path = tmp_path / "retry.log"
    retry_log = liblogit.LOGIT(
        name="FailureRetry",
        path=retry_path,
        level="debug",
        timestamp=False,
        sinks=["file"],
        failurePolicy={"mode": "retry", "retryAttempts": 1},
    )

    retry_log.log("info", "retry event")
    assert attempts == ["retry event", "retry event"]
    assert retry_path.read_text(encoding="utf-8").strip() == "INFO retry event"
    assert "transient sink failure" not in capsys.readouterr().err
    retry_log.close()


def test_failure_policy_config_validates_rules(tmp_path):
    with pytest.raises(liblogit.LogConfigurationError, match="failurePolicy.mode"):
        liblogit.LOGIT(path=tmp_path / "events.log", failurePolicy={"mode": "panic"})
    with pytest.raises(liblogit.LogConfigurationError, match="fallbackPath"):
        liblogit.LOGIT(path=tmp_path / "events.log", failurePolicy={"mode": "fallback"})
    with pytest.raises(ValueError, match="failurePolicy.retryAttempts"):
        liblogit.LogitConfig(
            path=tmp_path / "events.log",
            failure_policy={"mode": "retry", "retryAttempts": -1},
        )


def test_concurrent_file_logging_does_not_interleave_or_drop_events(tmp_path):
    log_path = tmp_path / "threaded.log"
    app_log = liblogit.LOGIT(
        path=log_path,
        level="debug",
        timestamp=False,
        sinks=["file"],
    )

    worker_count = 8
    event_count = 40
    barrier = threading.Barrier(worker_count)
    errors = []

    def worker(worker_id):
        try:
            barrier.wait()
            for index in range(event_count):
                app_log.log("info", f"worker={worker_id} event={index}")
        except BaseException as exc:
            errors.append(exc)

    threads = [threading.Thread(target=worker, args=(worker_id,)) for worker_id in range(worker_count)]
    for thread in threads:
        thread.start()
    for thread in threads:
        thread.join(timeout=5)

    app_log.flush()
    app_log.close()

    assert errors == []
    assert all(not thread.is_alive() for thread in threads)
    lines = log_path.read_text(encoding="utf-8").splitlines()
    expected = {
        f"INFO worker={worker_id} event={index}"
        for worker_id in range(worker_count)
        for index in range(event_count)
    }
    assert len(lines) == len(expected)
    assert set(lines) == expected


def test_concurrent_buffered_logging_flushes_complete_events(tmp_path):
    log_path = tmp_path / "threaded-buffered.log"
    app_log = liblogit.LOGIT(
        path=log_path,
        level="debug",
        timestamp=False,
        sinks=["file"],
        buffering={"mode": "async", "capacity": 13, "flushIntervalSeconds": 60},
    )

    worker_count = 6
    event_count = 35
    barrier = threading.Barrier(worker_count)
    errors = []

    def worker(worker_id):
        try:
            barrier.wait()
            for index in range(event_count):
                app_log.log("info", f"buffered-worker={worker_id} event={index}")
        except BaseException as exc:
            errors.append(exc)

    threads = [threading.Thread(target=worker, args=(worker_id,)) for worker_id in range(worker_count)]
    for thread in threads:
        thread.start()
    for thread in threads:
        thread.join(timeout=5)

    app_log.flush()
    app_log.close()

    assert errors == []
    assert all(not thread.is_alive() for thread in threads)
    lines = log_path.read_text(encoding="utf-8").splitlines()
    expected = {
        f"INFO buffered-worker={worker_id} event={index}"
        for worker_id in range(worker_count)
        for index in range(event_count)
    }
    assert len(lines) == len(expected)
    assert set(lines) == expected


def test_file_sink_creates_parents_appends_utf8_and_uses_level_labels(tmp_path):
    log_path = tmp_path / "nested" / "utf8.log"
    app_log = liblogit.LOGIT(
        name="FileText",
        path=log_path,
        level="trace",
        timestamp=False,
        sinks=["file"],
    )

    app_log.log("info", "first café")
    app_log.log("fatal", "second")
    app_log.flush()

    assert log_path.read_bytes().decode("utf-8").splitlines() == [
        "INFO first café",
        "FATAL second",
    ]


def test_file_sink_rotates_by_size_and_backup_count(tmp_path):
    log_path = tmp_path / "rotating.log"
    app_log = liblogit.LOGIT(
        path=log_path,
        level="debug",
        timestamp=False,
        sinks=["file"],
        rotation={"maxBytes": 20, "maxFiles": 2},
    )

    for message in ("alpha", "beta", "gamma", "delta"):
        app_log.log("info", message)
    app_log.flush()

    assert log_path.read_text(encoding="utf-8").splitlines() == ["INFO delta"]
    assert Path(f"{log_path}.1").read_text(encoding="utf-8").splitlines() == ["INFO gamma"]
    assert Path(f"{log_path}.2").read_text(encoding="utf-8").splitlines() == ["INFO beta"]
    assert not Path(f"{log_path}.3").exists()


def test_rotation_config_validates_positive_limits(tmp_path):
    with pytest.raises(liblogit.LogConfigurationError, match="rotation.maxBytes"):
        liblogit.LOGIT(path=tmp_path / "events.log", rotation={"maxFiles": 2})
    with pytest.raises(liblogit.LogConfigurationError, match="rotation.maxFiles"):
        liblogit.LOGIT(path=tmp_path / "events.log", rotation={"maxBytes": 100, "maxFiles": 0})
    with pytest.raises(ValueError, match="rotation.maxBytes"):
        liblogit.LogitConfig(path=tmp_path / "events.log", rotation={"maxBytes": 0})


def test_blank_logit_can_be_configured_with_attributes(tmp_path, capsys):
    log_path = tmp_path / "declared.log"
    LogIT = liblogit.LOGIT()

    assert LogIT.localPath is None
    assert LogIT.remotePath is None

    LogIT.name = "LogIT"
    LogIT.level = liblogit.DEBUG
    LogIT.timestamp = False
    LogIT.localPath = log_path
    LogIT(liblogit.DEBUG) << "attribute configured" << liblogit.ENDL
    LogIT.flush()

    assert LogIT.name == "LogIT"
    assert LogIT.path == str(log_path)
    assert "file" in LogIT.sinks
    assert "DEBUG attribute configured" in log_path.read_text(encoding="utf-8")
    assert "DEBUG attribute configured" in capsys.readouterr().err


def test_logit_accepts_initial_structure(tmp_path):
    log_path = tmp_path / "structured.log"
    LogIT = liblogit.LOGIT(
        {
            "name": "Structured",
            "localPath": str(log_path),
            "level": "debug",
            "timestamp": False,
        }
    )

    LogIT(liblogit.DEBUG) << "from structure" << liblogit.ENDL
    LogIT.flush()

    assert LogIT.localPath == str(log_path)
    assert "DEBUG from structure" in log_path.read_text(encoding="utf-8")


def test_level_enum_parses_aliases_and_orders():
    assert liblogit.Level.parse("warning") is liblogit.Level.WARN
    assert liblogit.Level.parse(liblogit.Level.DEBUG) is liblogit.Level.DEBUG
    assert liblogit.Level.DEBUG < liblogit.Level.ERROR
    assert liblogit.Level.INFO.python_level == logging.INFO


def test_log_event_renders_structured_payloads():
    event = liblogit.LogEvent(
        logger="AppLog",
        level="warning",
        message={"b": 2, "a": [1, 2]},
        metadata={"component": "api"},
    )

    assert event.level_name == "warn"
    assert event.rendered_message == '{"a":[1,2],"b":2}'
    assert event.metadata_json == '{"component":"api"}'
    assert event.as_json_object() == {
        "logger": "AppLog",
        "level": "warn",
        "message": '{"a":[1,2],"b":2}',
        "metadata": {"component": "api"},
    }


def test_logit_config_validates_structure_and_feeds_logit(tmp_path):
    from liblogit.config import LogitConfig

    log_path = tmp_path / "configured.log"
    database_path = tmp_path / "configured.sqlite"
    config = LogitConfig.from_structure(
        {
            "name": "Configured",
            "localPath": log_path,
            "databasePath": database_path,
            "level": "warning",
            "metadata": {"component": "config"},
            "retention": {"maxRecords": 3},
        },
        defaults={"timestamp": False, "format": "text"},
    )

    assert config.level == "warn"
    assert config.sinks == ("console", "file", "database")
    assert config.to_structure()["retention"] == {"maxRecords": 3}

    logit = liblogit.LOGIT(config)
    logit.log("warn", {"z": 9, "a": 1})
    logit.flush()

    assert log_path.read_text(encoding="utf-8").strip() == 'WARNING {"a":1,"z":9}'
    with sqlite3.connect(database_path) as connection:
        row = connection.execute(
            "SELECT logger, level, message, metadata_json FROM logit_events"
        ).fetchone()
    assert row == ("Configured", "warn", '{"a":1,"z":9}', '{"component":"config"}')


def test_logit_config_rejects_invalid_structure(tmp_path):
    with pytest.raises(ValueError, match="Unsupported LOGIT keys"):
        liblogit.LogitConfig.from_structure({"unexpected": True})
    with pytest.raises(ValueError, match="path must not be empty"):
        liblogit.LogitConfig(path="")
    with pytest.raises(ValueError, match="retention.maxRecords"):
        liblogit.LogitConfig(database_path=tmp_path / "events.sqlite", retention={"maxRecords": 0})


def test_logit_submodule_exports_public_object():
    from liblogit.logit import LOGIT as module_logit_alias
    from liblogit.logit import Logit as module_logit

    assert module_logit is liblogit.Logit
    assert module_logit_alias is liblogit.LOGIT


def test_log_builder_commit_lifecycle_is_deterministic(tmp_path):
    log_path = tmp_path / "builder.log"
    logit = liblogit.LOGIT(path=log_path, level="debug", timestamp=False, sinks=["file"])

    builder = logit("info")
    builder << "one" << " two"
    builder.commit().commit()
    logit("debug").commit()
    with pytest.raises(liblogit.LogConfigurationError):
        logit("not-a-level")
    logit.flush()

    assert log_path.read_text(encoding="utf-8").splitlines() == ["INFO one two"]


def test_log_builder_destructor_fallback_emits_uncommitted_message(tmp_path):
    log_path = tmp_path / "destructor.log"
    logit = liblogit.LOGIT(path=log_path, level="debug", timestamp=False, sinks=["file"])

    builder = logit("info")
    builder << "best effort"
    del builder
    gc.collect()
    logit.flush()

    assert log_path.read_text(encoding="utf-8").strip() == "INFO best effort"


def test_structured_payloads_render_canonically_in_text_and_json(tmp_path):
    text_path = tmp_path / "payloads.log"
    text_log = liblogit.LOGIT(
        {
            "name": "PayloadText",
            "localPath": str(text_path),
            "level": "debug",
            "timestamp": False,
            "sinks": ["file"],
        }
    )
    text_log.log("info", {"b": 2, "a": 1})
    text_log("info") << ["user", {"id": 7, "roles": ["admin", "dev"]}] << liblogit.ENDL
    text_log.flush()

    assert text_path.read_text(encoding="utf-8").splitlines() == [
        'INFO {"a":1,"b":2}',
        'INFO ["user",{"id":7,"roles":["admin","dev"]}]',
    ]

    json_path = tmp_path / "payloads.jsonl"
    json_log = liblogit.LOGIT(
        {
            "name": "PayloadJson",
            "localPath": str(json_path),
            "level": "debug",
            "timestamp": False,
            "format": "json",
            "sinks": ["file"],
        }
    )
    json_log.log("warn", {"z": 0, "a": [2, 1]})
    json_log.flush()

    event = json.loads(json_path.read_text(encoding="utf-8"))
    assert event == {
        "level": "warn",
        "logger": "PayloadJson",
        "message": '{"a":[2,1],"z":0}',
    }


def test_directory_path_mode_uses_logit_name_for_file_and_remote_paths(tmp_path):
    local_dir = tmp_path / "local"
    remote_dir = tmp_path / "remote share"
    LogIT = liblogit.LOGIT(
        {
            "name": "Directory Log",
            "localPath": str(local_dir),
            "remotePath": str(remote_dir),
            "pathMode": "directory",
            "level": "debug",
            "timestamp": False,
            "sinks": ["file", "network"],
        }
    )

    LogIT.log("info", "path mode event")
    LogIT.flush()

    expected = "INFO path mode event"
    assert LogIT.pathMode == "directory"
    assert (local_dir / "Directory_Log.log").read_text(encoding="utf-8").strip() == expected
    assert (remote_dir / "Directory_Log.log").read_text(encoding="utf-8").strip() == expected


def test_invalid_path_mode_raises():
    with pytest.raises(liblogit.LogConfigurationError):
        liblogit.LOGIT({"pathMode": "folder"})


def test_load_named_logits_from_v2_config(tmp_path):
    app_path = tmp_path / "app.log"
    audit_path = tmp_path / "audit.log"
    cfg_path = write_config(
        tmp_path,
        {
            "version": "0.2",
            "defaults": {
                "level": "info",
                "timestamp": False,
                "tag_level": True,
                "sinks": ["file"],
            },
            "logits": {
                "AppLog": {
                    "path": str(app_path),
                    "level": "debug",
                },
                "AuditLog": {
                    "path": str(audit_path),
                },
            },
        },
    )

    logits = liblogit.load_logits(cfg_path)
    logits["AppLog"](liblogit.DEBUG) << "debug visible" << liblogit.ENDL
    logits["AuditLog"](liblogit.DEBUG) << "debug hidden" << liblogit.ENDL
    liblogit.get_logit("AuditLog")(liblogit.INFO) << "audit visible" << liblogit.ENDL

    assert "DEBUG debug visible" in app_path.read_text(encoding="utf-8")
    assert not audit_path.exists() or "debug hidden" not in audit_path.read_text(encoding="utf-8")
    assert "INFO audit visible" in audit_path.read_text(encoding="utf-8")


def test_environment_overrides_named_logit_level_and_path(tmp_path, monkeypatch):
    original_app_path = tmp_path / "original-app.log"
    override_app_path = tmp_path / "override-app.log"
    audit_path = tmp_path / "audit.log"
    monkeypatch.setenv("LIBLOGIT_LEVEL", "error")
    monkeypatch.setenv("LIBLOGIT_APPLOG_LEVEL", "debug")
    monkeypatch.setenv("LIBLOGIT_APPLOG_PATH", str(override_app_path))
    cfg_path = write_config(
        tmp_path,
        {
            "version": "0.2",
            "defaults": {
                "level": "info",
                "timestamp": False,
                "sinks": ["file"],
            },
            "logits": {
                "AppLog": {
                    "path": str(original_app_path),
                },
                "AuditLog": {
                    "path": str(audit_path),
                },
            },
        },
    )

    logits = liblogit.load_logits(cfg_path)
    logits["AppLog"].log("debug", "named override visible")
    logits["AuditLog"].log("info", "global override hidden")

    assert "DEBUG named override visible" in override_app_path.read_text(encoding="utf-8")
    assert not original_app_path.exists()
    assert not audit_path.exists()


def test_environment_override_can_disable_named_logit(tmp_path, monkeypatch):
    log_path = tmp_path / "disabled.log"
    monkeypatch.setenv("LIBLOGIT_APPLOG_ENABLED", "false")
    cfg_path = write_config(
        tmp_path,
        {
            "version": "0.2",
            "logits": {
                "AppLog": {
                    "path": str(log_path),
                    "level": "debug",
                    "timestamp": False,
                    "sinks": ["file"],
                }
            },
        },
    )

    logits = liblogit.load_logits(cfg_path)
    logits["AppLog"].log("error", "disabled")

    assert not log_path.exists()


def test_environment_override_supports_named_path_mode(tmp_path, monkeypatch):
    original_path = tmp_path / "audit.log"
    override_dir = tmp_path / "logs"
    monkeypatch.setenv("LIBLOGIT_AUDIT_LOG_PATH_MODE", "directory")
    monkeypatch.setenv("LIBLOGIT_AUDIT_LOG_PATH", str(override_dir))
    cfg_path = write_config(
        tmp_path,
        {
            "version": "0.2",
            "logits": {
                "Audit.Log": {
                    "path": str(original_path),
                    "level": "info",
                    "timestamp": False,
                    "sinks": ["file"],
                }
            },
        },
    )

    logits = liblogit.load_logits(cfg_path)
    logits["Audit.Log"].log("info", "directory override")

    assert not original_path.exists()
    assert (override_dir / "Audit.Log.log").read_text(encoding="utf-8").strip() == (
        "INFO directory override"
    )


def test_environment_global_path_override_applies_to_legacy_config(tmp_path, monkeypatch):
    original_path = tmp_path / "legacy.log"
    override_path = tmp_path / "env-legacy.log"
    monkeypatch.setenv("LIBLOGIT_LEVEL", "info")
    monkeypatch.setenv("LIBLOGIT_PATH", str(override_path))
    cfg_path = write_config(
        tmp_path,
        {
            "level": {"threshold": "error", "tag": True},
            "timestamp": False,
            "file_location": str(original_path),
            "network_file_location": None,
        },
    )

    liblogit.init_from_config(cfg_path)
    liblogit.LOG("info") << "legacy env path" << liblogit.ENDL

    assert "INFO legacy env path" in override_path.read_text(encoding="utf-8")
    assert not original_path.exists()


def test_load_logits_migrates_v1_config_to_default_logit(tmp_path):
    log_path = tmp_path / "migrated.log"
    cfg_path = write_config(
        tmp_path,
        {
            "level": {"threshold": "debug", "tag": True},
            "timestamp": False,
            "file_location": str(log_path),
            "network_file_location": None,
        },
    )

    logs = liblogit.load_logits(cfg_path)
    logs["default"].log("debug", "migrated registry")
    liblogit.get_logit("default").log("info", "retrieved default")
    logs["default"].flush()

    assert sorted(logs) == ["default"]
    assert logs["default"].name == "default"
    assert logs["default"].level == "debug"
    assert logs["default"].path == str(log_path)
    assert log_path.read_text(encoding="utf-8").splitlines() == [
        "DEBUG migrated registry",
        "INFO retrieved default",
    ]


def test_json_lines_format_includes_metadata(tmp_path):
    log_path = tmp_path / "events.jsonl"
    app_log = liblogit.Logit(
        name="AppLog",
        path=log_path,
        level="info",
        sinks=["file"],
        format="json",
        metadata={"component": "api"},
    )

    app_log.log("info", {"event": "started"})
    app_log.flush()

    event = json.loads(log_path.read_text(encoding="utf-8"))
    assert event["logger"] == "AppLog"
    assert event["level"] == "info"
    assert json.loads(event["message"]) == {"event": "started"}
    assert event["metadata"] == {"component": "api"}


def test_sqlite_database_sink_retains_newest_records(tmp_path):
    database_path = tmp_path / "events.sqlite"
    app_log = liblogit.LOGIT(
        {
            "name": "StoreLog",
            "databasePath": str(database_path),
            "level": "debug",
            "timestamp": False,
            "sinks": ["database"],
            "retention": {"maxRecords": 2},
            "metadata": {"component": "store"},
        }
    )

    app_log.log("debug", "first")
    app_log.log("info", "second")
    app_log.log("error", "third")

    with sqlite3.connect(database_path) as connection:
        rows = connection.execute(
            "SELECT logger, level, message, metadata_json FROM logit_events ORDER BY sequence"
        ).fetchall()

    assert rows == [
        ("StoreLog", "info", "second", '{"component":"store"}'),
        ("StoreLog", "error", "third", '{"component":"store"}'),
    ]


def test_sqlite_database_sink_evicts_rows_older_than_max_age(tmp_path):
    database_path = tmp_path / "age.sqlite"
    app_log = liblogit.LOGIT(
        {
            "name": "AgeLog",
            "databasePath": str(database_path),
            "level": "debug",
            "sinks": ["database"],
            "retention": {"maxAgeSeconds": 60},
        }
    )

    app_log.log("info", "old")
    with sqlite3.connect(database_path) as connection:
        connection.execute(
            "UPDATE logit_events SET created_at = ? WHERE message = ?",
            ("1970-01-01T00:00:00+00:00", "old"),
        )
    app_log.log("info", "fresh")

    with sqlite3.connect(database_path) as connection:
        rows = connection.execute("SELECT message FROM logit_events ORDER BY sequence").fetchall()

    assert rows == [("fresh",)]


def test_sqlite_database_sink_evicts_oldest_rows_over_logical_byte_budget(tmp_path):
    database_path = tmp_path / "bytes.sqlite"
    app_log = liblogit.LOGIT(
        {
            "name": "ByteLog",
            "databasePath": str(database_path),
            "level": "debug",
            "sinks": ["database"],
            "retention": {"maxBytes": 220},
        }
    )

    app_log.log("info", "first-" + ("x" * 80))
    app_log.log("info", "second-" + ("y" * 80))

    with sqlite3.connect(database_path) as connection:
        rows = connection.execute("SELECT message FROM logit_events ORDER BY sequence").fetchall()

    assert rows == [("second-" + ("y" * 80),)]


def test_sqlite_database_sink_loads_from_config(tmp_path):
    database_path = tmp_path / "configured.sqlite"
    cfg_path = write_config(
        tmp_path,
        {
            "version": "0.2",
            "defaults": {
                "level": "info",
                "timestamp": False,
                "sinks": ["database"],
                "retention": {"maxRecords": 10},
            },
            "logits": {
                "DbLog": {
                    "databasePath": str(database_path),
                }
            },
        },
    )

    logs = liblogit.load_logits(cfg_path)
    logs["DbLog"](liblogit.INFO) << "from config" << liblogit.ENDL

    with sqlite3.connect(database_path) as connection:
        rows = connection.execute("SELECT logger, level, message FROM logit_events").fetchall()

    assert rows == [("DbLog", "info", "from config")]


def test_viewer_load_events_filters_sqlite_store(tmp_path):
    database_path = tmp_path / "viewer.sqlite"
    app_log = liblogit.LOGIT(
        {
            "name": "ViewerLog",
            "databasePath": str(database_path),
            "level": "debug",
            "timestamp": False,
            "sinks": ["database"],
            "retention": {"maxRecords": 10},
        }
    )
    app_log.log("debug", "hidden from error filter")
    app_log.log("error", "visible failure")
    with sqlite3.connect(database_path) as connection:
        connection.execute(
            "UPDATE logit_events SET created_at = ? WHERE message = ?",
            ("2026-05-24T14:00:00", "hidden from error filter"),
        )
        connection.execute(
            "UPDATE logit_events SET created_at = ? WHERE message = ?",
            ("2026-05-24T15:00:00", "visible failure"),
        )

    events = load_events(
        database_path,
        logger="ViewerLog",
        level="error",
        search="visible",
        since="2026-05-24T14:30:00",
        until="2026-05-24T15:30:00",
    )

    assert len(events) == 1
    assert events[0]["logger"] == "ViewerLog"
    assert events[0]["level"] == "error"
    assert events[0]["message"] == "visible failure"

    assert load_events(database_path, since="2026-05-24T16:00:00") == []


def test_invalid_extra_key(tmp_path):
    config = {
        "level": "info",
        "timestamp": True,
        "file_location": None,
        "network_file_location": None,
        "unexpected": 123,
    }
    cfg_path = write_config(tmp_path, config)

    with pytest.raises(liblogit.LogConfigurationError):
        liblogit.init_from_config(cfg_path)


def test_missing_level_raises(tmp_path):
    config = {
        "timestamp": True,
        "file_location": None,
        "network_file_location": None,
    }
    cfg_path = write_config(tmp_path, config)

    with pytest.raises(liblogit.LogConfigurationError):
        liblogit.init_from_config(cfg_path)


def test_file_handler_fallback(monkeypatch, tmp_path, capsys):
    """Logger should warn and continue when file handlers cannot be attached."""

    config = {
        "level": {"threshold": "info", "tag": True},
        "timestamp": True,
        "file_location": str(tmp_path / "app.log"),
        "network_file_location": None,
    }
    cfg_path = write_config(tmp_path, config)

    def boom(*_args, **_kwargs):
        raise OSError("disk full")

    monkeypatch.setattr(liblogit, "_CloseAfterEmitFileHandler", boom)

    liblogit.init_from_config(cfg_path)
    liblogit.LOG("info") << "still works" << liblogit.ENDL

    captured = capsys.readouterr()
    assert "Unable to attach file logger" in captured.err
    assert "still works" in captured.err


def test_direct_logit_releases_file_handle_after_emit():
    """Direct file sinks should not keep temp log directories locked on Windows."""

    with tempfile.TemporaryDirectory() as directory:
        log_path = Path(directory) / "app.log"
        app_log = liblogit.LOGIT(
            {
                "localPath": str(log_path),
                "level": "debug",
                "timestamp": False,
                "sinks": ["file"],
            }
        )

        app_log(liblogit.INFO) << "releasable" << liblogit.ENDL

        assert log_path.read_text(encoding="utf-8").strip() == "INFO releasable"


def test_copy_sample_config_into_directory(tmp_path):
    sample_path = liblogit.copy_sample_config(tmp_path)
    assert sample_path == tmp_path / "logit.sample.json"

    data = json.loads(sample_path.read_text(encoding="utf-8"))
    assert data["version"] == "0.2"
    assert data["logits"]["AppLog"]["level"] == "debug"
    assert data["logits"]["AppLog"]["databasePath"] == "logs/app-logit.sqlite"


def test_copy_sample_config_overwrite(tmp_path):
    target = tmp_path / "logit.sample.json"
    target.write_text("{}", encoding="utf-8")

    with pytest.raises(FileExistsError):
        liblogit.copy_sample_config(target)

    updated = liblogit.copy_sample_config(target, overwrite=True)
    assert updated == target
    assert json.loads(updated.read_text(encoding="utf-8"))["defaults"]["timestamp"] is True
