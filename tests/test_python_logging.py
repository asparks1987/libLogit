"""Unit tests for the liblogit Python API."""

import json
from pathlib import Path

import pytest

import liblogit


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

    monkeypatch.setattr(liblogit.logging, "FileHandler", boom)

    liblogit.init_from_config(cfg_path)
    liblogit.LOG("info") << "still works" << liblogit.ENDL

    captured = capsys.readouterr()
    assert "Unable to attach file logger" in captured.err
    assert "still works" in captured.err


def test_copy_sample_config_into_directory(tmp_path):
    sample_path = liblogit.copy_sample_config(tmp_path)
    assert sample_path == tmp_path / "logit.sample.json"

    data = json.loads(sample_path.read_text(encoding="utf-8"))
    assert data["level"]["threshold"] == "info"


def test_copy_sample_config_overwrite(tmp_path):
    target = tmp_path / "logit.sample.json"
    target.write_text("{}", encoding="utf-8")

    with pytest.raises(FileExistsError):
        liblogit.copy_sample_config(target)

    updated = liblogit.copy_sample_config(target, overwrite=True)
    assert updated == target
    assert json.loads(updated.read_text(encoding="utf-8"))["timestamp"] is True
