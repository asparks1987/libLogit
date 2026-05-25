"""Python runner for shared libLogit conformance fixtures."""

import json
from datetime import datetime
from pathlib import Path

import pytest

import liblogit


FIXTURE_DIR = Path(__file__).parent / "conformance" / "fixtures"


def fixture_paths():
    return sorted(FIXTURE_DIR.glob("*.json"))


@pytest.mark.parametrize("fixture_path", fixture_paths(), ids=lambda path: f"{path.stem}-contract")
def test_conformance_fixture_contract(fixture_path):
    fixture = json.loads(fixture_path.read_text(encoding="utf-8"))

    assert isinstance(fixture.get("name"), str) and fixture["name"]
    assert isinstance(fixture.get("description"), str) and fixture["description"]
    assert isinstance(fixture.get("config"), dict)
    assert isinstance(fixture.get("messages"), list)
    assert isinstance(fixture.get("expected_files"), dict)

    clock = fixture.get("clock")
    assert isinstance(clock, dict)
    fixed_at = clock.get("fixed_at")
    assert isinstance(fixed_at, str) and fixed_at.endswith("Z")
    datetime.fromisoformat(fixed_at.replace("Z", "+00:00"))

    for message in fixture["messages"]:
        assert isinstance(message.get("logger"), str) and message["logger"]
        assert isinstance(message.get("level"), str) and message["level"]
        assert isinstance(message.get("fragments"), list)
        assert all(isinstance(fragment, str) for fragment in message["fragments"])

    for relative_path, expected_lines in fixture["expected_files"].items():
        assert isinstance(relative_path, str) and relative_path
        assert isinstance(expected_lines, list)
        assert all(isinstance(line, str) for line in expected_lines)


@pytest.mark.parametrize("fixture_path", fixture_paths(), ids=lambda path: path.stem)
def test_python_conformance_fixture(tmp_path, fixture_path):
    fixture = json.loads(fixture_path.read_text(encoding="utf-8"))
    config = fixture["config"]

    for logit_config in config["logits"].values():
        for key in ("path", "localPath", "file_location"):
            if key in logit_config and logit_config[key]:
                logit_config[key] = str(tmp_path / logit_config[key])
        for key in ("remotePath", "remote_path", "network_path", "network_file_location"):
            if key in logit_config and logit_config[key]:
                logit_config[key] = str(tmp_path / logit_config[key])

    config_path = tmp_path / "logit.json"
    config_path.write_text(json.dumps(config), encoding="utf-8")

    logits = liblogit.load_logits(config_path)
    for message in fixture["messages"]:
        builder = logits[message["logger"]](message["level"])
        for fragment in message["fragments"]:
            builder << fragment
        builder << liblogit.ENDL

    for logit in logits.values():
        logit.flush()

    for relative_path, expected_lines in fixture["expected_files"].items():
        actual_path = tmp_path / relative_path
        assert actual_path.exists(), f"Expected output file was not created: {relative_path}"
        actual_lines = actual_path.read_text(encoding="utf-8").splitlines()
        assert actual_lines == expected_lines
