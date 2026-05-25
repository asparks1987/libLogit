"""Inspect and smoke-test the Python libLogit release wheel.

This script is intentionally dependency-free so both local RC verification and
hosted GitHub Actions can prove the same installed-package behavior.
"""

from __future__ import annotations

import argparse
import os
import subprocess
import sys
import tarfile
import tempfile
import zipfile
from pathlib import Path


EXPECTED_PACKAGE_NAME = "liblogit"
EXPECTED_PACKAGE_VERSION = "1.0.0a1"
EXPECTED_WHEEL_NAME = f"{EXPECTED_PACKAGE_NAME}-{EXPECTED_PACKAGE_VERSION}-py3-none-any.whl"
EXPECTED_SDIST_NAME = f"{EXPECTED_PACKAGE_NAME}-{EXPECTED_PACKAGE_VERSION}.tar.gz"

SMOKE_CODE = r"""
import contextlib
import io
import json
import os
import sqlite3
import subprocess
import sys
from pathlib import Path

from liblogit import DEBUG, ENDL, INFO, LOGIT, __version__
from liblogit.viewer import load_events

work = Path(sys.argv[1])
logs = work / "logs"
remote = work / "remote"
database_path = logs / "installed.sqlite"
log_path = logs / "installed.log"
remote_path = remote / "installed-remote.log"

stream = io.StringIO()
with contextlib.redirect_stderr(stream):
    LogIT = LOGIT(
        {
            "name": "InstalledSmoke",
            "localPath": str(log_path),
            "remotePath": str(remote_path),
            "databasePath": str(database_path),
            "level": "debug",
            "timestamp": False,
            "format": "json",
            "metadata": {"component": "rc-smoke"},
            "retention": {"maxRecords": 5},
            "sinks": ["console", "file", "network", "database"],
        }
    )
    LogIT(DEBUG) << {"event": "installed_wheel_smoke", "step": 1} << ENDL
    LogIT.log(INFO, "installed wheel info event", metadata={"scope": "rc"})
    LogIT.close()

console_output = stream.getvalue()
if "installed_wheel_smoke" not in console_output:
    raise AssertionError("Installed wheel did not emit the expected console output.")

for path in (log_path, remote_path, database_path):
    if not path.is_file():
        raise AssertionError(f"Installed wheel did not create expected sink: {path}")

file_text = log_path.read_text(encoding="utf-8")
remote_text = remote_path.read_text(encoding="utf-8")
if "installed wheel info event" not in file_text:
    raise AssertionError("Installed wheel did not append the expected local file output.")
if "installed_wheel_smoke" not in remote_text:
    raise AssertionError("Installed wheel did not append the expected remote-path output.")

with sqlite3.connect(database_path) as connection:
    rows = connection.execute(
        "SELECT logger, level, message, metadata_json FROM logit_events ORDER BY sequence"
    ).fetchall()
if len(rows) != 2:
    raise AssertionError(f"Expected two SQLite rows, found {len(rows)}.")
if rows[0][0] != "InstalledSmoke" or rows[0][1] != "debug":
    raise AssertionError("Installed wheel SQLite row did not preserve logger/level.")
metadata = json.loads(rows[1][3])
if metadata.get("component") != "rc-smoke" or metadata.get("scope") != "rc":
    raise AssertionError("Installed wheel SQLite row did not preserve merged metadata.")

events = load_events(database_path, logger="InstalledSmoke", search="info event", limit=10)
if len(events) != 1 or events[0]["level"] != "info":
    raise AssertionError("Installed wheel viewer API did not filter expected events.")

scripts_dir = Path(sys.executable).parent
viewer_name = "liblogit-viewer.exe" if os.name == "nt" else "liblogit-viewer"
viewer = scripts_dir / viewer_name
if not viewer.is_file():
    raise AssertionError(f"Installed console script is missing: {viewer}")
result = subprocess.run(
    [
        str(viewer),
        str(database_path),
        "--print",
        "--logger",
        "InstalledSmoke",
        "--search",
        "wheel",
    ],
    check=True,
    capture_output=True,
    text=True,
)
if "installed wheel info event" not in result.stdout:
    raise AssertionError("Installed viewer console script did not print expected events.")

print(f"Installed wheel smoke test passed for liblogit {__version__}.")
"""


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("dist", type=Path, help="Directory containing one libLogit wheel and one sdist")
    parser.add_argument(
        "--skip-install-smoke",
        action="store_true",
        help="Only inspect archive contents without installing the wheel",
    )
    args = parser.parse_args(argv)

    wheel, sdist = inspect_artifacts(args.dist)
    if not args.skip_install_smoke:
        run_installed_wheel_smoke(wheel)
    return 0


def inspect_artifacts(dist: Path) -> tuple[Path, Path]:
    dist = dist.resolve()
    wheels = sorted(dist.glob("liblogit-*.whl"))
    sdists = sorted(dist.glob("liblogit-*.tar.gz"))
    if len(wheels) != 1:
        raise AssertionError(f"Expected one wheel, found {len(wheels)}")
    if len(sdists) != 1:
        raise AssertionError(f"Expected one sdist, found {len(sdists)}")

    wheel = wheels[0]
    sdist = sdists[0]
    if wheel.name != EXPECTED_WHEEL_NAME:
        raise AssertionError(f"Unexpected wheel filename: {wheel.name}")
    if sdist.name != EXPECTED_SDIST_NAME:
        raise AssertionError(f"Unexpected sdist filename: {sdist.name}")

    inspect_wheel(wheel)
    inspect_sdist(sdist)
    print(f"Inspected {wheel.name}", flush=True)
    print(f"Inspected {sdist.name}", flush=True)
    print("RC artifact inspection passed.", flush=True)
    return wheel, sdist


def inspect_wheel(wheel: Path) -> None:
    with zipfile.ZipFile(wheel) as archive:
        wheel_names = set(archive.namelist())
        entry_points = [name for name in wheel_names if name.endswith(".dist-info/entry_points.txt")]
        metadata = [name for name in wheel_names if name.endswith(".dist-info/METADATA")]
        if len(entry_points) != 1:
            raise AssertionError("Wheel is missing a single entry_points.txt")
        if len(metadata) != 1:
            raise AssertionError("Wheel is missing a single METADATA file")
        entry_point_text = archive.read(entry_points[0]).decode("utf-8")
        metadata_text = archive.read(metadata[0]).decode("utf-8")

    assert_metadata(metadata_text, "Wheel")

    wheel_exact = {
        "liblogit/__init__.py",
        "liblogit/builder.py",
        "liblogit/config.py",
        "liblogit/errors.py",
        "liblogit/events.py",
        "liblogit/formatters.py",
        "liblogit/internal_config.py",
        "liblogit/levels.py",
        "liblogit/logit.py",
        "liblogit/redaction.py",
        "liblogit/runtime.py",
        "liblogit/state.py",
        "liblogit/viewer.py",
        "liblogit/py.typed",
        "liblogit/data/logit.sample.json",
        "liblogit/data/logit.v1.sample.json",
        "liblogit/sinks/__init__.py",
        "liblogit/sinks/base.py",
    }
    missing = sorted(wheel_exact - wheel_names)
    if missing:
        raise AssertionError(f"Wheel is missing expected files: {missing}")

    for suffix in (
        "/share/liblogit/schema/logit.v2.schema.json",
        "/share/liblogit/examples/config/v2-basic.json",
        "/share/liblogit/examples/config/v2-database.json",
        "/share/liblogit/examples/config/v2-directory-path.json",
    ):
        if not any(name.endswith(suffix) for name in wheel_names):
            raise AssertionError(f"Wheel is missing data file ending with {suffix}")

    if "liblogit-viewer = liblogit.viewer:main" not in entry_point_text:
        raise AssertionError("Wheel console entry point is missing liblogit-viewer")


def inspect_sdist(sdist: Path) -> None:
    with tarfile.open(sdist) as archive:
        sdist_names = set(archive.getnames())
        pkg_info = [name for name in sdist_names if name.endswith("/PKG-INFO") and name.count("/") == 1]
        if len(pkg_info) != 1:
            raise AssertionError("sdist is missing a single PKG-INFO file")
        pkg_info_text = archive.extractfile(pkg_info[0])
        if pkg_info_text is None:
            raise AssertionError("sdist PKG-INFO file could not be read")
        metadata_text = pkg_info_text.read().decode("utf-8")

    assert_metadata(metadata_text, "sdist")

    for suffix in (
        "/README.md",
        "/docs/burndown.md",
        "/docs/adr/0001-alpha-packaging.md",
        "/docs/releases/v1.0.0-alpha.1-checklist.md",
        "/docs/releases/v1.0.0-alpha.1.md",
        "/docs/releases/v1.0.0-alpha.1-rc-evidence.md",
        "/examples/config/v2-basic.json",
        "/schema/logit.v2.schema.json",
        "/scripts/check-hosted-ci.ps1",
        "/scripts/verify-alpha.ps1",
        "/scripts/verify-static.ps1",
        "/scripts/verify-python-wheel-smoke.py",
        "/scripts/verify-javascript-package-smoke.js",
        "/scripts/verify-rc-local.ps1",
        "/tests/conformance/fixtures/basic_text_file.json",
        "/tests/conformance/fixtures/json_metadata.json",
        "/tests/conformance/fixtures/level_filtering.json",
        "/liblogit/builder.py",
        "/liblogit/runtime.py",
    ):
        if not any(name.endswith(suffix) for name in sdist_names):
            raise AssertionError(f"sdist is missing file ending with {suffix}")


def assert_metadata(metadata_text: str, artifact_name: str) -> None:
    required = {
        f"Name: {EXPECTED_PACKAGE_NAME}": f"{artifact_name} package name",
        f"Version: {EXPECTED_PACKAGE_VERSION}": f"{artifact_name} package version",
    }
    for line, description in required.items():
        if line not in metadata_text:
            raise AssertionError(f"{description} metadata is missing {line!r}")


def run_installed_wheel_smoke(wheel: Path) -> None:
    with tempfile.TemporaryDirectory(prefix="liblogit-wheel-venv-") as venv_dir:
        venv_path = Path(venv_dir).resolve()
        subprocess.run([sys.executable, "-m", "venv", str(venv_path)], check=True)
        venv_python = resolve_venv_python(venv_path)
        subprocess.run(
            [str(venv_python), "-m", "pip", "install", "--no-index", "--no-deps", str(wheel)],
            check=True,
        )
        with tempfile.TemporaryDirectory(prefix="liblogit-wheel-smoke-") as work_dir:
            env = os.environ.copy()
            env.pop("PYTHONPATH", None)
            env["PYTHONNOUSERSITE"] = "1"
            subprocess.run(
                [str(venv_python), "-c", SMOKE_CODE, work_dir],
                cwd=work_dir,
                env=env,
                check=True,
            )


def resolve_venv_python(venv_path: Path) -> Path:
    candidates = (
        venv_path / "Scripts" / "python.exe",
        venv_path / "bin" / "python",
    )
    for candidate in candidates:
        if candidate.is_file():
            return candidate
    raise AssertionError(f"Could not find Python executable in virtual environment: {venv_path}")


if __name__ == "__main__":
    raise SystemExit(main())
