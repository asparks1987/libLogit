"""Shim to support ``python setup.py`` invocations.

The project metadata lives in ``setup.cfg`` and ``pyproject.toml``.
"""

from setuptools import setup


if __name__ == "__main__":
    setup()
