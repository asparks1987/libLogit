"""Packaging checks for the Python alpha artifact."""

from importlib import resources


def test_python_package_ships_sample_config_and_type_marker():
    package_root = resources.files("liblogit")

    assert package_root.joinpath("py.typed").is_file()
    assert resources.files("liblogit.data").joinpath("logit.sample.json").is_file()
    assert resources.files("liblogit.data").joinpath("logit.v1.sample.json").is_file()


def test_python_package_version_matches_alpha_release():
    import liblogit

    assert liblogit.__version__ == "1.0.0a1"


def test_public_error_type_comes_from_shared_module():
    import liblogit
    from liblogit.errors import LogConfigurationError

    assert liblogit.LogConfigurationError is LogConfigurationError


def test_package_root_uses_internal_config_type():
    import liblogit
    from liblogit.internal_config import _Config

    assert liblogit._Config is _Config


def test_package_root_uses_state_and_builder_modules():
    import liblogit
    from liblogit.builder import ENDL, _LogBuilder
    from liblogit.state import _LoggerState

    assert liblogit._LoggerState is _LoggerState
    assert liblogit._LogBuilder is _LogBuilder
    assert liblogit.ENDL is ENDL
