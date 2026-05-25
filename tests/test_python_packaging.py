"""Packaging checks for the Python alpha artifact."""

from importlib import resources


def test_python_package_ships_sample_config_and_type_marker():
    package_root = resources.files("liblogit")

    assert package_root.joinpath("py.typed").is_file()
    assert resources.files("liblogit.data").joinpath("logit.sample.json").is_file()
    assert resources.files("liblogit.data").joinpath("logit.v1.sample.json").is_file()
