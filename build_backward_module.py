#!/usr/bin/env python3

from pathlib import Path

import typer
import jinja2

template = """module Backward exposing (deprecatedNames)

{-| Links from tz/backward.
-}

import Dict exposing (Dict)


{-| Old Name -> New Name.

Note that `tz/backward` file has the format of "LINK NEW-NAME OLD-NAME".

-}
deprecatedNames : Dict String String
deprecatedNames =
    Dict.fromList
        [ {{ deprecated_names }}
        ]

"""


def main(tz_dir: Path, backward_module: Path):
    backward_source_file = tz_dir / "backward"
    links = dict()

    with backward_source_file.open() as file:
        for line in file:
            line = line.strip()
            if line == "" or line.startswith("#"):
                continue

            _, new, old, *_ = line.split()
            links[old] = new

    deprecated_names = "\n        , ".join(
        f'( "{old}", "{new}" )'
        for old, new in links.items()
    )
    with backward_module.open("w") as file:
        jinja2.Template(template).stream(deprecated_names=deprecated_names).dump(file)


if __name__ == "__main__":
    typer.run(main)
