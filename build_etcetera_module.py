#!/usr/bin/env python3

from pathlib import Path

import typer
import jinja2
import settings

ETCETERA_MODULE_TEMPLATE = """module Etcetera exposing (zones)

{-| Zones in `tz/etcetera`
-}

import Dict exposing (Dict)
import Time2
import Time2.Zone.Specification exposing (Zone, ZoneRules(..), ZoneState)


minYear : Int
minYear =
    {{ min_year }}


maxYear : Int
maxYear =
    {{ max_year }}


fromSpecification : String -> Zone -> Time2.Zone
fromSpecification name zone =
    let
        ( descending, bottom ) =
            zone |> Time2.Zone.Specification.toOffsets minYear maxYear
    in
    Time2.customZone name descending bottom


{-| zones
-}
zones : Dict String (() -> Time2.Zone)
zones =
    Dict.fromList
        [ {{ zone_var_names }}
        ]



-- Zones
{% for name, offset in zone_offsets.items() %}

{-| `{{ name }}`
-}
{{ name | zone_name_to_var_name }} : () -> Time2.Zone
{{ name | zone_name_to_var_name }} _ =
    fromSpecification "{{ name }}" (Zone [] (ZoneState {{ offset * 60 }} (Save 0)))
{% endfor %}


-- Links
{% for alias, canonical in zone_aliases.items() %}

{-| `{{ alias }}` (alias of `{{ canonical }}`)
-}
{{ alias | zone_name_to_var_name }} : () -> Time2.Zone
{{ alias | zone_name_to_var_name }} _ =
    fromSpecification "{{ alias }}" (Zone [] (ZoneState {{ zone_offsets[canonical] * 60 }} (Save 0)))
{% endfor %}
"""


def main(tz_dir: Path, etcetera_module: Path):
    etcetera_source_file = tz_dir / "etcetera"
    zone_offsets = {}
    zone_aliases = {}

    with etcetera_source_file.open() as file:
        for line in file:
            line = line.strip()
            if line == "" or line.startswith("#"):
                continue

            if line.startswith("Zone"):
                _, name, offset, *_ = line.split()
                zone_offsets[name] = int(offset)
            elif line.startswith("Link"):
                _, canonical, alias = line.split()
                zone_aliases[alias] = canonical

    with etcetera_module.open("w") as file:
        env = jinja2.Environment()
        env.filters["zone_name_to_var_name"] = zone_name_to_var_name
        env.from_string(ETCETERA_MODULE_TEMPLATE).stream(
            zone_offsets=zone_offsets,
            zone_aliases=zone_aliases,
            zone_var_names="\n        , ".join(
                f'( "{name}", {zone_name_to_var_name(name)} )'
                for name in (*zone_offsets, *zone_aliases)
            ),
            min_year=settings.min_year,
            max_year=settings.max_year,
        ).dump(file)


def zone_name_to_var_name(name: str):
    return name.replace("/", "__").replace("-", "_").replace("+", "___").lower()


if __name__ == "__main__":
    typer.run(main)
