module Time2.Zone exposing
    ( version
    , get, getOrUtc
    , zoneNames
    , ZONE_IDS
    )

{-| This library provides time zone data from the `VERSION` release of the IANA
Time Zone Database.

@docs version


# Usage


## Find a zone

@docs get, getOrUtc


## Zone names

@docs zoneNames


## Zones

@docs ZONE_IDS

-}

import Backward
import Dict exposing (Dict)
import Maybe exposing (Maybe)
import Time exposing (Month(..), Weekday(..))
import Time2
import Time2.Zone.Specification exposing (Clock(..), DateTime, DayOfMonth(..), Rule, Zone, ZoneRules(..), ZoneState)


{-| What release of the IANA Time Zone Database is this data from?
-}
version : String
version =
    "VERSION"


minYear : Int
minYear =
    MIN_YEAR


maxYear : Int
maxYear =
    MAX_YEAR


fromSpecification : String -> Zone -> Time2.Zone
fromSpecification name zone =
    let
        ( descending, bottom ) =
            zone |> Time2.Zone.Specification.toOffsets minYear maxYear
    in
    Time2.customZone name descending bottom


{-| Get [`Time2.Zone`][zone] of the given name, with deprecated names changed to new names.

[zone]: /packages/pd9333/elm-time2/latest/Time2#Zone

    get "Africa/Asmera"
    --> Just (africa__nairobi ())

    get "America/New_York"
    --> Just (america__new_york ())

-}
get : String -> Maybe Time2.Zone
get name =
    let
        name_ =
            Dict.get name Backward.deprecatedNames
                |> Maybe.withDefault name
    in
    Dict.get name_ zones
        |> Maybe.map (\f -> f ())


{-|

    import Time2

    getOrUtc (Just "America/New_York")
    --> america__new_york ()

    getOrUtc Nothing
    --> Time2.utc

    getOrUtc (Just "XXX")
    --> Time2.utc

-}
getOrUtc : Maybe String -> Time2.Zone
getOrUtc name =
    Maybe.andThen get name
        |> Maybe.withDefault Time2.utc


{-| All zone names.
-}
zoneNames : List String
zoneNames =
    Dict.keys zones


zones : Dict String (() -> Time2.Zone)
zones =
    [ ZONE_NAME_ID_PAIRS
    ]
        |> Dict.fromList
