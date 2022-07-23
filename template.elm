module Time2.Zone exposing
    ( version
    , get, getOrUtc
    , zones
    , ZONE_IDS
    )

{-| This library provides time zone data from the `VERSION` release of the IANA
Time Zone Database.

@docs version


## Find a zone

@docs get, getOrUtc


## Zones

@docs zones

-}

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


{-|

    get "America/New_York"
    --> Just (america__new_york ())

-}
get : String -> Maybe Time2.Zone
get name =
    Dict.get name zones
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


{-| You can look up an unevaluated zone by its zone name in the `zones` dictionary.

    import Dict

    Dict.get "America/New_York" zones
    --> Just america__new_york

-}
zones : Dict String (() -> Time2.Zone)
zones =
    [ ZONE_NAME_ID_PAIRS
    ]
        |> Dict.fromList
