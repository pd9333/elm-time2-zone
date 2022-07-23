module Etcetera exposing (zones)

{-| Zones in `tz/etcetera`
-}

import Dict exposing (Dict)
import Time2
import Time2.Zone.Specification exposing (Zone, ZoneRules(..), ZoneState)


minYear : Int
minYear =
    1970


maxYear : Int
maxYear =
    2037


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
        [ ( "Etc/GMT", etc__gmt )
        , ( "Etc/UTC", etc__utc )
        , ( "Etc/GMT-14", etc__gmt_14 )
        , ( "Etc/GMT-13", etc__gmt_13 )
        , ( "Etc/GMT-12", etc__gmt_12 )
        , ( "Etc/GMT-11", etc__gmt_11 )
        , ( "Etc/GMT-10", etc__gmt_10 )
        , ( "Etc/GMT-9", etc__gmt_9 )
        , ( "Etc/GMT-8", etc__gmt_8 )
        , ( "Etc/GMT-7", etc__gmt_7 )
        , ( "Etc/GMT-6", etc__gmt_6 )
        , ( "Etc/GMT-5", etc__gmt_5 )
        , ( "Etc/GMT-4", etc__gmt_4 )
        , ( "Etc/GMT-3", etc__gmt_3 )
        , ( "Etc/GMT-2", etc__gmt_2 )
        , ( "Etc/GMT-1", etc__gmt_1 )
        , ( "Etc/GMT+1", etc__gmt___1 )
        , ( "Etc/GMT+2", etc__gmt___2 )
        , ( "Etc/GMT+3", etc__gmt___3 )
        , ( "Etc/GMT+4", etc__gmt___4 )
        , ( "Etc/GMT+5", etc__gmt___5 )
        , ( "Etc/GMT+6", etc__gmt___6 )
        , ( "Etc/GMT+7", etc__gmt___7 )
        , ( "Etc/GMT+8", etc__gmt___8 )
        , ( "Etc/GMT+9", etc__gmt___9 )
        , ( "Etc/GMT+10", etc__gmt___10 )
        , ( "Etc/GMT+11", etc__gmt___11 )
        , ( "Etc/GMT+12", etc__gmt___12 )
        , ( "GMT", gmt )
        , ( "Etc/Universal", etc__universal )
        , ( "Etc/Zulu", etc__zulu )
        , ( "Etc/Greenwich", etc__greenwich )
        , ( "Etc/GMT-0", etc__gmt_0 )
        , ( "Etc/GMT+0", etc__gmt___0 )
        , ( "Etc/GMT0", etc__gmt0 )
        ]



-- Zones


{-| `Etc/GMT`
-}
etc__gmt : () -> Time2.Zone
etc__gmt _ =
    fromSpecification "Etc/GMT" (Zone [] (ZoneState 0 (Save 0)))


{-| `Etc/UTC`
-}
etc__utc : () -> Time2.Zone
etc__utc _ =
    fromSpecification "Etc/UTC" (Zone [] (ZoneState 0 (Save 0)))


{-| `Etc/GMT-14`
-}
etc__gmt_14 : () -> Time2.Zone
etc__gmt_14 _ =
    fromSpecification "Etc/GMT-14" (Zone [] (ZoneState 840 (Save 0)))


{-| `Etc/GMT-13`
-}
etc__gmt_13 : () -> Time2.Zone
etc__gmt_13 _ =
    fromSpecification "Etc/GMT-13" (Zone [] (ZoneState 780 (Save 0)))


{-| `Etc/GMT-12`
-}
etc__gmt_12 : () -> Time2.Zone
etc__gmt_12 _ =
    fromSpecification "Etc/GMT-12" (Zone [] (ZoneState 720 (Save 0)))


{-| `Etc/GMT-11`
-}
etc__gmt_11 : () -> Time2.Zone
etc__gmt_11 _ =
    fromSpecification "Etc/GMT-11" (Zone [] (ZoneState 660 (Save 0)))


{-| `Etc/GMT-10`
-}
etc__gmt_10 : () -> Time2.Zone
etc__gmt_10 _ =
    fromSpecification "Etc/GMT-10" (Zone [] (ZoneState 600 (Save 0)))


{-| `Etc/GMT-9`
-}
etc__gmt_9 : () -> Time2.Zone
etc__gmt_9 _ =
    fromSpecification "Etc/GMT-9" (Zone [] (ZoneState 540 (Save 0)))


{-| `Etc/GMT-8`
-}
etc__gmt_8 : () -> Time2.Zone
etc__gmt_8 _ =
    fromSpecification "Etc/GMT-8" (Zone [] (ZoneState 480 (Save 0)))


{-| `Etc/GMT-7`
-}
etc__gmt_7 : () -> Time2.Zone
etc__gmt_7 _ =
    fromSpecification "Etc/GMT-7" (Zone [] (ZoneState 420 (Save 0)))


{-| `Etc/GMT-6`
-}
etc__gmt_6 : () -> Time2.Zone
etc__gmt_6 _ =
    fromSpecification "Etc/GMT-6" (Zone [] (ZoneState 360 (Save 0)))


{-| `Etc/GMT-5`
-}
etc__gmt_5 : () -> Time2.Zone
etc__gmt_5 _ =
    fromSpecification "Etc/GMT-5" (Zone [] (ZoneState 300 (Save 0)))


{-| `Etc/GMT-4`
-}
etc__gmt_4 : () -> Time2.Zone
etc__gmt_4 _ =
    fromSpecification "Etc/GMT-4" (Zone [] (ZoneState 240 (Save 0)))


{-| `Etc/GMT-3`
-}
etc__gmt_3 : () -> Time2.Zone
etc__gmt_3 _ =
    fromSpecification "Etc/GMT-3" (Zone [] (ZoneState 180 (Save 0)))


{-| `Etc/GMT-2`
-}
etc__gmt_2 : () -> Time2.Zone
etc__gmt_2 _ =
    fromSpecification "Etc/GMT-2" (Zone [] (ZoneState 120 (Save 0)))


{-| `Etc/GMT-1`
-}
etc__gmt_1 : () -> Time2.Zone
etc__gmt_1 _ =
    fromSpecification "Etc/GMT-1" (Zone [] (ZoneState 60 (Save 0)))


{-| `Etc/GMT+1`
-}
etc__gmt___1 : () -> Time2.Zone
etc__gmt___1 _ =
    fromSpecification "Etc/GMT+1" (Zone [] (ZoneState -60 (Save 0)))


{-| `Etc/GMT+2`
-}
etc__gmt___2 : () -> Time2.Zone
etc__gmt___2 _ =
    fromSpecification "Etc/GMT+2" (Zone [] (ZoneState -120 (Save 0)))


{-| `Etc/GMT+3`
-}
etc__gmt___3 : () -> Time2.Zone
etc__gmt___3 _ =
    fromSpecification "Etc/GMT+3" (Zone [] (ZoneState -180 (Save 0)))


{-| `Etc/GMT+4`
-}
etc__gmt___4 : () -> Time2.Zone
etc__gmt___4 _ =
    fromSpecification "Etc/GMT+4" (Zone [] (ZoneState -240 (Save 0)))


{-| `Etc/GMT+5`
-}
etc__gmt___5 : () -> Time2.Zone
etc__gmt___5 _ =
    fromSpecification "Etc/GMT+5" (Zone [] (ZoneState -300 (Save 0)))


{-| `Etc/GMT+6`
-}
etc__gmt___6 : () -> Time2.Zone
etc__gmt___6 _ =
    fromSpecification "Etc/GMT+6" (Zone [] (ZoneState -360 (Save 0)))


{-| `Etc/GMT+7`
-}
etc__gmt___7 : () -> Time2.Zone
etc__gmt___7 _ =
    fromSpecification "Etc/GMT+7" (Zone [] (ZoneState -420 (Save 0)))


{-| `Etc/GMT+8`
-}
etc__gmt___8 : () -> Time2.Zone
etc__gmt___8 _ =
    fromSpecification "Etc/GMT+8" (Zone [] (ZoneState -480 (Save 0)))


{-| `Etc/GMT+9`
-}
etc__gmt___9 : () -> Time2.Zone
etc__gmt___9 _ =
    fromSpecification "Etc/GMT+9" (Zone [] (ZoneState -540 (Save 0)))


{-| `Etc/GMT+10`
-}
etc__gmt___10 : () -> Time2.Zone
etc__gmt___10 _ =
    fromSpecification "Etc/GMT+10" (Zone [] (ZoneState -600 (Save 0)))


{-| `Etc/GMT+11`
-}
etc__gmt___11 : () -> Time2.Zone
etc__gmt___11 _ =
    fromSpecification "Etc/GMT+11" (Zone [] (ZoneState -660 (Save 0)))


{-| `Etc/GMT+12`
-}
etc__gmt___12 : () -> Time2.Zone
etc__gmt___12 _ =
    fromSpecification "Etc/GMT+12" (Zone [] (ZoneState -720 (Save 0)))



-- Links


{-| `GMT` (alias of `Etc/GMT`)
-}
gmt : () -> Time2.Zone
gmt _ =
    fromSpecification "GMT" (Zone [] (ZoneState 0 (Save 0)))


{-| `Etc/Universal` (alias of `Etc/UTC`)
-}
etc__universal : () -> Time2.Zone
etc__universal _ =
    fromSpecification "Etc/Universal" (Zone [] (ZoneState 0 (Save 0)))


{-| `Etc/Zulu` (alias of `Etc/UTC`)
-}
etc__zulu : () -> Time2.Zone
etc__zulu _ =
    fromSpecification "Etc/Zulu" (Zone [] (ZoneState 0 (Save 0)))


{-| `Etc/Greenwich` (alias of `Etc/GMT`)
-}
etc__greenwich : () -> Time2.Zone
etc__greenwich _ =
    fromSpecification "Etc/Greenwich" (Zone [] (ZoneState 0 (Save 0)))


{-| `Etc/GMT-0` (alias of `Etc/GMT`)
-}
etc__gmt_0 : () -> Time2.Zone
etc__gmt_0 _ =
    fromSpecification "Etc/GMT-0" (Zone [] (ZoneState 0 (Save 0)))


{-| `Etc/GMT+0` (alias of `Etc/GMT`)
-}
etc__gmt___0 : () -> Time2.Zone
etc__gmt___0 _ =
    fromSpecification "Etc/GMT+0" (Zone [] (ZoneState 0 (Save 0)))


{-| `Etc/GMT0` (alias of `Etc/GMT`)
-}
etc__gmt0 : () -> Time2.Zone
etc__gmt0 _ =
    fromSpecification "Etc/GMT0" (Zone [] (ZoneState 0 (Save 0)))
