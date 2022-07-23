module UpstreamTest exposing (..)

import Dict
import Expect
import Json.Decode
import Test exposing (Test)
import Time
import Time2
import Time2.Zone
import TimeZone


type alias Era =
    { start : Int
    , offset : Int
    }


eraDecoder : Json.Decode.Decoder Era
eraDecoder =
    Json.Decode.map2 Era
        (Json.Decode.field "s" Json.Decode.int)
        (Json.Decode.field "o" Json.Decode.int)


{-| Can't use `Time2.toStandardZone` here because we want to compare the raw data.
-}
decoderFromTime2Zone : Json.Decode.Decoder Time.Zone
decoderFromTime2Zone =
    Json.Decode.map2 Time.customZone
        (Json.Decode.field "o" Json.Decode.int)
        (Json.Decode.field "e" (Json.Decode.list eraDecoder))


findDifferWithUpstream : List String -> Expect.Expectation
findDifferWithUpstream names =
    case names of
        [] ->
            Expect.pass

        name :: rest ->
            let
                upstream =
                    Dict.get name TimeZone.zones
                        |> Maybe.map (\f -> f ())

                ours =
                    Time2.Zone.get name
                        |> Maybe.map Time2.encodeZone
                        |> Maybe.andThen (Json.Decode.decodeValue decoderFromTime2Zone >> Result.toMaybe)
            in
            if ours /= upstream then
                Expect.fail name

            else
                findDifferWithUpstream rest


sameAsUpstream : Test
sameAsUpstream =
    Test.test "sameAsUpstream" (\() -> findDifferWithUpstream Time2.Zone.zoneNames)
