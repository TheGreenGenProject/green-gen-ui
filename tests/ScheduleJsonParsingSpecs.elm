module ScheduleJsonParsingSpecs exposing (suite)

import Data.Schedule exposing (Duration(..), Schedule(..), UTCTimestamp(..))
import Expect
import Json.Decode as Decoder exposing (Error(..), Value)
import Query.Json.ScheduleDecoder exposing (decodeSchedule)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Schedule Json decoders"
        [ describe "should decode correctly"
            [
              test "a OneOff schedule" <|
                \_ ->
                    """{
                        "OneOff": {
                            "start": { "value": 1608992036107 },
                            "end": { "value": 1608995096107 }
                       }}"""
                        |> Decoder.decodeString decodeSchedule
                        |> Expect.equal (OneOff (UTC 1608992036107) (UTC 1608995096107) |> Ok),

              test "a Recurring schedule" <|
                \_ ->
                    """{
                        "Recurring": {
                            "first": { "value": 1608992036107 },
                            "duration": { "millis": 60000 },
                            "every": { "millis": 360000000 },
                            "until": { "value": 1608995096107 }
                       }}"""
                        |> Decoder.decodeString decodeSchedule
                        |> Expect.equal (Recurring (UTC 1608992036107) (Duration 60000) (Duration 360000000) (UTC 1608995096107) |> Ok)
            ]
        ]