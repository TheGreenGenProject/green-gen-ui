module Query.Json.ScheduleDecoder exposing (decodeSchedule)

import Data.Schedule exposing (Duration(..), Schedule(..))
import Json.Decode as Decoder exposing (Decoder, int, succeed)
import Json.Decode.Pipeline exposing (required)
import Query.Json.DecoderUtils exposing (decodeTimestamp)


decodeSchedule: Decoder Schedule
decodeSchedule = Decoder.oneOf [
        decodeOneOff,
        decodeRecurring
    ]

decodeOneOff: Decoder Schedule
decodeOneOff = Decoder.field "OneOff"
    (succeed OneOff
        |> required "start" decodeTimestamp
        |> required "end" decodeTimestamp)

decodeRecurring: Decoder Schedule
decodeRecurring = Decoder.field "Recurring"
    (succeed Recurring
        |> required "first" decodeTimestamp
        |> required "duration" decodeDuration
        |> required "every" decodeDuration
        |> required "until" decodeTimestamp)

decodeDuration: Decoder Duration
decodeDuration = succeed Duration
    |> required "millis" int