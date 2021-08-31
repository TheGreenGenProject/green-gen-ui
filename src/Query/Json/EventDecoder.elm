module Query.Json.EventDecoder exposing (decodeEvent, decodeEventId)

import Data.Event exposing (Event, EventId(..))
import Json.Decode exposing (Decoder, int, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Query.Json.DecoderUtils exposing (decodeUserId, decodeUuid)
import Query.Json.LocationDecoder exposing (decodeLocation)
import Query.Json.ScheduleDecoder exposing (decodeSchedule)


decodeEvent: Decoder Event
decodeEvent = succeed Event
    |> required "id" decodeEventId
    |> required "owner" decodeUserId
    |> required "description" string
    |> required "maxParticipants" int
    |> required "schedule" decodeSchedule
    |> required "location" decodeLocation

decodeEventId: Decoder EventId
decodeEventId = succeed EventId
    |> required "value" decodeUuid