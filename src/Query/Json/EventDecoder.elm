module Query.Json.EventDecoder exposing (decodeEvent, decodeEventId)

import Data.Event exposing (Event, EventId(..))
import Json.Decode exposing (Decoder, int, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Query.Json.DecoderUtils exposing (decodeUserId, decodeUuid)
import Query.Json.LocationDecoder exposing (decodeLocation)
import Query.Json.ScheduleDecoder exposing (decodeSchedule)

-- {"id":{"value":{"uuid":"e9e4e469-58e0-4a6d-98a2-dd280ac812df"}},
-- "owner":{"value":{"uuid":"44f5186f-bed8-4063-97f5-3f9bf2dd8871"}},
-- "maxParticipants":10,
-- "description":"#boom",
-- "location":{"Address":{"address":null,"zipCode":"E2","country":{"name":"World"}}},
-- "schedule":{"OneOff":{"start":{"value":1631989301075},"end":{"value":1632000101075}}}}
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