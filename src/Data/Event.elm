module Data.Event exposing (..)

import Data.Location exposing (Location)
import Data.Schedule exposing (Schedule)
import Uuid exposing (Uuid)
import Data.User exposing (UserId)


type EventId = EventId Uuid
type alias Event = {
    id: EventId,
    owner: UserId,
    description: String,
    maxParticipants: Int,
    schedule: Schedule,
    location: Location
 }

fromUuid: Uuid -> EventId
fromUuid uuid = EventId uuid

toString: EventId -> String
toString (EventId uuid) = uuid |> Uuid.toString

fromString: String -> Maybe EventId
fromString = Maybe.map EventId << Uuid.fromString