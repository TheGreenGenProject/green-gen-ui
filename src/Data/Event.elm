module Data.Event exposing (..)


import Uuid exposing (Uuid)
import Data.User exposing (UserId)

type EventId = EventId Uuid
type alias Event = {
    id: EventId,
    content: String,
    participants: List UserId,
    location: (Float, Float)
  }

fromUuid: Uuid -> EventId
fromUuid uuid = EventId uuid

toString: EventId -> String
toString (EventId uuid) = uuid |> Uuid.toString