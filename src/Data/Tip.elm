module Data.Tip exposing (..)

import Uuid exposing (Uuid)
import Data.User exposing (UserId)
import Data.Schedule exposing (UTCTimestamp)

type TipId = TipId Uuid
type alias Tip = {
    id: TipId,
    author: UserId,
    content: String,
    created: UTCTimestamp
  }

fromUuid: Uuid -> TipId
fromUuid uuid = TipId uuid

toString: TipId -> String
toString (TipId uuid) = uuid |> Uuid.toString

fromString: String -> Maybe TipId
fromString = Maybe.map TipId << Uuid.fromString