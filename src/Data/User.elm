module Data.User exposing (..)

import Uuid exposing(Uuid)

type UserId = UserId Uuid
type alias User = {
    id: UserId,
    pseudo: String,
    enabled: Bool
 }

fromUuid: Uuid -> UserId
fromUuid = UserId

fromString: String -> Maybe UserId
fromString = Maybe.map UserId << Uuid.fromString

toString: UserId -> String
toString (UserId uuid) = uuid |> Uuid.toString