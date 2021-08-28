module Data.Partner exposing (..)

import Data.Post exposing (PostId)
import Data.Url exposing (Url)
import Data.User exposing (UserId)
import Uuid exposing (Uuid)


type PartnerId = PartnerId Uuid

type alias Partner = {
    id: PartnerId,
    userId: UserId,
    name: String,
    description: String,
    url: Url
 }

toString: PartnerId -> String
toString (PartnerId uuid) = uuid |> Uuid.toString