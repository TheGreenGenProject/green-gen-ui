module Query.Json.WallDecoder exposing (..)

import Data.Page exposing (Page(..))
import Data.User exposing (UserId)
import Data.Wall exposing (Wall(..))
import Json.Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Query.Json.PostDecoder exposing (decodePostIds, decodePosts)


decodeWall: UserId -> Page -> Decoder Wall
decodeWall userId page = succeed Wall
    |> hardcoded userId
    |> hardcoded page
    |> required "posts" decodePostIds
