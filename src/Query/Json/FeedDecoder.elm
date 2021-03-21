module Query.Json.FeedDecoder exposing (..)

import Data.Feed exposing (Feed(..))
import Data.Page exposing (Page(..))
import Json.Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Query.Json.PostDecoder exposing (decodePostIds, decodePosts)


decodeFeed: Page -> Decoder Feed
decodeFeed page = succeed Feed
    |> hardcoded page
    |> required "posts" decodePostIds