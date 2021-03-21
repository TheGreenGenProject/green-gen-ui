module Query.Json.RankDecoder exposing (..)

import Data.Rank exposing (ScoreBreakdown)
import Json.Decode exposing (Decoder, int, succeed)
import Json.Decode.Pipeline exposing (required)


decodeBreakdown: Decoder ScoreBreakdown
decodeBreakdown = succeed ScoreBreakdown
    |> required "fromLikes"   int
    |> required "fromFollows" int
    |> required "fromPosts"   int
    |> required "fromEvents"  int
