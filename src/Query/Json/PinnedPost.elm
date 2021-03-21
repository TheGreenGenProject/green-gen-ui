module Query.Json.PinnedPost exposing (..)

import Data.Post exposing (PinnedPost(..))
import Json.Decode as Decoder exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (required)
import Query.Json.DecoderUtils exposing (decodeTimestamp)
import Query.Json.PostDecoder exposing (decodePostId)


decodePinnedPosts: Decoder (List PinnedPost)
decodePinnedPosts = Decoder.list decodePinnedPost

decodePinnedPost: Decoder PinnedPost
decodePinnedPost = succeed PinnedPost
    |> required "postId" decodePostId
    |> required "timestamp" decodeTimestamp