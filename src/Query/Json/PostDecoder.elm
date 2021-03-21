module Query.Json.PostDecoder exposing (..)

import Data.Challenge exposing (ChallengeId(..))
import Data.Hashtag exposing (Hashtag(..))
import Data.Post exposing (Post, PostContent(..), PostId(..), Source(..))
import Json.Decode as Decoder exposing (Decoder, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Query.Json.DecoderUtils exposing(..)
import Query.Json.SourceDecoder exposing (decodeSources)
import Query.Json.TipDecoder exposing (decodeTipId)


decodePosts: Decoder (List Post)
decodePosts = Decoder.list decodePost

decodePost: Decoder Post
decodePost = Decoder.oneOf [
    decodeFreeTextPost,
    decodeTipPost,
    decodeChallengePost,
    decodeRePost
  ]

decodeFreeTextPost: Decoder Post
decodeFreeTextPost = decodePostFields |> Decoder.field "FreeTextPost"

decodeTipPost: Decoder Post
decodeTipPost = decodePostFields |> Decoder.field "TipPost"

decodeChallengePost: Decoder Post
decodeChallengePost = decodePostFields |> Decoder.field "ChallengePost"

decodeRePost: Decoder Post
decodeRePost = decodePostFields |> Decoder.field "RePost"

decodePostFields: Decoder Post
decodePostFields =
    Decoder.map5 Post
        (Decoder.field "id" decodePostId)
        (Decoder.field "author" decodeUserId)
        (decodePostContent)
        (Decoder.field "created" decodeTimestamp)
        (Decoder.field "hashtags" decodeHashtags)

decodePostIds: Decoder (List PostId)
decodePostIds = Decoder.list decodePostId

decodePostId: Decoder PostId
decodePostId = succeed PostId
    |> required "value" decodeUuid

decodeChallengeId: Decoder ChallengeId
decodeChallengeId = succeed ChallengeId
    |> required "value" decodeUuid

decodeHashtags: Decoder (List Hashtag)
decodeHashtags = Decoder.list decodeHashtag

decodeHashtag: Decoder Hashtag
decodeHashtag = succeed Hashtag
    |> required "value" string


-- Post contents

--type PostContent = Repost PostId
--    | EventPost EventId
--    | ChallengePost ChallengeId
--    | TipPost TipId
--    | PollPost PollId
--    | FreeTextPost String (List Source)
decodePostContent: Decoder PostContent
decodePostContent = Decoder.oneOf [
        decodeFreeTextPostContent,
        decodeTipPostContent,
        decodeChallengePostContent,
        decodeRePostContent
  ]

decodeFreeTextPostContent: Decoder PostContent
decodeFreeTextPostContent =  succeed FreeTextPost
    |> required "content" string
    |> required "sources" decodeSources

decodeTipPostContent: Decoder PostContent
decodeTipPostContent = succeed TipPost
    |> required "tip" decodeTipId

decodeChallengePostContent: Decoder PostContent
decodeChallengePostContent = succeed ChallengePost
    |> required "challenge" decodeChallengeId

decodeRePostContent: Decoder PostContent
decodeRePostContent = succeed RePost
    |> required "originalId" decodePostId