module Data.Post exposing (..)

import Data.Hashtag exposing (Hashtag(..))
import Uuid exposing (Uuid)
import Data.Challenge exposing (ChallengeId)
import Data.Event exposing (EventId)
import Data.Poll exposing (PollId)
import Data.Tip exposing (TipId)
import Data.User exposing (UserId)
import Data.Url exposing (Url)
import Data.Schedule exposing (UTCTimestamp)

type PostId = PostId Uuid

type PinnedPost = PinnedPost PostId UTCTimestamp

type Source = MySelf
    | Web Url
    | PostReference PostId
    | AcademicReference String

type PostContent = RePost PostId
    | EventPost EventId
    | ChallengePost ChallengeId
    | TipPost TipId
    | PollPost PollId
    | FreeTextPost String (List Source)

type alias Post = {
    id: PostId,
    author: UserId,
    content: PostContent,
    created: UTCTimestamp,
    hashtags: List Hashtag
  }

toString: PostId -> String
toString (PostId uuid) = uuid |> Uuid.toString

fromString: String -> Maybe PostId
fromString = Maybe.map PostId << Uuid.fromString