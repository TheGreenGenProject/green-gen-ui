module Query.Feed exposing (fetchFeed, hasNewPosts, scheduleFeedCheck, generateInitialFeed)

import Data.Page as Page exposing (Page(..))
import Data.Post as Post exposing (PostId)
import Data.User as UserId
import Data.Feed exposing (Feed, postIds)
import Http
import Json.Decode exposing (bool)
import Query.CacheQueryUtils exposing (fetchFromIdAndCacheAll)
import Query.Json.FeedDecoder exposing (decodeFeed)
import Query.QueryUtils exposing (authHeader, baseUrl)
import Query.TaskUtils exposing (delay, thread)
import State.Cache exposing (Cache)
import State.FeedState as FeedState exposing (FeedState)
import State.UserState exposing (UserInfo)
import Task exposing (Task)
import Update.Msg exposing (Msg(..))
import Url.Builder exposing (absolute)
import Query.Json.DecoderUtils exposing (jsonResolver, unitDecoder)


fetchFeed: Cache -> UserInfo -> Page -> Cmd Msg
fetchFeed cache user page =  fetchFeedPage user page
    |> Task.andThen (\feed -> fetchFromIdAndCacheAll cache user (postIds feed) |> thread feed)
    |> Task.attempt HttpFeedFetched

fetchFeedPage: UserInfo -> Page -> Task Http.Error Feed
fetchFeedPage user page = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute [
        "feed",
        user.id |> UserId.toString,
        page |> Page.number |> String.fromInt] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| decodeFeed page
    , timeout = Nothing
  }

generateInitialFeed: UserInfo -> Cmd Msg
generateInitialFeed user = Http.task {
    method = "POST"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["post","new","feed"] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| unitDecoder
    , timeout = Nothing
 } |> Task.attempt (\_ -> RefreshFeed)

hasNewPosts: UserInfo -> FeedState -> Cmd Msg
hasNewPosts user state = let url = case (FeedState.lastPost state) of
                                         Just postId -> baseUrl ++ absolute ["feed", "has-new-posts", postId |> Post.toString] []
                                         Nothing     -> baseUrl ++ absolute ["feed", "has-posts"] []
    in Http.task {
        method = "GET"
        , headers = [authHeader user]
        , url = url
        , body = Http.emptyBody
        , resolver = jsonResolver <| bool
        , timeout = Nothing
      } |> Task.attempt HttpFeedChecked

scheduleFeedCheck: Float -> Cmd Msg
scheduleFeedCheck time = delay time CheckFeed