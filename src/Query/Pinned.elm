module Query.Pinned exposing (pin, unpin, fetchPinnedPosts)

import Data.Page as Page exposing (Page)
import Data.Pinned exposing (Pinned(..))
import Data.Post as Post exposing (PinnedPost(..), PostId)
import Http
import Query.AggregatedCacheQueryUtils exposing (fetchAggregatedAndCacheAll)
import Query.Json.DecoderUtils exposing (jsonResolver, unitDecoder)
import Query.Json.PinnedPost exposing (decodePinnedPosts)
import Query.QueryUtils exposing (authHeader, baseUrl)
import Query.TaskUtils exposing (thread)
import State.Cache exposing (Cache, addAllPostPinned)
import State.PinnedState exposing (postId)
import State.UserState exposing (UserInfo)
import Task exposing (Task)
import Update.Msg exposing (Msg(..))
import Url.Builder exposing (absolute, string)


pin: UserInfo -> PostId -> Cmd Msg
pin user postId = Debug.log ("Pinning post " ++ (Post.toString postId))
    Http.task {
        method     = "POST"
        , headers  = [authHeader user]
        , url      = baseUrl ++ absolute ["pin", "add"] [string "post-id" (Post.toString postId)]
        , body     = Http.emptyBody
        , resolver = jsonResolver <| unitDecoder
        , timeout  = Nothing
     } |> Task.attempt HttpPostPinned

unpin: UserInfo -> PostId -> Cmd Msg
unpin user postId = Debug.log ("Unpinning post " ++ (Post.toString postId))
    Http.task {
        method     = "POST"
        , headers  = [authHeader user]
        , url      = baseUrl ++ absolute ["pin", "remove"] [string "post-id" (Post.toString postId)]
        , body     = Http.emptyBody
        , resolver = jsonResolver <| unitDecoder
        , timeout  = Nothing
     } |> Task.attempt HttpPostUnpinned

fetchPinnedPosts: Cache -> UserInfo -> Page -> Cmd Msg
fetchPinnedPosts cache user page = Debug.log "Fetching pinned posts"
    fetchPinnedPostList user page
    |> Task.andThen (\pinned           -> fetchAggregatedAndCacheAll cache user (List.map postId pinned) |> thread pinned)
    |> Task.andThen (\(cache1, pinned) -> Task.succeed (addAllPostPinned cache1 pinned, Pinned page pinned))
    |> Task.attempt HttpPinnedPostsFetched

fetchPinnedPostList: UserInfo -> Page -> Task Http.Error (List PinnedPost)
fetchPinnedPostList user page = Debug.log "Pinned posts"
    Http.task {
        method     = "GET"
        , headers  = [authHeader user]
        , url      = baseUrl ++ absolute ["pin", "pinned", page |> Page.number |> String.fromInt] []
        , body     = Http.emptyBody
        , resolver = jsonResolver <| decodePinnedPosts
        , timeout  = Nothing
     }