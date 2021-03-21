module Query.Wall exposing (fetchWall, fetchUserWall)

import Data.Page as Page exposing (Page(..))
import Data.User as UserId exposing (UserId)
import Data.Wall exposing (Wall, postIds)
import Http
import Query.CacheQueryUtils exposing (fetchAndCacheScoreBreakdown, fetchFromIdAndCacheAll)
import Query.Json.WallDecoder exposing (decodeWall)
import Query.QueryUtils exposing (authHeader, baseUrl)
import Query.TaskUtils exposing (thread)
import State.Cache exposing (Cache)
import State.UserState exposing (UserInfo)
import Task exposing (Task)
import Update.Msg exposing (Msg(..))
import Url.Builder exposing (absolute)
import Query.Json.DecoderUtils exposing (jsonResolver)


fetchWall: Cache -> UserInfo -> Page -> Cmd Msg
fetchWall cache user page = fetchUserWall cache user user.id page

fetchUserWall: Cache -> UserInfo -> UserId -> Page -> Cmd Msg
fetchUserWall cache user targetId page =  fetchWallPage user targetId page
    |> Task.andThen (\wall -> fetchFromIdAndCacheAll cache user (postIds wall)           |> thread wall)
    |> Task.andThen (\(cache1, wall) -> fetchAndCacheScoreBreakdown cache1 user targetId |> thread wall)
    |> Task.attempt HttpWallFetched

fetchWallPage: UserInfo -> UserId -> Page -> Task Http.Error Wall
fetchWallPage user targetId page = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute [
        "wall",
        targetId |> UserId.toString,
        page |> Page.number |> String.fromInt] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| decodeWall targetId page
    , timeout = Nothing
 }




