module Query.Hashtag exposing (refreshHashtagTrend)

import Data.Hashtag exposing (Hashtag)
import Http
import Json.Decode exposing (int, list)
import Query.Json.DecoderUtils exposing (decodePair, jsonResolver)
import Query.Json.PostDecoder exposing (decodeHashtag)
import Query.QueryUtils exposing (authHeader, baseUrl)
import Query.TaskUtils exposing (thread)
import State.Cache as Cache exposing (Cache)
import State.UserState exposing (UserInfo)
import Task exposing (Task)
import Update.Msg exposing (Msg(..))
import Url.Builder exposing (absolute)


refreshHashtagTrend: UserInfo -> Cmd Msg
refreshHashtagTrend user =
  fetchHashtagTrend user 50
  |> Task.attempt HttpHashtagTrendRefreshed


fetchHashtagTrend: UserInfo -> Int -> Task Http.Error (List (Int, Hashtag))
fetchHashtagTrend user n = Http.task {
    method     = "GET"
    , headers  = [authHeader user]
    , url      = baseUrl ++ absolute ["hashtag", "trend", "by-posts", String.fromInt n] []
    , body     = Http.emptyBody
    , resolver = jsonResolver <| (list (decodePair int decodeHashtag))
    , timeout  = Nothing
  }
