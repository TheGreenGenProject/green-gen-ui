module Query.Following exposing (..)

import Data.User as User exposing (UserId)
import Http
import Query.Json.DecoderUtils exposing (jsonResolver, unitDecoder)
import Query.QueryUtils exposing (authHeader, baseUrl)
import State.UserState exposing (UserInfo)
import Task
import Update.Msg exposing (Msg(..))
import Url.Builder exposing (absolute, string)


follow: UserInfo -> UserId -> Cmd Msg
follow user userId = Debug.log ("Following user " ++ (User.toString userId))
    Http.task {
        method     = "POST"
        , headers  = [authHeader user]
        , url      = baseUrl ++ absolute ["followers", "add"] [string "followed-id" (User.toString userId)]
        , body     = Http.emptyBody
        , resolver = jsonResolver <| unitDecoder
        , timeout  = Nothing
     } |> Task.attempt HttpUserFollowed

unfollow: UserInfo -> UserId -> Cmd Msg
unfollow user userId = Debug.log ("Unfollowing user " ++ (User.toString userId))
    Http.task {
        method     = "POST"
        , headers  = [authHeader user]
        , url      = baseUrl ++ absolute ["followers", "remove"] [string "followed-id" (User.toString userId)]
        , body     = Http.emptyBody
        , resolver = jsonResolver <| unitDecoder
        , timeout  = Nothing
     } |> Task.attempt HttpUserUnfollowed