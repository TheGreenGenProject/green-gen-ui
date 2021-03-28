module Query.Following exposing (..)

import Data.Hashtag as Hashtag exposing (Hashtag)
import Data.User as User exposing (UserId)
import Http
import Query.Json.DecoderUtils exposing (jsonResolver, unitDecoder)
import Query.QueryUtils exposing (authHeader, baseUrl)
import State.UserState exposing (UserInfo)
import Task
import Update.Msg exposing (Msg(..))
import Url.Builder exposing (absolute, string)


followUser: UserInfo -> UserId -> Cmd Msg
followUser user userId = Debug.log ("Following user " ++ (User.toString userId))
    Http.task {
        method     = "POST"
        , headers  = [authHeader user]
        , url      = baseUrl ++ absolute ["followers", "add"] [string "followed-id" (User.toString userId)]
        , body     = Http.emptyBody
        , resolver = jsonResolver <| unitDecoder
        , timeout  = Nothing
     } |> Task.attempt HttpUserFollowed

unfollowUser: UserInfo -> UserId -> Cmd Msg
unfollowUser user userId = Debug.log ("Unfollowing user " ++ (User.toString userId))
    Http.task {
        method     = "POST"
        , headers  = [authHeader user]
        , url      = baseUrl ++ absolute ["followers", "remove"] [string "followed-id" (User.toString userId)]
        , body     = Http.emptyBody
        , resolver = jsonResolver <| unitDecoder
        , timeout  = Nothing
     } |> Task.attempt HttpUserUnfollowed

followHashtag: UserInfo -> Hashtag -> Cmd Msg
followHashtag user hashtag = Debug.log ("Following hashtag " ++ (Hashtag.format hashtag))
    Http.task {
        method     = "POST"
        , headers  = [authHeader user]
        , url      = baseUrl ++ absolute ["hashtag", "followers", "add", Hashtag.toString hashtag] []
        , body     = Http.emptyBody
        , resolver = jsonResolver <| unitDecoder
        , timeout  = Nothing
     } |> Task.attempt HttpHashtagFollowed

unfollowHashtag: UserInfo -> Hashtag -> Cmd Msg
unfollowHashtag user hashtag = Debug.log ("Unfollowing hashtag " ++ (Hashtag.format hashtag))
    Http.task {
        method     = "POST"
        , headers  = [authHeader user]
        , url      = baseUrl ++ absolute ["hashtag", "followers", "remove", Hashtag.toString hashtag] []
        , body     = Http.emptyBody
        , resolver = jsonResolver <| unitDecoder
        , timeout  = Nothing
     } |> Task.attempt HttpHashtagUnfollowed