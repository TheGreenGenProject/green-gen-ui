module Query.Notification exposing (fetchNotifications, markAsRead, hasUnreadNotifications, scheduleNotificationCheck)

import Data.Notification as Notification exposing (Notification, NotificationId)
import Data.Page as Page exposing (Page)
import Http
import Json.Decode exposing (bool)
import Query.CacheQueryUtils exposing (fetchAndCacheAllUsersFromNotifications)
import Query.Json.DecoderUtils exposing (jsonResolver, unitDecoder)
import Query.Json.NotificationDecoder exposing (decodeUnreadNotifications)
import Query.QueryUtils exposing (authHeader, baseUrl)
import Query.TaskUtils exposing (delay, thread)
import State.Cache exposing (Cache)
import State.UserState exposing (UserInfo)
import Task exposing (Task)
import Update.Msg exposing (Msg(..))
import Url.Builder exposing (absolute, string)


fetchNotifications: Cache -> UserInfo -> Page -> Cmd Msg
fetchNotifications cache user page = fetchUnreadNotifications user page
    |> Task.andThen (\notifs -> fetchAndCacheAllUsersFromNotifications cache user notifs |> thread notifs)
    |> Task.attempt HttpUnreadNotificationsFetched

fetchUnreadNotifications: UserInfo -> Page -> Task Http.Error (List Notification)
fetchUnreadNotifications user page = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["notification","all","unread", page |> Page.number |> String.fromInt] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| decodeUnreadNotifications
    , timeout = Nothing
  }

hasUnreadNotifications: UserInfo -> Cmd Msg
hasUnreadNotifications user = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["notification", "has-some"] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| bool
    , timeout = Nothing
  } |> Task.attempt HttpNotificationsChecked

markAsRead: UserInfo -> NotificationId -> Cmd Msg
markAsRead user notifId = Http.task {
    method = "POST"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["notification","read"] [string "notif-id"  (notifId |> Notification.toString)]
    , body = Http.emptyBody
    , resolver = jsonResolver <| unitDecoder
    , timeout = Nothing
  } |> Task.attempt HttpMarkNotificationAsRead

scheduleNotificationCheck: Float -> Cmd Msg
scheduleNotificationCheck time = delay time CheckNotifications