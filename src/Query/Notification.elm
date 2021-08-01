module Query.Notification exposing (
    fetchNotifications
    , fetchUnreadNotifications
    , markAsRead
    , hasUnreadNotifications
    , scheduleNotificationCheck)


import Data.Notification as Notification exposing (Notification, NotificationId)
import Data.Page as Page exposing (Page)
import Http
import Json.Decode exposing (bool)
import Query.CacheQueryUtils exposing (fetchAndCacheAllUsersFromNotifications)
import Query.Json.DecoderUtils exposing (jsonResolver, unitDecoder)
import Query.Json.NotificationDecoder exposing (decodeNotifications)
import Query.QueryUtils exposing (authHeader, baseUrl)
import Query.TaskUtils exposing (delay, thread)
import State.Cache exposing (Cache)
import State.NotificationState exposing (NotificationTab(..))
import State.UserState exposing (UserInfo)
import Task exposing (Task)
import Update.Msg exposing (Msg(..))
import Url.Builder exposing (absolute, string)


fetchNotifications: Cache -> UserInfo -> NotificationTab -> Page -> Cmd Msg
fetchNotifications cache user tab page = case tab of
    UnreadTab -> fetchUnreadNotifications cache user page
    AllTab    -> fetchAllNotifications cache user page

fetchAllNotifications: Cache -> UserInfo -> Page -> Cmd Msg
fetchAllNotifications cache user page = (fetchAllNotificationsPage user page)
    |> Task.andThen (\notifs -> fetchAndCacheAllUsersFromNotifications cache user notifs |> thread {number=page, notifications=notifs})
    |> Task.attempt HttpNotificationsFetched

fetchAllNotificationsPage: UserInfo -> Page -> Task Http.Error (List Notification)
fetchAllNotificationsPage user page = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["notification","all", page |> Page.number |> String.fromInt] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| decodeNotifications
    , timeout = Nothing
  }

fetchUnreadNotifications: Cache -> UserInfo -> Page -> Cmd Msg
fetchUnreadNotifications cache user page = fetchUnreadNotificationsPage user page
    |> Task.andThen (\notifs -> fetchAndCacheAllUsersFromNotifications cache user notifs |> thread {number=page, notifications=notifs})
    |> Task.attempt HttpNotificationsFetched

fetchUnreadNotificationsPage: UserInfo -> Page -> Task Http.Error (List Notification)
fetchUnreadNotificationsPage user page = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["notification","all","unread", page |> Page.number |> String.fromInt] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| decodeNotifications
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