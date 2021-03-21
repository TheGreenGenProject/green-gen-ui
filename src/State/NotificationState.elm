module State.NotificationState exposing (
    NotificationState
    , empty
    , from
    , markAsRead
    , updateUnreadStatus
    , moveToPage
    , hasMoreNotifications)

import Data.Notification exposing (Notification, NotificationId)
import Data.Page as Page exposing (Page)


type alias NotificationState = {
    unread: Bool,
    currentPage: Page,
    latest: List Notification
 }

empty: NotificationState
empty = {
    unread = False,
    currentPage = Page.first,
    latest = []
 }

from: Page -> List Notification -> NotificationState
from page notifs = {
    unread = notifs |> List.isEmpty |> not,
    currentPage = page,
    latest = notifs
 }

markAsRead: NotificationState -> NotificationId -> NotificationState
markAsRead state id = {state|
    latest = List.filter (\x -> x.id == id |> not) state.latest
 } |> refreshUnreadStatus


updateUnreadStatus: NotificationState -> Bool -> NotificationState
updateUnreadStatus state status = {state|
    unread = status
 }

refreshUnreadStatus: NotificationState -> NotificationState
refreshUnreadStatus state  = {state|
    unread = state.latest |> List.isEmpty |> not
 }

moveToPage: NotificationState -> Page -> NotificationState
moveToPage state page = {state|
    currentPage = page,
    latest = if state.currentPage == page then state.latest else []
  }

hasMoreNotifications: NotificationState -> Bool
hasMoreNotifications state = state.latest |> List.isEmpty |> not