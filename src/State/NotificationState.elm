module State.NotificationState exposing (
    NotificationState
    , NotificationPage
    , NotificationTab(..)
    , empty
    , refresh
    , changeTab
    , from
    , markAsRead
    , updateUnreadStatus
    , moveToPage
    , hasUnreadNotifications
    , hasNotifications
    , isLoadingMore
    , noMoreDataToLoad
    , getAllUpTo)

import Data.Notification exposing (Notification, NotificationId, Status(..))
import Data.Page as Page exposing (Page(..))
import Dict
import State.PageCache as PageCache exposing (PageCache)
import Utils.ListUtils as ListUtils
import Utils.MaybeUtils as MaybeUtils


type NotificationTab = UnreadTab | AllTab

type alias NotificationState = {
    currentTab: NotificationTab,
    unread: Bool,
    currentPage: Page,
    latest: NotificationPageCache
 }

empty: NotificationState
empty = {
    currentTab = UnreadTab,
    unread = False,
    currentPage = Page.first,
    latest = PageCache.empty
 }

refresh: NotificationState
refresh = empty

changeTab: NotificationTab -> NotificationState -> NotificationState
changeTab tab state = {state|
    currentTab = tab,
    unread = False,
    currentPage = Page.first,
    latest = PageCache.empty
 }

from: Page -> List Notification -> NotificationState -> NotificationState
from page notifs state = {
    currentTab = state.currentTab,
    unread = state.unread || (notifs |> List.any (\notif -> notif.status==Unread)),
    currentPage = page,
    latest = PageCache.add {number=page, notifications=notifs} page state.latest
        |> PageCache.loading page
 }

markAsRead: NotificationState -> NotificationId -> NotificationState
markAsRead state id = {state|
    latest = markNotificationAsRead id state.latest
 } |> refreshUnreadStatus


updateUnreadStatus: NotificationState -> Bool -> NotificationState
updateUnreadStatus state status = {state|
    unread = status
 }

refreshUnreadStatus: NotificationState -> NotificationState
refreshUnreadStatus state  = {state|
    unread =  state.latest |> PageCache.isEmpty |> not
 }

moveToPage: NotificationState -> Page -> NotificationState
moveToPage state page =
    if state.latest.noMoreData then state
    else if state.currentPage == page then state
    else if isLoadingMore state then state
    else { state|
        currentPage = page,
        latest = state.latest |> PageCache.loading page
 }

hasNotifications: NotificationState -> Bool
hasNotifications state = state.latest
    |> PageCache.pageContains (\p -> p.notifications |> List.isEmpty |> not)

hasUnreadNotifications: NotificationState -> Bool
hasUnreadNotifications state = state.latest
    |> PageCache.pageContains (\p -> p.notifications |> List.any (\n -> n.status == Unread))

isLoadingMore: NotificationState -> Bool
isLoadingMore state = state.latest.loading
    |> MaybeUtils.nonEmpty

noMoreDataToLoad: NotificationState -> Bool
noMoreDataToLoad state = state.latest.noMoreData


-- Notification cache

type alias NotificationPage = {
    number: Page,
    notifications: List Notification
 }

type alias NotificationPageCache = PageCache NotificationPage

getAllUpTo: Page -> NotificationPageCache -> Maybe NotificationPage
getAllUpTo page cache =
    let sortedKeys = cache.cache
            |> Dict.keys |> List.sort
            |> ListUtils.takeWhile (\key -> key <= (Page.number page))
        allNotifs = sortedKeys
            |> List.concatMap (\p -> (PageCache.get (Page p) cache)
            |> Maybe.map (.notifications)
            |> Maybe.withDefault [])
    in if List.isEmpty allNotifs
        then Nothing
        else { number = page, notifications = allNotifs } |> Just

markNotificationAsRead: NotificationId -> NotificationPageCache -> NotificationPageCache
markNotificationAsRead id cache =
    let markNotificationInPage: NotificationId -> Status -> NotificationPage -> NotificationPage
        markNotificationInPage nid status page = {page|
            notifications = page.notifications |> List.map (\notif -> if notif.id /= nid then notif else {notif| status = status}) }
    in cache |> PageCache.map (markNotificationInPage id Read)