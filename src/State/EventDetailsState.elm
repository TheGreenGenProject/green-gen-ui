module State.EventDetailsState exposing (..)

import Data.Page as Page exposing (Page)
import Data.User exposing (UserId)
import State.PageCache as PageCache
import State.UserPageCache as UserPageCache exposing (UserPage, UserPageCache)
import Utils.MaybeUtils as MaybeUtils


type EventDetailsTab =
    EventDetailsTab
    | PendingRequestsTab
    | ParticipantsTab
    | EventDiscussionTab

type alias EventDetailsPagedTab = {
    tab: EventDetailsTab
    , page: Page
 }

type alias EventDetailsState = {
    currentTab: EventDetailsTab,
    currentPage: Page,
    userCache: UserPageCache
 }

empty: EventDetailsState
empty = {
    currentTab = EventDetailsTab,
    currentPage = Page.first,
    userCache = PageCache.empty
 }

from: List UserId -> EventDetailsPagedTab -> EventDetailsState -> EventDetailsState
from users pagedTab state = {
    currentTab = pagedTab.tab,
    currentPage = pagedTab.page,
    userCache = state.userCache
        |> UserPageCache.add { number = pagedTab.page, items = users }
        |> PageCache.loading  pagedTab.page
 }

changeTab: EventDetailsTab -> EventDetailsState -> EventDetailsState
changeTab tab state = {state|
    currentTab = tab,
    currentPage = Page.first,
    userCache = PageCache.empty
 }

allUpToCurrentPage: EventDetailsState -> Maybe UserPage
allUpToCurrentPage state = state.userCache
    |> UserPageCache.getAllUpTo state.currentPage

isLoadingMore: EventDetailsState -> Bool
isLoadingMore state = state.userCache.loading
    |> MaybeUtils.nonEmpty

noMoreDataToLoad: EventDetailsState -> Bool
noMoreDataToLoad state = state.userCache.noMoreData

moveToPage: EventDetailsState -> Page -> EventDetailsState
moveToPage state page =
    if state.userCache.noMoreData then state
    else if state.currentPage == page then state
    else if isLoadingMore state then state
    else { state|
        currentPage = page,
        userCache = state.userCache |> PageCache.loading page
 }