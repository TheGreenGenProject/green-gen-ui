module State.EventState exposing (..)

import Data.Page as Page exposing (Page)
import Data.Post exposing (PostId)
import State.PageCache as PageCache
import State.PostPageCache as PostPageCache exposing (PostPage, PostPageCache)
import Utils.MaybeUtils as MaybeUtils


type EventTab = IncomingEventTab
    | ParticipationAcceptedEventTab
    | ParticipatedEventTab
    | RequestedEventTab
    | CancelledEventTab
    | OrganizedEventTab

type alias EventPagedTab = {
    tab: EventTab
    , page: Page
 }

type alias EventState = {
    currentTab: EventTab,
    currentPage: Page,
    postCache: PostPageCache
 }

empty: EventState
empty = {
    currentTab = IncomingEventTab,
    currentPage = Page.first,
    postCache = PageCache.empty
 }

from: List PostId -> EventPagedTab -> EventState -> EventState
from posts pagedTab state = {
    currentTab = pagedTab.tab,
    currentPage = pagedTab.page,
    postCache = state.postCache
        |> PostPageCache.add { number = pagedTab.page, items = posts }
        |> PageCache.loading  pagedTab.page
 }

changeTab: EventTab -> EventState -> EventState
changeTab tab state = {state|
    currentTab = tab,
    currentPage = Page.first,
    postCache = PageCache.empty
 }

allUpToCurrentPage: EventState -> Maybe PostPage
allUpToCurrentPage state = state.postCache
    |> PostPageCache.getAllUpTo state.currentPage

isLoadingMore: EventState -> Bool
isLoadingMore state = state.postCache.loading
    |> MaybeUtils.nonEmpty

noMoreDataToLoad: EventState -> Bool
noMoreDataToLoad state = state.postCache.noMoreData

moveToPage: EventState -> Page -> EventState
moveToPage state page =
    if state.postCache.noMoreData then state
    else if state.currentPage == page then state
    else if isLoadingMore state then state
    else { state|
        currentPage = page,
        postCache = state.postCache |> PageCache.loading page
 }