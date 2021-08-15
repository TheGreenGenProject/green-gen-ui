module State.ChallengeState exposing (..)

import Data.Page as Page exposing (Page)
import Data.Post exposing (PostId)
import State.PageCache as PageCache
import State.PostPageCache as PostPageCache exposing (PostPage, PostPageCache)
import Utils.MaybeUtils as MaybeUtils


type ChallengeTab = OnGoingTab | OnTracksTab | FailedTab | UpcomingTab | ReportDueTab | FinishedTab | AuthoredTab

type alias ChallengePagedTab = {
    tab: ChallengeTab
    , page: Page
 }

type alias ChallengeState = {
    currentTab: ChallengeTab,
    currentPage: Page,
    postCache: PostPageCache
 }

empty: ChallengeState
empty = {
    currentTab = OnGoingTab,
    currentPage = Page.first,
    postCache = PageCache.empty
 }

from: List PostId -> ChallengePagedTab -> ChallengeState -> ChallengeState
from posts pagedTab state = {
    currentTab = pagedTab.tab,
    currentPage = pagedTab.page,
    postCache = state.postCache
        |> PostPageCache.add { number = pagedTab.page, items = posts }
        |> PageCache.loading  pagedTab.page
 }

changeTab: ChallengeTab -> ChallengeState -> ChallengeState
changeTab tab state = {state|
    currentTab = tab ,
    currentPage = Page.first,
    postCache = PageCache.empty
 }

allUpToCurrentPage: ChallengeState -> Maybe PostPage
allUpToCurrentPage state = state.postCache
    |> PostPageCache.getAllUpTo state.currentPage

isLoadingMore: ChallengeState -> Bool
isLoadingMore state = state.postCache.loading
    |> MaybeUtils.nonEmpty

noMoreDataToLoad: ChallengeState -> Bool
noMoreDataToLoad state = state.postCache.noMoreData

moveToPage: ChallengeState -> Page -> ChallengeState
moveToPage state page =
    if state.postCache.noMoreData then state
    else if state.currentPage == page then state
    else if isLoadingMore state then state
    else { state|
        currentPage = page,
        postCache = state.postCache |> PageCache.loading page
 }