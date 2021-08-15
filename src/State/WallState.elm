module State.WallState exposing (..)

import Data.Page as Page exposing (Page)
import Data.User exposing (UserId)
import Data.Wall exposing (Wall(..))
import State.PageCache as PageCache
import State.PostPageCache as PostPageCache exposing (PostPage, PostPageCache)
import Utils.MaybeUtils as MaybeUtils


type alias WallState = {
    user: Maybe UserId,
    currentPage: Page,
    postCache: PostPageCache
 }

empty: WallState
empty = {
    user = Nothing,
    currentPage = Page.first,
    postCache = PageCache.empty
 }

refresh: WallState
refresh = empty

from: WallState -> Wall -> WallState
from state (Wall user page posts) = {
    user = Just user,
    currentPage = page,
    postCache = state.postCache
        |> PostPageCache.add { number = page, items = posts }
        |> PageCache.loading page
 }

allUpToCurrentPage: WallState -> Maybe PostPage
allUpToCurrentPage state = state.postCache
    |> PostPageCache.getAllUpTo state.currentPage

isLoadingMore: WallState -> Bool
isLoadingMore state = state.postCache.loading
    |> MaybeUtils.nonEmpty

noMoreDataToLoad: WallState -> Bool
noMoreDataToLoad state = state.postCache.noMoreData

moveToPage: WallState -> Page -> WallState
moveToPage state page =
    if state.postCache.noMoreData then state
    else if state.currentPage == page then state
    else if isLoadingMore state then state
    else { state|
        currentPage = page,
        postCache = state.postCache |> PageCache.loading page
 }
