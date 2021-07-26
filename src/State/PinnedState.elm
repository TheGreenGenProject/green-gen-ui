module State.PinnedState exposing (..)

import Data.Page as Page exposing (Page)
import Data.Pinned as Pinned exposing (Pinned(..))
import Data.Post exposing (PinnedPost(..), Post, PostId)
import State.PostPage exposing (PostPage)
import State.PostPageCache as PostPageCache exposing (PostPageCache)
import Utils.MaybeUtils as MaybeUtils


type alias PinnedState = {
    currentPage: Page,
    postCache: PostPageCache
 }

empty: PinnedState
empty = {
    currentPage = Page.first,
    postCache = PostPageCache.empty
 }

refresh: PinnedState
refresh = empty

from: PinnedState -> Pinned -> PinnedState
from state (Pinned page posts) = {
    currentPage = page,
    postCache = state.postCache
        |> PostPageCache.add { number = page, posts = (Pinned page posts) |> Pinned.postIds }
        |> PostPageCache.loading page
 }

isEmpty: PinnedState -> Bool
isEmpty state = state.postCache
    |> PostPageCache.isEmpty

moveToPage: PinnedState -> Page -> PinnedState
moveToPage state page =
    if state.postCache.noMoreData then state
    else if state.currentPage == page then state
    else if isLoadingMore state then state
    else { state|
        currentPage = page,
        postCache = state.postCache |> PostPageCache.loading page
     }

allUpToCurrentPage: PinnedState -> Maybe PostPage
allUpToCurrentPage state = state.postCache
    |> PostPageCache.getAllUpTo state.currentPage

isLoadingMore: PinnedState -> Bool
isLoadingMore state = state.postCache.loading
    |> MaybeUtils.nonEmpty

noMoreDataToLoad: PinnedState -> Bool
noMoreDataToLoad state = state.postCache.noMoreData

postId: PinnedPost -> PostId
postId (PinnedPost id _) = id