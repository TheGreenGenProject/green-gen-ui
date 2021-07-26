module State.FeedState exposing (..)

import Data.Feed exposing (Feed(..))
import Data.Page as Page exposing (Page)
import Data.Post exposing (PostId)
import State.PostPage exposing (PostPage)
import State.PostPageCache as PostPageCache exposing (PostPageCache)
import Utils.MaybeUtils as MaybeUtils

type alias FeedState = {
    newPostsAvailable: Bool,
    currentPage: Page,
    postCache: PostPageCache
 }

empty: FeedState
empty = {
    newPostsAvailable = False,
    currentPage = Page.first,
    postCache = PostPageCache.empty
 }

refresh: FeedState
refresh = empty

from: FeedState -> Feed -> FeedState
from state (Feed page posts) = {
    newPostsAvailable = False,
    currentPage = page,
    postCache = state.postCache
        |> PostPageCache.add { number = page, posts = posts }
        |> PostPageCache.loading page
 }

allUpToCurrentPage: FeedState -> Maybe PostPage
allUpToCurrentPage state = state.postCache
    |> PostPageCache.getAllUpTo state.currentPage

isLoadingMore: FeedState -> Bool
isLoadingMore state = state.postCache.loading
    |> MaybeUtils.nonEmpty

noMoreDataToLoad: FeedState -> Bool
noMoreDataToLoad state = state.postCache.noMoreData

moveToPage: FeedState -> Page -> FeedState
moveToPage state page =
    if state.postCache.noMoreData then state
    else if state.currentPage == page then state
    else if isLoadingMore state then state
    else { state|
        currentPage = page,
        postCache = state.postCache |> PostPageCache.loading page
     }

lastPost: FeedState -> Maybe PostId
lastPost state = state.postCache
    |> PostPageCache.get Page.first
    |> Maybe.map (\x -> x.posts)
    |> Maybe.andThen (List.head)

updateNewPostsAvailable: FeedState -> Bool -> FeedState
updateNewPostsAvailable state status = {state|
    newPostsAvailable = status
 }