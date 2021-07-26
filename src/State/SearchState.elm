module State.SearchState exposing (..)

import Data.Hashtag exposing (Hashtag(..))
import Data.Page as Page exposing (Page)
import Data.Post exposing (Post, PostId)
import Data.User exposing (UserId)
import State.PostPage exposing (PostPage)
import State.PostPageCache as PostPageCache exposing (PostPageCache)
import Utils.MaybeUtils as MaybeUtils

type SearchResult = SearchResult Page (List PostId)

type SearchFilter =
    EmptySearch
    | ByHashtag (List Hashtag)
    | ByAuthor UserId

type alias SearchState = {
    field: String,
    filter: SearchFilter,
    history: List SearchFilter,
    currentPage: Page,
    postCache: PostPageCache
  }

empty: SearchState
empty = {
    field = "",
    filter = EmptySearch,
    history = [],
    currentPage = Page.first,
    postCache = PostPageCache.empty
  }

-- Input a value in the search field
input: SearchState -> String -> SearchState
input state txt = {state| field = txt}

-- Applies a search, ie transform search input parameters into an actual SearchFilter
applyInput: SearchState -> SearchState
applyInput state = {state|
    field = "",
    filter = toSearchFilter state.field,
    history = state.filter :: state.history,
    currentPage = Page.first,
    postCache = PostPageCache.empty
  }

-- Create a ready search from hashtagss
fromHashtags: SearchState -> List Hashtag -> SearchState
fromHashtags state hashtags = {state|
    field = "",
    filter = ByHashtag hashtags,
    history = state.filter :: state.history,
    currentPage = Page.first,
    postCache = PostPageCache.empty
   }

-- Create a ready search from a user pseudo
fromUserId: SearchState -> UserId -> SearchState
fromUserId state userId = {state|
    field = "",
    filter = ByAuthor userId,
    history = state.filter :: state.history,
    currentPage = Page.first,
    postCache = PostPageCache.empty
   }

-- Applies the result of the search
withResults: SearchState -> SearchResult -> SearchState
withResults state (SearchResult page ps) = {state|
    currentPage = page,
    postCache = state.postCache
        |> PostPageCache.add (PostPage page ps)
        |> PostPageCache.loading page
  }

-- Create a search filter from the content of the String
toSearchFilter: String -> SearchFilter
toSearchFilter str = if String.trim str == ""
    then EmptySearch
    else str |> String.split " "
             |> List.filter (not << String.isEmpty << String.trim)
             |> List.map Hashtag
             |> ByHashtag

-- Check if the last performed search has some results
hasResult: SearchState -> Bool
hasResult state = PostPageCache.nonEmpty state.postCache

clearSearch: SearchState -> SearchState
clearSearch = clearFilter << clearResults

clearFilter: SearchState -> SearchState
clearFilter state = {state|
    field = "",
    filter = EmptySearch
  }

clearHistory: SearchState -> SearchState
clearHistory state = {state| history = [] }

clearResults: SearchState -> SearchState
clearResults state = {state|
    field = "",
    currentPage = Page.first,
    postCache = PostPageCache.empty
  }

currentPage: SearchState -> Maybe PostPage
currentPage state = state.postCache
    |> PostPageCache.get state.currentPage

allUpToCurrentPage: SearchState -> Maybe PostPage
allUpToCurrentPage state = state.postCache
    |> PostPageCache.getAllUpTo state.currentPage

isLoadingMore: SearchState -> Bool
isLoadingMore state = state.postCache.loading |> MaybeUtils.nonEmpty

noMoreDataToLoad: SearchState -> Bool
noMoreDataToLoad state = state.postCache.noMoreData

firstPage: SearchState -> Maybe PostPage
firstPage state = state.postCache
    |> PostPageCache.get Page.first

moveToPage: SearchState -> Page -> SearchState
moveToPage state page = {state| currentPage = page }

hasMorePost: SearchState -> Bool
hasMorePost state = not state.postCache.noMoreData
