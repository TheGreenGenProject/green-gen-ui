module State.SearchState exposing (..)

import Array
import Data.Hashtag exposing (Hashtag(..))
import Data.Page as Page exposing (Page)
import Data.Post exposing (Post, PostId)
import Data.User exposing (UserId)
import State.PostPage as PostPage exposing (PostPage)

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
    results: List PostPage
  }

empty: SearchState
empty = {
    field = "",
    filter = EmptySearch,
    history = [],
    currentPage = Page.first,
    results = []
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
    results = []
  }

-- Create a ready search from hashtagss
fromHashtags: SearchState -> List Hashtag -> SearchState
fromHashtags state hashtags = {state|
    field = "",
    filter = ByHashtag hashtags,
    history = state.filter :: state.history,
    currentPage = Page.first,
    results = []
   }

-- Create a ready search from a user pseudo
fromUserId: SearchState -> UserId -> SearchState
fromUserId state userId = {state|
    field = "",
    filter = ByAuthor userId,
    history = state.filter :: state.history,
    currentPage = Page.first,
    results = []
   }

-- Applies the result of the search
withResults: SearchState -> SearchResult -> SearchState
withResults state (SearchResult page ps) = {state|
    currentPage = page,
    results = [PostPage page ps]
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
hasResult state = state.results
    |> List.map (\pp -> List.length pp.posts)
    |> List.sum
    |> (<) 0

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
    results = []
  }

currentPage: SearchState -> Maybe PostPage
currentPage state = state.results
    |> Array.fromList
    |> Array.get ((Page.number state.currentPage) - 1)

firstPage: SearchState -> Maybe PostPage
firstPage state = state.results |> List.head

moveToPage: SearchState -> Page -> SearchState
moveToPage state page = {state| currentPage = page }

-- FIXME
hasMorePost: SearchState -> Bool
hasMorePost state = state.results
    |> List.head
    |> Maybe.map PostPage.isLast
    |> Maybe.withDefault False
