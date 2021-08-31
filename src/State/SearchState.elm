module State.SearchState exposing (..)

import Data.Hashtag exposing (Hashtag(..))
import Data.Page as Page exposing (Page)
import Data.Post exposing (Post, PostId)
import Data.User exposing (UserId)
import State.PageCache as PageCache
import State.PostPageCache as PostPageCache exposing (PostPage, PostPageCache)
import State.UserPageCache as UserPageCache exposing (UserPage, UserPageCache)
import Utils.MaybeUtils as MaybeUtils

type SearchResult a = SearchResult Page (List a)
type alias PostSearchResult = SearchResult PostId
type alias UserSearchResult = SearchResult UserId

type PostType = AllPostTypes
    | PollPosts
    | ChallengePosts
    | EventPosts
    | TipPosts
    | FreeTextPosts

type SearchFilter =
    EmptySearch
    | ByHashtag (List Hashtag) PostType
    | ByAuthor UserId PostType
    | ByUserPrefix String

type alias SearchState = {
    field: String,
    filter: SearchFilter,
    history: List SearchFilter,
    currentPage: Page,
    postCache: PostPageCache,
    userCache: UserPageCache
  }

empty: SearchState
empty = {
    field = "",
    filter = EmptySearch,
    history = [],
    currentPage = Page.first,
    postCache = PageCache.empty,
    userCache = PageCache.empty
  }

isPostSearchFilter: SearchFilter -> Bool
isPostSearchFilter filter = case filter of
    ByUserPrefix _ -> False
    _              -> True

isUserSearchFilter: SearchFilter -> Bool
isUserSearchFilter filter = case filter of
    ByUserPrefix _ -> True
    _              -> False

postTypeFilter: SearchFilter -> Maybe PostType
postTypeFilter filter = case filter of
    ByHashtag _ pt -> Just pt
    ByAuthor _ pt  -> Just pt
    _              -> Nothing

allPostTypes: List PostType
allPostTypes = [
    AllPostTypes
    , TipPosts
    , ChallengePosts
    , EventPosts
    , PollPosts
    , FreeTextPosts]

changePostTypeFilter: SearchState -> PostType -> SearchState
changePostTypeFilter state postType =
    let newFilter = case state.filter of
                    ByHashtag hashtags _ -> ByHashtag hashtags postType
                    ByAuthor userId _    -> ByAuthor userId postType
                    unchanged            -> unchanged
    in {state | filter = newFilter,
        currentPage = Page.first,
        postCache = PageCache.empty,
        userCache = PageCache.empty}

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
    postCache = PageCache.empty,
    userCache = PageCache.empty
  }

-- Create a ready search from hashtagss
fromHashtags: SearchState -> List Hashtag -> SearchState
fromHashtags state hashtags = {state|
    field = "",
    filter = ByHashtag hashtags AllPostTypes,
    history = state.filter :: state.history,
    currentPage = Page.first,
    postCache = PageCache.empty,
    userCache = PageCache.empty
   }

-- Create a ready search from a user pseudo
fromUserId: SearchState -> UserId -> SearchState
fromUserId state userId = {state|
    field = "",
    filter = ByAuthor userId AllPostTypes,
    history = state.filter :: state.history,
    currentPage = Page.first,
    postCache = PageCache.empty,
    userCache = PageCache.empty
   }

-- Create a ready search from a user pseudo prefix
fromUserPseudoPrefix: SearchState -> String -> SearchState
fromUserPseudoPrefix state prefix = {state|
    field = "",
    filter = ByUserPrefix prefix,
    history = state.filter :: state.history,
    currentPage = Page.first,
    postCache = PageCache.empty,
    userCache = PageCache.empty
   }

-- Applies the result of the search
withPostResults: SearchState -> PostSearchResult -> SearchState
withPostResults state (SearchResult page ps) = {state|
    currentPage = page,
    postCache = state.postCache
        |> PostPageCache.add {number = page, items = ps}
        |> PageCache.loading page,
    userCache = PageCache.empty
  }

withUserResults: SearchState -> UserSearchResult -> SearchState
withUserResults state (SearchResult page ps) = {state|
    currentPage = page,
    postCache = PageCache.empty,
    userCache = state.userCache
        |> UserPageCache.add {number = page, items = ps}
        |> PageCache.loading page
  }

-- Create a search filter from the content of the String
toSearchFilter: String -> SearchFilter
toSearchFilter str =
    if String.trim str == ""
    then EmptySearch
    else if isOneUserPseudoPrefix str
    then str |> String.trim
             |> String.dropLeft 1
             |> ByUserPrefix
    else str |> String.split " "
             |> List.filter (not << String.isEmpty << String.trim)
             |> List.map Hashtag
             |> (\x -> ByHashtag x AllPostTypes)

isOneUserPseudoPrefix: String -> Bool
isOneUserPseudoPrefix str =
    (str |> String.trim |> String.startsWith "@") && ((str |> String.split " " |> List.length) == 1)



-- Check if the last performed search has some results
hasResult: SearchState -> Bool
hasResult state = PageCache.nonEmpty state.postCache

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
    postCache = PageCache.empty,
    userCache = PageCache.empty
  }

currentPostPage: SearchState -> Maybe PostPage
currentPostPage state = state.postCache
    |> PageCache.get state.currentPage

allUpToCurrentPostPage: SearchState -> Maybe PostPage
allUpToCurrentPostPage state = state.postCache
    |> PostPageCache.getAllUpTo state.currentPage

isLoadingMorePost: SearchState -> Bool
isLoadingMorePost state = state.postCache.loading |> MaybeUtils.nonEmpty

noMorePostToLoad: SearchState -> Bool
noMorePostToLoad state = state.postCache.noMoreData

allUpToCurrentUserPage: SearchState -> Maybe UserPage
allUpToCurrentUserPage state = state.userCache
    |> UserPageCache.getAllUpTo state.currentPage

isLoadingMoreUser: SearchState -> Bool
isLoadingMoreUser state = state.userCache.loading |> MaybeUtils.nonEmpty

noMoreUserToLoad: SearchState -> Bool
noMoreUserToLoad state = state.userCache.noMoreData

moveToPage: SearchState -> Page -> SearchState
moveToPage state page = {state| currentPage = page }

