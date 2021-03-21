module State.PinnedState exposing (..)

import Data.Page as Page exposing (Page)
import Data.Post exposing (PinnedPost(..), Post, PostId)
import State.Cache as Cache exposing (Cache)
import State.PostPage as PostPage exposing (PostPage)


type alias PinnedState = {
    currentPage: Page,
    posts: Maybe PostPage
 }

empty: PinnedState
empty = { currentPage = Page.first, posts = Nothing }

from: List PinnedPost -> PinnedState
from pinned = let ids   = List.map postId pinned in
    {
        currentPage = Page.first,
        posts = { number = Page.first, posts = ids } |> Just
    }

isEmpty: PinnedState -> Bool
isEmpty state = state.currentPage == Page.first &&
    (state.posts == Nothing || state.posts == Just { number = Page.first, posts = []})

currentPage: PinnedState -> Maybe PostPage
currentPage state = state.posts

moveToPage: PinnedState -> Page -> PinnedState
moveToPage state page = {state|
    currentPage = page,
    posts = if state.currentPage == page then state.posts else Nothing
  }

postId: PinnedPost -> PostId
postId (PinnedPost id _) = id

hasMorePost: PinnedState -> Bool
hasMorePost state = state.posts
    |> Maybe.map (PostPage.isLast >> not)
    |> Maybe.withDefault False