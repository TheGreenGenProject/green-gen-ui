module State.FeedState exposing (..)

import Data.Feed exposing (Feed(..))
import Data.Page as Page exposing (Page)
import Data.Post exposing (PostId)
import State.PostPage as PostPage exposing (PostPage)

type alias FeedState = {
    newPostsAvailable: Bool,
    currentPage: Page,
    posts: Maybe PostPage
 }

empty: FeedState
empty = {
    newPostsAvailable = False,
    currentPage = Page.first,
    posts = Nothing
 }

from: Feed -> FeedState
from (Feed page posts) = {
    newPostsAvailable = False,
    currentPage = page,
    posts = { number = page, posts = posts} |> Just
  }

currentPage: FeedState -> Maybe PostPage
currentPage state = state.posts

moveToPage: FeedState -> Page -> FeedState
moveToPage state page = {state|
    currentPage = page,
    posts = if state.currentPage == page then state.posts else Nothing
 }

hasMorePost: FeedState -> Bool
hasMorePost state = state.posts
    |> Maybe.map (PostPage.isLast >> not)
    |> Maybe.withDefault False

lastPost: FeedState -> Maybe PostId
lastPost state = state.posts
    |> Maybe.map (\x -> x.posts)
    |> Maybe.andThen (List.head)

updateNewPostsAvailable: FeedState -> Bool -> FeedState
updateNewPostsAvailable state status = {state|
    newPostsAvailable = status
 }