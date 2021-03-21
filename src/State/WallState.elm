module State.WallState exposing (..)

import Data.Page as Page exposing (Page)
import Data.User exposing (UserId)
import Data.Wall exposing (Wall(..))
import State.PostPage as PostPage exposing (PostPage)


type alias WallState = {
    user: Maybe UserId,
    currentPage: Page,
    posts: Maybe PostPage
 }

empty: WallState
empty = {
    user = Nothing,
    currentPage = Page.first,
    posts = Nothing
 }

from: Wall -> WallState
from (Wall userId page posts) = {
    user = Just userId,
    currentPage = page,
    posts = { number = page, posts = posts } |> Just
 }

currentPage: WallState -> Maybe PostPage
currentPage state = state.posts

moveToPage: WallState -> Page -> WallState
moveToPage state page = {state|
    currentPage = page,
    posts = if state.currentPage == page then state.posts else Nothing
 }

hasMorePost: WallState -> Bool
hasMorePost state = state.posts
    |> Maybe.map (PostPage.isLast >> not)
    |> Maybe.withDefault False

