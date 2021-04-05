module State.ChallengeState exposing (..)

import Data.Page as Page exposing (Page)
import Data.Post exposing (PostId)
import State.PostPage as PostPage exposing (PostPage)


type ChallengeTab = OnGoingTab | OnTracksTab | FailedTab | UpcomingTab | ReportDueTab | FinishedTab | AuthoredTab

type alias ChallengePagedTab = {
    tab: ChallengeTab
    , page: Page
 }

type alias ChallengeState = {
    currentTab: ChallengeTab,
    posts: Maybe PostPage
 }

empty: ChallengeState
empty = {
    currentTab = OnGoingTab,
    posts = Nothing
 }

from: List PostId -> ChallengePagedTab -> ChallengeState
from posts pagedTab = {
    currentTab = pagedTab.tab,
    posts = { number = pagedTab.page, posts = posts } |> Just
 }

changeTab: ChallengeTab -> ChallengeState -> ChallengeState
changeTab tab state = {state| currentTab = tab , posts = Nothing}

hasMorePost: ChallengeState -> Bool
hasMorePost state = state.posts
    |> Maybe.map (PostPage.isLast >> not)
    |> Maybe.withDefault False

currentPage: ChallengeState -> Page
currentPage state = state.posts
    |> Maybe.map (.number)
    |> Maybe.withDefault Page.first