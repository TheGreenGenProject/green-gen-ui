module View.ChallengeScreen exposing (challengeScreen)

import Data.Page as Page
import Data.Post exposing (PostId)
import Data.Schedule exposing (UTCTimestamp)
import Element exposing (Element, centerX, column, fill, height, padding, spacing, width, wrappedRow)
import State.AppState exposing (AppState)
import State.Cache exposing (Cache)
import State.ChallengeState as ChallengeState exposing (ChallengeState, ChallengeTab(..))
import State.GenericPage as GenericPage
import State.PostPageCache exposing (PostPage)
import Update.Msg exposing (Msg(..))
import View.InfiniteScroll exposing (infiniteScrollWithMoreButton)
import View.PostRenderer exposing (renderLoadingPostPage, renderPostId)
import View.ScreenUtils
import View.Style exposing (tabButton)
import View.UIStyle as UIStyle exposing (UIStyle)

challengeScreen: AppState -> Element Msg
challengeScreen state = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 5 ]
    [ challengeTabs state, renderChallengeTabContent state]

challengeTabs: AppState -> Element Msg
challengeTabs state = wrappedRow [spacing 5] [
    challengeTabButton state.uiStyle "On going" (ChangeChallengeTab OnGoingTab) (state.challenge.currentTab==OnGoingTab)
    , challengeTabButton state.uiStyle "Report due" (ChangeChallengeTab ReportDueTab) (state.challenge.currentTab==ReportDueTab)
    , challengeTabButton state.uiStyle "Upcoming" (ChangeChallengeTab UpcomingTab) (state.challenge.currentTab==UpcomingTab)
    , challengeTabButton state.uiStyle "Finished" (ChangeChallengeTab FinishedTab) (state.challenge.currentTab==FinishedTab)
    , challengeTabButton state.uiStyle "On tracks" (ChangeChallengeTab OnTracksTab) (state.challenge.currentTab==OnTracksTab)
    , challengeTabButton state.uiStyle "Failed" (ChangeChallengeTab FailedTab) (state.challenge.currentTab==FailedTab)
    , challengeTabButton state.uiStyle "Authored" (ChangeChallengeTab AuthoredTab) (state.challenge.currentTab==AuthoredTab)
 ]

challengeTabButton: UIStyle -> String -> Msg -> Bool -> Element Msg
challengeTabButton ui label msg selected = tabButton ui label msg selected

renderChallengeTabContent: AppState -> Element Msg
renderChallengeTabContent state = case ChallengeState.allUpToCurrentPage state.challenge of
    Nothing -> renderLoadingPosts state.uiStyle
    Just posts -> if GenericPage.isEmpty posts && Page.isFirst posts.number then renderNoPostPage state.uiStyle
                  else renderPostPage state posts
                     |> infiniteScrollWithMoreButton state.uiStyle (UIStyle.isMobile state.device) "challenge" (ChangeChallengePage (Page.next state.challenge.currentPage))

renderPostPage: AppState -> PostPage -> Element Msg
renderPostPage state page = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 10 ]
    <| List.map (renderSinglePost state.uiStyle state.timestamp state.cache) page.items

renderSinglePost: UIStyle -> UTCTimestamp -> Cache -> PostId -> Element Msg
renderSinglePost = renderPostId

renderNoPostPage: UIStyle -> Element Msg
renderNoPostPage ui = View.ScreenUtils.emptyScreen ui "No challenges"

renderLoadingPosts: UIStyle -> Element Msg
renderLoadingPosts ui = renderLoadingPostPage ui 2