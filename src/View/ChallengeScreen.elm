module View.ChallengeScreen exposing (challengeScreen)

import Data.Page as Page
import Data.Post exposing (PostId)
import Data.Schedule exposing (UTCTimestamp)
import Element exposing (Element, centerX, column, fill, height, padding, row, spacing, width)
import State.AppState exposing (AppState)
import State.Cache exposing (Cache)
import State.ChallengeState as ChallengeState exposing (ChallengeState, ChallengeTab(..))
import State.GenericPage as GenericPage
import State.PostPageCache exposing (PostPage)
import Update.Msg exposing (Msg(..))
import View.InfiniteScroll exposing (infiniteScroll)
import View.PostRenderer exposing (renderLoadingPostPage, renderPostId)
import View.ScreenUtils
import View.Style exposing (tabButton)

challengeScreen: AppState -> Element Msg
challengeScreen state = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 5 ]
    [ challengeTabs state.challenge
     , renderChallengeTabContent state]

challengeTabs: ChallengeState -> Element Msg
challengeTabs state = row [spacing 5] [
    challengeTabButton "On going" (ChangeChallengeTab OnGoingTab) (state.currentTab==OnGoingTab)
    , challengeTabButton "Report due" (ChangeChallengeTab ReportDueTab) (state.currentTab==ReportDueTab)
    , challengeTabButton "Upcoming" (ChangeChallengeTab UpcomingTab) (state.currentTab==UpcomingTab)
    , challengeTabButton "Finished" (ChangeChallengeTab FinishedTab) (state.currentTab==FinishedTab)
    , challengeTabButton "On tracks" (ChangeChallengeTab OnTracksTab) (state.currentTab==OnTracksTab)
    , challengeTabButton "Failed" (ChangeChallengeTab FailedTab) (state.currentTab==FailedTab)
    , challengeTabButton "Authored" (ChangeChallengeTab AuthoredTab) (state.currentTab==AuthoredTab)
 ]

challengeTabButton: String -> Msg -> Bool -> Element Msg
challengeTabButton label msg selected = tabButton label msg selected

renderChallengeTabContent: AppState -> Element Msg
renderChallengeTabContent state = case ChallengeState.allUpToCurrentPage state.challenge of
    Nothing -> renderLoadingPosts
    Just posts -> if GenericPage.isEmpty posts && Page.isFirst posts.number then renderNoPostPage
                  else renderPostPage state.timestamp state.cache posts
                     |> infiniteScroll "challenge" (ChangeChallengePage (Page.next state.challenge.currentPage))

renderPostPage: UTCTimestamp -> Cache -> PostPage -> Element Msg
renderPostPage tmstp cache page = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 10 ]
    <| List.map (renderSinglePost tmstp cache) page.items

renderSinglePost: UTCTimestamp -> Cache -> PostId -> Element Msg
renderSinglePost = renderPostId

renderNoPostPage: Element Msg
renderNoPostPage = View.ScreenUtils.emptyScreen "No challenges"

renderLoadingPosts: Element Msg
renderLoadingPosts = renderLoadingPostPage 2