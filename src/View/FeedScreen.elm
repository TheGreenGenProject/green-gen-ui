module View.FeedScreen exposing (..)

import Data.Page as Page
import Data.Post exposing (Post, PostId)
import Data.Schedule exposing (UTCTimestamp)
import Element exposing (Element, centerX, column, fill, height, padding, spacing, width)
import State.AppState exposing (AppState)
import State.Cache exposing (Cache)
import State.FeedState as FeedState exposing (FeedState)
import State.GenericPage as GenericPage
import State.PostPageCache exposing (PostPage)
import Update.Msg exposing (Msg(..))
import View.InfiniteScroll exposing (infiniteScrollWithMoreButton)
import View.PostRenderer exposing (renderLoadingPostPage, renderPostId)
import View.UIStyle as UIStyle exposing (UIStyle)
import View.WelcomeScreen as WelcomeScreen


feedScreen: AppState -> Element Msg
feedScreen state = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 5 ]
    [ renderFeedState state ]
    |> infiniteScrollWithMoreButton state.uiStyle (UIStyle.isMobile state.device) "feed" (ChangeFeedPage (Page.next state.feed.currentPage))

renderFeedState: AppState -> Element Msg
renderFeedState state = case FeedState.allUpToCurrentPage state.feed of
    Just page -> if GenericPage.isEmpty page then renderNoPostPage state.uiStyle else renderPostPage state page
    Nothing   -> renderLoadingPosts state.uiStyle

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
renderNoPostPage = WelcomeScreen.welcomeScreen

renderLoadingPosts: UIStyle -> Element Msg
renderLoadingPosts ui = renderLoadingPostPage ui 2
