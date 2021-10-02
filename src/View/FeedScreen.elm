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
import View.InfiniteScroll exposing (infiniteScroll)
import View.PostRenderer exposing (renderLoadingPostPage, renderPostId)
import View.ScreenUtils
import View.WelcomeScreen as WelcomeScreen


feedScreen: AppState -> Element Msg
feedScreen state = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 5 ]
    [ renderFeedState state.timestamp state.cache state.feed ]
    |> infiniteScroll "feed" (ChangeFeedPage (Page.next state.feed.currentPage))

renderFeedState: UTCTimestamp -> Cache -> FeedState -> Element Msg
renderFeedState tmstp cache state = case FeedState.allUpToCurrentPage state of
    Just page -> if GenericPage.isEmpty page then renderNoPostPage else renderPostPage tmstp cache page
    Nothing   -> renderLoadingPosts

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
renderNoPostPage = WelcomeScreen.welcomeScreen

renderLoadingPosts: Element Msg
renderLoadingPosts = renderLoadingPostPage 2
