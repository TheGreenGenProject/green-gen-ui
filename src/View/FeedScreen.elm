module View.FeedScreen exposing (..)

import Data.Post exposing (Post, PostId)
import Data.Schedule exposing (UTCTimestamp)
import Element exposing (Element, centerX, column, fill, height, padding, scrollbarY, spacing, width)
import State.AppState exposing (AppState)
import State.Cache exposing (Cache)
import State.FeedState as FeedState exposing (FeedState)
import State.PostPage exposing (PostPage)
import Update.Msg exposing (Msg(..))
import View.PostRenderer exposing (renderPostId)
import View.ScreenUtils
import View.Style exposing (paged)


feedScreen: AppState -> Element Msg
feedScreen state = column [
        width fill
        , height fill
        , scrollbarY
        , centerX
        , spacing 5
        , padding 20 ]
    [ renderFeedState state.timestamp state.cache state.feed ]
    |> paged state.feed.currentPage (\p -> ChangeFeedPage p) (FeedState.hasMorePost state.feed)

renderFeedState: UTCTimestamp -> Cache -> FeedState -> Element Msg
renderFeedState tmstp cache state = case FeedState.currentPage state of
    Just page -> renderPostPage tmstp cache page
    Nothing   -> renderNoPostPage

renderPostPage: UTCTimestamp -> Cache -> PostPage -> Element Msg
renderPostPage tmstp cache page = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 10 ]
    <| List.map (renderSinglePost tmstp cache) page.posts

renderSinglePost: UTCTimestamp -> Cache -> PostId -> Element Msg
renderSinglePost = renderPostId

renderNoPostPage: Element Msg
renderNoPostPage = View.ScreenUtils.emptyScreen "No posts"
