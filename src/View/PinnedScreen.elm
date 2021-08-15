module View.PinnedScreen exposing (pinnedScreen)

import Data.Page as Page
import Data.Post exposing (Post, PostId)
import Data.Schedule exposing (UTCTimestamp)
import Element exposing (Element, centerX, column, fill, height, padding, spacing, width)
import State.AppState exposing (AppState)
import State.Cache exposing (Cache)
import State.GenericPage as GenericPage
import State.PinnedState as PinnedState exposing (PinnedState)
import State.PostPageCache exposing (PostPage)
import Update.Msg exposing (Msg(..))
import View.InfiniteScroll exposing (infiniteScroll)
import View.PostRenderer exposing (renderLoadingPostPage, renderPostId)
import View.ScreenUtils


pinnedScreen: AppState -> Element Msg
pinnedScreen state = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 5 ]
    [ renderPinnedState state.timestamp state.cache state.pinned ]
    |> infiniteScroll "pinned" (ChangePinnedPage (Page.next state.pinned.currentPage))

renderPinnedState: UTCTimestamp -> Cache -> PinnedState -> Element Msg
renderPinnedState tmstp cache state =
    case PinnedState.allUpToCurrentPage state of
        Just page -> if GenericPage.isEmpty page then renderNoPinnedPostPage else renderPostPage tmstp cache page
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

renderNoPinnedPostPage: Element Msg
renderNoPinnedPostPage = View.ScreenUtils.emptyScreen "No pinned post"

renderLoadingPosts: Element Msg
renderLoadingPosts = renderLoadingPostPage 2
