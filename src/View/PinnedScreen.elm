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
import View.InfiniteScroll exposing (infiniteScroll, infiniteScrollWithMoreButton)
import View.PostRenderer exposing (renderLoadingPostPage, renderPostId)
import View.ScreenUtils
import View.UIStyle as UIStyle exposing (UIStyle)


pinnedScreen: AppState -> Element Msg
pinnedScreen state = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 5 ]
    [ renderPinnedState state ]
    |> infiniteScrollWithMoreButton state.uiStyle (UIStyle.isMobile state.device) "pinned" (ChangePinnedPage (Page.next state.pinned.currentPage))

renderPinnedState: AppState -> Element Msg
renderPinnedState state =
    case PinnedState.allUpToCurrentPage state.pinned of
        Just page -> if GenericPage.isEmpty page then renderNoPinnedPostPage state.uiStyle else renderPostPage state page
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

renderNoPinnedPostPage: UIStyle -> Element Msg
renderNoPinnedPostPage ui = View.ScreenUtils.emptyScreen ui "No pinned post"

renderLoadingPosts: UIStyle -> Element Msg
renderLoadingPosts ui = renderLoadingPostPage ui 2
