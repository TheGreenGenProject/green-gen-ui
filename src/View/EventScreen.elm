module View.EventScreen exposing (eventScreen)

import Data.Page as Page
import Data.Post exposing (PostId)
import Data.Schedule exposing (UTCTimestamp)
import Element exposing (Element, centerX, column, el, fill, height, padding, row, scrollbarX, spacing, width, wrappedRow)
import State.AppState exposing (AppState)
import State.Cache exposing (Cache)
import State.EventState as EventState exposing (EventState, EventTab(..))
import State.GenericPage as GenericPage
import State.PostPageCache exposing (PostPage)
import Update.Msg exposing (Msg(..))
import View.InfiniteScroll exposing (infiniteScroll, infiniteScrollWithMoreButton)
import View.PostRenderer exposing (renderLoadingPostPage, renderPostId)
import View.ScreenUtils
import View.Style exposing (tabButton)
import View.UIStyle as UIStyle exposing (UIStyle)


eventScreen: AppState -> Element Msg
eventScreen state = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 5 ]
    [ eventTabs state.uiStyle state.event
     , renderEventTabContent state]

eventTabs: UIStyle -> EventState -> Element Msg
eventTabs ui state = wrappedRow [spacing 5] [
    eventTabButton ui "Incoming" (ChangeEventTab IncomingEventTab) (state.currentTab==IncomingEventTab)
    , eventTabButton ui "Accepted" (ChangeEventTab ParticipationAcceptedEventTab) (state.currentTab==ParticipationAcceptedEventTab)
    , eventTabButton ui "Requested" (ChangeEventTab RequestedEventTab) (state.currentTab==RequestedEventTab)
    , eventTabButton ui "Cancelled" (ChangeEventTab CancelledEventTab) (state.currentTab==CancelledEventTab)
    , eventTabButton ui "Participated" (ChangeEventTab ParticipatedEventTab) (state.currentTab==ParticipatedEventTab)
    , eventTabButton ui "Organized" (ChangeEventTab OrganizedEventTab) (state.currentTab==OrganizedEventTab)
 ]

eventTabButton: UIStyle -> String -> Msg -> Bool -> Element Msg
eventTabButton ui label msg selected = tabButton ui label msg selected

renderEventTabContent: AppState -> Element Msg
renderEventTabContent state = case EventState.allUpToCurrentPage state.event of
    Nothing    -> renderLoadingPosts state.uiStyle
    Just posts -> if GenericPage.isEmpty posts && Page.isFirst posts.number then renderNoPostPage state.uiStyle
                  else renderPostPage state posts
                     |> infiniteScrollWithMoreButton state.uiStyle (UIStyle.isMobile state.device) "event" (ChangeEventPage (Page.next state.event.currentPage))

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
renderNoPostPage ui = View.ScreenUtils.emptyScreen ui "No events"

renderLoadingPosts: UIStyle -> Element Msg
renderLoadingPosts ui = renderLoadingPostPage ui 2