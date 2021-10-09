module View.EventScreen exposing (eventScreen)

import Data.Page as Page
import Data.Post exposing (PostId)
import Data.Schedule exposing (UTCTimestamp)
import Element exposing (Element, centerX, column, fill, height, padding, row, spacing, width)
import State.AppState exposing (AppState)
import State.Cache exposing (Cache)
import State.EventState as EventState exposing (EventState, EventTab(..))
import State.GenericPage as GenericPage
import State.PostPageCache exposing (PostPage)
import Update.Msg exposing (Msg(..))
import View.InfiniteScroll exposing (infiniteScroll)
import View.PostRenderer exposing (renderLoadingPostPage, renderPostId)
import View.ScreenUtils
import View.Style exposing (tabButton)


eventScreen: AppState -> Element Msg
eventScreen state = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 5 ]
    [ eventTabs state.event
     , renderEventTabContent state]

eventTabs: EventState -> Element Msg
eventTabs state = row [spacing 5] [
    eventTabButton "Incoming" (ChangeEventTab IncomingEventTab) (state.currentTab==IncomingEventTab)
    , eventTabButton "Accepted" (ChangeEventTab ParticipationAcceptedEventTab) (state.currentTab==ParticipationAcceptedEventTab)
    , eventTabButton "Requested" (ChangeEventTab RequestedEventTab) (state.currentTab==RequestedEventTab)
    , eventTabButton "Cancelled" (ChangeEventTab CancelledEventTab) (state.currentTab==CancelledEventTab)
    , eventTabButton "Participated" (ChangeEventTab ParticipatedEventTab) (state.currentTab==ParticipatedEventTab)
    , eventTabButton "Organized" (ChangeEventTab OrganizedEventTab) (state.currentTab==OrganizedEventTab)
 ]

eventTabButton: String -> Msg -> Bool -> Element Msg
eventTabButton label msg selected = tabButton label msg selected

renderEventTabContent: AppState -> Element Msg
renderEventTabContent state = case EventState.allUpToCurrentPage state.event of
    Nothing    -> renderLoadingPosts
    Just posts -> if GenericPage.isEmpty posts && Page.isFirst posts.number then renderNoPostPage
                  else renderPostPage state.timestamp state.cache posts
                     |> infiniteScroll "event" (ChangeEventPage (Page.next state.event.currentPage))

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
renderNoPostPage = View.ScreenUtils.emptyScreen "No events"

renderLoadingPosts: Element Msg
renderLoadingPosts = renderLoadingPostPage 2