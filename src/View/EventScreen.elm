module View.EventScreen exposing (eventScreen)

import Data.Page as Page
import Data.Post exposing (PostId)
import Data.Schedule exposing (UTCTimestamp)
import Element exposing (Element, centerX, column, fill, height, padding, row, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import State.AppState exposing (AppState)
import State.Cache exposing (Cache)
import State.EventState as EventState exposing (EventState, EventTab(..))
import State.GenericPage as GenericPage
import State.PostPageCache exposing (PostPage)
import Update.Msg exposing (Msg(..))
import View.InfiniteScroll exposing (infiniteScroll)
import View.PostRenderer exposing (renderLoadingPostPage, renderPostId)
import View.ScreenUtils
import View.Theme exposing (background)


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
eventTabButton label msg selected = Input.button [
    Font.size 14
    , Font.color background
    , if selected then Font.italic else Font.regular
    , Border.color background
    , Border.widthEach { bottom = (if selected then 3 else 0), top = 0, left = 0, right = 0 }
    , padding 5
 ] { onPress = Just msg, label = label |> text }

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