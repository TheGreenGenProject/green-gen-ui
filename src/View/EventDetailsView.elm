module View.EventDetailsView exposing (eventDetailsScreen)


import Data.Event exposing (Event, EventId)
import Data.Location exposing (Location(..), formatAddress, toMapUrl)
import Data.Page as Page
import Data.Schedule as Schedule exposing (UTCTimestamp(..))
import Data.Url exposing (Url(..))
import Data.User exposing (UserId)
import Element exposing (Element, alignLeft, alignRight, alignTop, centerX, centerY, column, el, fill, height, padding, paddingXY, paragraph, px, row, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import State.AppState exposing (AppState)
import State.Cache as Cache exposing (Cache)
import State.EventDetailsState as EventDetailsState exposing (EventDetailsState, EventDetailsTab(..))
import State.GenericPage as GenericPage
import State.UserPageCache exposing (UserPage)
import State.UserState as UserState
import Update.Msg exposing (Msg(..))
import Utils.DateUtils as DateUtils
import Utils.MaybeUtils as MaybeUtils
import View.Icons as Icons
import View.InfiniteScroll exposing (infiniteScroll, infiniteScrollWithMoreButton)
import View.PostRenderer as PostRenderer
import View.ScreenUtils
import View.Style exposing (relFontSize, relSize, tabButton, titledElementStyle, titledTextStyle)
import View.Theme exposing (blue)
import View.UIStyle as UIStyle exposing (UIStyle)
import View.UserListRenderer exposing (renderLoadingUserPage, renderUserId)


eventDetailsScreen: AppState -> EventId -> Element Msg
eventDetailsScreen state eventId =
    let maybeEvent       = Cache.getEvent state.cache eventId
        maybeOwner       = maybeEvent |> Maybe.map (.owner)
        maybeCurrentUser = UserState.currentUser state.user
        isEventOwner     = (maybeCurrentUser |> MaybeUtils.nonEmpty) && (maybeCurrentUser |> Maybe.map (.id)) == maybeOwner
    in column [
        width fill
        , height fill
        , centerX
        , spacing 10
        , padding 20 ]
        (case (maybeEvent, maybeOwner) of
            (Just event, Just _) -> [
                renderEventHeader state isEventOwner event
                , renderEventDetailsTabs state event isEventOwner
                , renderSelectedTabContent state event isEventOwner]
            _                             -> [ renderEventNotFoundPage state.uiStyle]
        )

renderEventHeader: AppState -> Bool -> Event -> Element Msg
renderEventHeader state isEventOwner event =
    let status = if isEventClosed state.timestamp event
                 then el [relFontSize state.uiStyle 5, Font.italic] ("This event is now CLOSED." |> text)
                 else if isEventCancelled state.cache event
                 then el [relFontSize state.uiStyle 5, Font.bold] ("This event is ** CANCELLED **" |> text)
                 else if isEventOwner
                 then el [relFontSize state.uiStyle 5, Font.bold] ("You are the organizer of this event." |> text)
                 else if hasEventParticipationBeenAccepted state.cache event.id
                 then el [relFontSize state.uiStyle 5, Font.bold] ("You are PARTICIPATING to this event." |> text)
                 else if hasEventParticipationBeenRequested state.cache event.id
                 then el [relFontSize state.uiStyle 5, Font.bold] ("You have a PENDING request for this event." |> text)
                 else el [relFontSize state.uiStyle 5, Font.italic] ("This event is opened." |> text)
    in paragraph [paddingXY 10 2] [status, text "  ", renderEventButtons state event.id isEventOwner]

renderEventDetailsTabs: AppState -> Event -> Bool -> Element Msg
renderEventDetailsTabs state event isOwner =  row [spacing 5] [
    eventDetailsTabButton state.uiStyle "Details" (ChangeEventDetailsTab event.id EventDetailsTab) (state.eventDetails.currentTab==EventDetailsTab)
    , eventDetailsTabButton state.uiStyle "Pending Requests" (ChangeEventDetailsTab event.id PendingRequestsTab) (state.eventDetails.currentTab==PendingRequestsTab)
    , eventDetailsTabButton state.uiStyle "Participants" (ChangeEventDetailsTab event.id ParticipantsTab) (state.eventDetails.currentTab==ParticipantsTab)
    , eventDetailsTabButton state.uiStyle "Discussion" (ChangeEventDetailsTab event.id EventDiscussionTab) (state.eventDetails.currentTab==EventDiscussionTab)
 ]

renderEvent: AppState -> Event -> Element Msg
renderEvent state event =
    row [padding 5, spacing 10, width fill] [
        el [Font.color state.uiStyle.theme.enabledButton, alignTop, alignLeft] (Icons.event state.uiStyle.large)
        , column [alignLeft, spacing 20, width fill] [
            titledTextStyle state.uiStyle "Event description" event.description
            , titledElementStyle state.uiStyle "Organized by" (event.owner |> renderUserId state.uiStyle state.cache)
            , titledElementStyle state.uiStyle "When ?" (DateUtils.formatDate (Schedule.start event.schedule) |> text)
            , titledElementStyle state.uiStyle "Where ?" (renderLocation state.uiStyle event.location)
            , titledElementStyle state.uiStyle "How many people are going ?" (renderEventCapacity state event)
        ]
     ]

renderEventNotFoundPage: UIStyle -> Element Msg
renderEventNotFoundPage ui = View.ScreenUtils.emptyScreen ui "Event cannot be found"

isEventClosed: UTCTimestamp -> Event -> Bool
isEventClosed (UTC now) event =
    let (UTC utc) = (Schedule.start event.schedule) in now > utc

isEventCancelled: Cache -> Event -> Bool
isEventCancelled cache event =
    isEventIdCancelled cache event.id

isEventIdCancelled: Cache -> EventId -> Bool
isEventIdCancelled cache eventId =
    Cache.getEventCancelledStatus cache eventId
    |> Maybe.withDefault False

hasEventParticipationBeenAccepted: Cache -> EventId -> Bool
hasEventParticipationBeenAccepted cache eventId =
    Cache.getEventParticipationStatus cache eventId
    |> Maybe.withDefault False

hasEventParticipationBeenRequested: Cache -> EventId -> Bool
hasEventParticipationBeenRequested cache eventId =
    Cache.getEventParticipationRequestStatus cache eventId
    |> Maybe.withDefault False

renderEventButtons: AppState -> EventId -> Bool -> Element Msg
renderEventButtons state eventId isOwner = let cancelled = isEventIdCancelled state.cache eventId in
    if cancelled
    then Element.none
    else if isOwner
    then renderCancelButton state.uiStyle eventId
    else if (hasEventParticipationBeenRequested state.cache eventId) || (hasEventParticipationBeenAccepted state.cache eventId)
    then renderCancelParticipationButton state.uiStyle eventId
    else renderRequestParticipationButton state.uiStyle eventId

renderCancelButton: UIStyle -> EventId -> Element Msg
renderCancelButton ui id =
    Input.button [relFontSize ui 1, paddingXY 2 2, Border.width 1, Border.rounded 5] {
        onPress = id |> CancelEvent |> Just
        , label = (text "Cancel Event")
    }

renderCancelParticipationButton: UIStyle -> EventId -> Element Msg
renderCancelParticipationButton ui id =
    Input.button [relFontSize ui 1, paddingXY 2 2, Border.width 1, Border.rounded 5] {
        onPress = id |> CancelEventParticipation |> Just
        , label = (text "Cancel participation")
    }

renderRequestParticipationButton: UIStyle -> EventId -> Element Msg
renderRequestParticipationButton ui id =
    Input.button [relFontSize ui 1, paddingXY 2 2, Border.width 1, Border.rounded 5] {
        onPress = id |> RequestEventParticipation |> Just
        , label = (text "Request participation")
    }

renderLocation: UIStyle -> Location -> Element Msg
renderLocation ui loc = case loc of
    Online (Url url) -> column [spacing 3] [
        "The event is located online." |> text |> relSize ui 0
        , Element.newTabLink [relFontSize ui 0] { url = url, label = url |> text |> List.singleton |> paragraph [Font.color blue] }
     ]
    MapUrl (Url url) ->
        Element.newTabLink [relFontSize ui 0] { url = url, label = "See on maps" |> text |> el [Font.color blue] }
    (GeoLocation _ _) as geo -> let (Url url) = toMapUrl 17 geo in
        Element.newTabLink [relFontSize ui 0] { url = url, label = "See on maps" |> text |> el [Font.color blue] }
    (Address _ _ _) as address -> column [spacing 3] [
        "The event is located at the following address" |> text |> relSize ui 0
        , formatAddress address |> text |> el [Font.color blue, relFontSize ui 0]
     ]

renderEventCapacity: AppState -> Event -> Element Msg
renderEventCapacity state event = column [relFontSize state.uiStyle 0, centerY, height fill] [
    "- Event is open to " ++ (event.maxParticipants |> String.fromInt) ++ " participant(s)." |> text
    ,"- " ++ (Cache.getEventParticipantCount state.cache event.id
        |> Maybe.withDefault 0
        |> String.fromInt) ++ " is/are participating." |> text
    ,"- " ++ (Cache.getEventParticipantCount state.cache event.id
        |> Maybe.map (\n -> event.maxParticipants - n)
        |> Maybe.withDefault 0
        |> String.fromInt) ++ " slots are still opened" |> text
 ]



renderSelectedTabContent: AppState -> Event -> Bool -> Element Msg
renderSelectedTabContent state event isOwner =
    case state.eventDetails.currentTab of
        EventDetailsTab    -> renderEvent state event
        PendingRequestsTab -> renderPendingRequestUserListTabContent state event isOwner
        ParticipantsTab    -> renderParticipantListTabContent state event isOwner
        EventDiscussionTab -> renderDiscussion state event


eventDetailsTabButton: UIStyle -> String -> Msg -> Bool -> Element Msg
eventDetailsTabButton ui label msg selected = tabButton ui label msg selected


renderPendingRequestUserListTabContent: AppState -> Event -> Bool -> Element Msg
renderPendingRequestUserListTabContent state event isOwner = case EventDetailsState.allUpToCurrentPage state.eventDetails of
    Nothing    -> renderLoadingUsers state.uiStyle
    Just users -> if GenericPage.isEmpty users && Page.isFirst users.number then renderNoUserPage state.uiStyle
                  else renderUserPage users (renderSingleUserPendingRequest state event.id isOwner)
                     |> infiniteScrollWithMoreButton state.uiStyle (UIStyle.isMobile state.device) "event-users" (ChangeEventDetailsPage event.id (Page.next state.eventDetails.currentPage))

renderParticipantListTabContent: AppState -> Event -> Bool -> Element Msg
renderParticipantListTabContent state event isOwner = case EventDetailsState.allUpToCurrentPage state.eventDetails of
    Nothing    -> renderLoadingUsers state.uiStyle
    Just users -> if GenericPage.isEmpty users && Page.isFirst users.number then renderNoUserPage state.uiStyle
                  else renderUserPage users (renderSingleParticipant state.uiStyle state.cache event.id isOwner)
                     |> infiniteScrollWithMoreButton state.uiStyle (UIStyle.isMobile state.device) "event-users" (ChangeEventDetailsPage event.id (Page.next state.eventDetails.currentPage))

renderUserPage: UserPage ->  (UserId -> Element Msg) -> Element Msg
renderUserPage page f = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 10 ]
    <| List.map (f) page.items

renderSingleUserPendingRequest: AppState -> EventId -> Bool -> UserId -> Element Msg
renderSingleUserPendingRequest state eventId isOwner userId = row [width fill] [
    renderUserId state.uiStyle state.cache userId,
    if isOwner
    then row [alignRight, spacing 5] [
        renderAcceptPendingRequestButton state.uiStyle eventId userId
        , renderRejectedPendingRequestButton state.uiStyle eventId userId]
    else Element.none
 ]

renderAcceptPendingRequestButton: UIStyle -> EventId -> UserId -> Element Msg
renderAcceptPendingRequestButton ui eventId userId =
    Input.button [relFontSize ui 1, paddingXY 2 2, Border.width 1, Border.rounded 5, width <| px 50] {
        onPress = AcceptUserEventParticipation eventId userId |> Just
        , label = (text "Accept") |> el [centerX]
    }

renderRejectedPendingRequestButton: UIStyle -> EventId -> UserId -> Element Msg
renderRejectedPendingRequestButton ui eventId userId =
    Input.button [relFontSize ui 1, paddingXY 2 2, Border.width 1, Border.rounded 5, width <| px 50] {
        onPress = RejectUserEventParticipation eventId userId |> Just
        , label = (text "Reject") |> el [centerX]
    }

renderSingleParticipant: UIStyle -> Cache -> EventId -> Bool -> UserId -> Element Msg
renderSingleParticipant ui cache eventId isOwner userId = renderUserId ui cache userId

renderNoUserPage: UIStyle -> Element Msg
renderNoUserPage ui = View.ScreenUtils.emptyScreen ui "No users"

renderLoadingUsers: UIStyle -> Element Msg
renderLoadingUsers ui = renderLoadingUserPage ui 2

renderDiscussion: AppState -> Event -> Element Msg
renderDiscussion state event =
    case (Cache.getPostIdForEvent state.cache event.id) of
        Just postId -> PostRenderer.renderOpenedConversation state.uiStyle state.timestamp state.cache postId
        Nothing     -> View.ScreenUtils.emptyScreen state.uiStyle "Couldn't find conversation"