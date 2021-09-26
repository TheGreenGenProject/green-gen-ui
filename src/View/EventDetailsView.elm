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
import View.InfiniteScroll exposing (infiniteScroll)
import View.PostRenderer as PostRenderer
import View.ScreenUtils
import View.Style exposing (size, titledElementStyle, titledTextStyle)
import View.Theme as Theme exposing (background, blue)
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
                renderEventHeader state.cache state.timestamp isEventOwner event
                , renderEventDetailsTabs state.eventDetails event isEventOwner
                , renderSelectedTabContent state event isEventOwner]
            _                             -> [ renderEventNotFoundPage ]
        )

renderEventHeader: Cache -> UTCTimestamp -> Bool -> Event -> Element Msg
renderEventHeader cache now isEventOwner event =
    let status = if isEventClosed now event
                 then el [Font.size 15, Font.italic] ("This event is now CLOSED." |> text)
                 else if isEventCancelled cache event
                 then el [Font.size 15, Font.bold] ("This event is ** CANCELLED **" |> text)
                 else if isEventOwner
                 then el [Font.size 15, Font.bold] ("You are the organizer of this event." |> text)
                 else if hasEventParticipationBeenAccepted cache event.id
                 then el [Font.size 15, Font.bold] ("You are PARTICIPATING to this event." |> text)
                 else if hasEventParticipationBeenRequested cache event.id
                 then el [Font.size 15, Font.bold] ("You have a PENDING request for this event." |> text)
                 else el [Font.size 15, Font.italic] ("This event is opened." |> text)
    in paragraph [paddingXY 10 2] [status, text "  ", renderEventButtons cache event.id isEventOwner]

renderEventDetailsTabs: EventDetailsState -> Event -> Bool -> Element Msg
renderEventDetailsTabs state event isOwner =  row [spacing 5] [
    eventDetailsTabButton "Details" (ChangeEventDetailsTab  event.id EventDetailsTab) (state.currentTab==EventDetailsTab)
    , eventDetailsTabButton "Pending Requests" (ChangeEventDetailsTab  event.id PendingRequestsTab) (state.currentTab==PendingRequestsTab)
    , eventDetailsTabButton "Participants" (ChangeEventDetailsTab  event.id ParticipantsTab) (state.currentTab==ParticipantsTab)
    , eventDetailsTabButton "Discussion" (ChangeEventDetailsTab event.id EventDiscussionTab) (state.currentTab==EventDiscussionTab)
 ]

renderEvent: Cache -> UTCTimestamp -> Event -> Element Msg
renderEvent cache now event =
    row [padding 5, spacing 10, width fill] [
        el [Font.color Theme.background, alignTop, alignLeft] (Icons.event Icons.large)
        , column [alignLeft, spacing 20, width fill] [
            titledTextStyle "Event description" event.description 10
            , titledElementStyle "Organized by" (event.owner |> renderUserId cache) 10
            , titledElementStyle "When ?" (DateUtils.formatDate (Schedule.start event.schedule) |> text) 10
            , titledElementStyle "Where ?" (renderLocation event.location) 10
            , titledElementStyle "How many people are going ?" (renderEventCapacity cache event) 10
        ]
     ]

renderEventNotFoundPage: Element Msg
renderEventNotFoundPage = View.ScreenUtils.emptyScreen "Event cannot be found"

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

renderEventButtons: Cache -> EventId -> Bool -> Element Msg
renderEventButtons cache eventId isOwner = let cancelled = isEventIdCancelled cache eventId in
    if cancelled
    then Element.none
    else if isOwner
    then renderCancelButton eventId
    else if (hasEventParticipationBeenRequested cache eventId) || (hasEventParticipationBeenAccepted cache eventId)
    then renderCancelParticipationButton eventId
    else renderRequestParticipationButton eventId

renderCancelButton: EventId -> Element Msg
renderCancelButton id =
    Input.button [Font.size 11, paddingXY 2 2, Border.width 1, Border.rounded 5] {
        onPress = id |> CancelEvent |> Just
        , label = (text "Cancel Event")
    }

renderCancelParticipationButton: EventId -> Element Msg
renderCancelParticipationButton id =
    Input.button [Font.size 11, paddingXY 2 2, Border.width 1, Border.rounded 5] {
        onPress = id |> CancelEventParticipation |> Just
        , label = (text "Cancel participation")
    }

renderRequestParticipationButton: EventId -> Element Msg
renderRequestParticipationButton id =
    Input.button [Font.size 11, paddingXY 2 2, Border.width 1, Border.rounded 5] {
        onPress = id |> RequestEventParticipation |> Just
        , label = (text "Request participation")
    }

renderLocation: Location -> Element Msg
renderLocation loc = case loc of
    Online (Url url) -> column [spacing 3] [
        "The event is located online." |> text |> size 10
        , Element.newTabLink [Font.size 10] { url = url, label = url |> text |> List.singleton |> paragraph [Font.color blue] }
     ]
    MapUrl (Url url) ->
        Element.newTabLink [Font.size 10] { url = url, label = "See on maps" |> text |> el [Font.color blue] }
    (GeoLocation _ _) as geo -> let (Url url) = toMapUrl 17 geo in
        Element.newTabLink [Font.size 10] { url = url, label = "See on maps" |> text |> el [Font.color blue] }
    (Address _ _ _) as address -> column [spacing 3] [
        "The event is located at the following address" |> text |> size 10
        , formatAddress address |> text |> el [Font.color blue, Font.size 10]
     ]

renderEventCapacity: Cache -> Event -> Element Msg
renderEventCapacity cache event = column [Font.size 10, centerY, height fill] [
    "- Event is open to " ++ (event.maxParticipants |> String.fromInt) ++ " participant(s)." |> text
    ,"- " ++ (Cache.getEventParticipantCount cache event.id
        |> Maybe.withDefault 0
        |> String.fromInt) ++ " is/are participating." |> text
    ,"- " ++ (Cache.getEventParticipantCount cache event.id
        |> Maybe.map (\n -> event.maxParticipants - n)
        |> Maybe.withDefault 0
        |> String.fromInt) ++ " slots are still opened" |> text
 ]



renderSelectedTabContent: AppState -> Event -> Bool -> Element Msg
renderSelectedTabContent state event isOwner =
    case state.eventDetails.currentTab of
        EventDetailsTab    -> renderEvent state.cache state.timestamp event
        PendingRequestsTab -> renderPendingRequestUserListTabContent state event isOwner
        ParticipantsTab    -> renderParticipantListTabContent state event isOwner
        EventDiscussionTab -> renderDiscussion state event


eventDetailsTabButton: String -> Msg -> Bool -> Element Msg
eventDetailsTabButton label msg selected = Input.button [
    Font.size 14
    , Font.color background
    , if selected then Font.italic else Font.regular
    , Border.color background
    , Border.widthEach { bottom = (if selected then 3 else 0), top = 0, left = 0, right = 0 }
    , padding 5
 ] { onPress = Just msg, label = label |> text }


renderPendingRequestUserListTabContent: AppState -> Event -> Bool -> Element Msg
renderPendingRequestUserListTabContent state event isOwner = case EventDetailsState.allUpToCurrentPage state.eventDetails of
    Nothing    -> renderLoadingUsers
    Just users -> if GenericPage.isEmpty users && Page.isFirst users.number then renderNoUserPage
                  else renderUserPage users (renderSingleUserPendingRequest state.cache event.id isOwner)
                     |> infiniteScroll "event-users" (ChangeEventDetailsPage event.id (Page.next state.eventDetails.currentPage))

renderParticipantListTabContent: AppState -> Event -> Bool -> Element Msg
renderParticipantListTabContent state event isOwner = case EventDetailsState.allUpToCurrentPage state.eventDetails of
    Nothing    -> renderLoadingUsers
    Just users -> if GenericPage.isEmpty users && Page.isFirst users.number then renderNoUserPage
                  else renderUserPage users (renderSingleParticipant state.cache event.id isOwner)
                     |> infiniteScroll "event-users" (ChangeEventDetailsPage event.id (Page.next state.eventDetails.currentPage))

renderUserPage: UserPage ->  (UserId -> Element Msg) -> Element Msg
renderUserPage page f = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 10 ]
    <| List.map (f) page.items

renderSingleUserPendingRequest: Cache -> EventId -> Bool -> UserId -> Element Msg
renderSingleUserPendingRequest cache eventId isOwner userId = row [width fill] [
    renderUserId cache userId,
    if isOwner
    then row [alignRight, spacing 5] [
        renderAcceptPendingRequestButton eventId userId
        , renderRejectedPendingRequestButton eventId userId]
    else Element.none
 ]

renderAcceptPendingRequestButton: EventId -> UserId -> Element Msg
renderAcceptPendingRequestButton eventId userId =
    Input.button [Font.size 11, paddingXY 2 2, Border.width 1, Border.rounded 5, width <| px 50] {
        onPress = AcceptUserEventParticipation eventId userId |> Just
        , label = (text "Accept") |> el [centerX]
    }

renderRejectedPendingRequestButton: EventId -> UserId -> Element Msg
renderRejectedPendingRequestButton eventId userId =
    Input.button [Font.size 11, paddingXY 2 2, Border.width 1, Border.rounded 5, width <| px 50] {
        onPress = RejectUserEventParticipation eventId userId |> Just
        , label = (text "Reject") |> el [centerX]
    }

renderSingleParticipant: Cache -> EventId -> Bool -> UserId -> Element Msg
renderSingleParticipant cache eventId isOwner userId = renderUserId cache userId

renderNoUserPage: Element Msg
renderNoUserPage = View.ScreenUtils.emptyScreen "No users"

renderLoadingUsers: Element Msg
renderLoadingUsers = renderLoadingUserPage 2

renderDiscussion: AppState -> Event -> Element Msg
renderDiscussion state event =
    case (Cache.getPostIdForEvent state.cache event.id) of
        Just postId -> PostRenderer.renderOpenedConversation state.timestamp state.cache postId
        Nothing     -> View.ScreenUtils.emptyScreen "Couldn't find conversation"