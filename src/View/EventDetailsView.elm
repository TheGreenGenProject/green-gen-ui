module View.EventDetailsView exposing (eventDetailsScreen)


import Data.Event exposing (Event, EventId)
import Data.Location exposing (Location(..), formatAddress, toMapUrl)
import Data.Schedule as Schedule exposing (UTCTimestamp(..))
import Data.Url exposing (Url(..))
import Element exposing (Element, alignLeft, alignTop, centerX, centerY, column, el, fill, height, padding, paddingXY, paragraph, row, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import State.AppState exposing (AppState)
import State.Cache as Cache exposing (Cache)
import State.UserState as UserState
import Update.Msg exposing (Msg(..))
import Utils.DateUtils as DateUtils
import Utils.MaybeUtils as MaybeUtils
import View.Icons as Icons
import View.ScreenUtils
import View.Style exposing (size, titledElementStyle, titledTextStyle)
import View.Theme as Theme exposing (blue)
import View.UserListRenderer exposing (renderUserId)


eventDetailsScreen: AppState -> EventId -> Element Msg
eventDetailsScreen state challengeId =
    let maybeEvent       = Cache.getEvent state.cache challengeId
        maybeOwner       = maybeEvent |> Maybe.map (.owner)
        maybeCurrentUser = UserState.currentUser state.user
        isEventOwner     = (maybeCurrentUser |> MaybeUtils.nonEmpty) && (maybeCurrentUser |> Maybe.map (.id)) == maybeOwner
    in column [
        width fill
        , centerX
        , spacing 10
        , padding 20 ]
        (case (maybeEvent, maybeOwner) of
            (Just event, Just _) -> [
                renderEventHeader state.cache state.timestamp isEventOwner event
                , renderEvent state.cache state.timestamp event]
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
    Cache.getEventCancelledStatus cache event.id
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
renderEventButtons cache eventId isOwner =
    if isOwner
    then Element.none
    else if (hasEventParticipationBeenRequested cache eventId) || (hasEventParticipationBeenAccepted cache eventId)
    then renderCancelParticipationButton eventId
    else renderRequestParticipationButton eventId

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