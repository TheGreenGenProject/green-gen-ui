module View.NotificationScreen exposing (notificationScreen)

import Data.Challenge exposing (ChallengeId)
import Data.Event exposing (EventId)
import Data.Notification as Notification exposing (Notification, NotificationContent(..), Status(..))
import Data.Page as Page
import Data.Schedule exposing (UTCTimestamp)
import Data.User exposing (UserId)
import Element exposing (Element, alignLeft, centerX, column, el, fill, height, padding, row, spacing, text, width)
import Element.Font as Font
import Element.Input as Input
import State.AppState exposing (AppState)
import State.Cache as Cache exposing (Cache)
import State.NotificationState as NotificationState exposing (NotificationState, NotificationTab(..))
import Update.Msg exposing (Msg(..))
import Utils.DateUtils as DateUtils
import View.Icons as Icons
import View.InfiniteScroll exposing (infiniteScroll)
import View.ScreenUtils as ScreenUtils
import View.Style exposing (notifHeaderStyle, tabButton, userStyle, viewChallengeButtonStyle, viewEventButtonStyle)


notificationScreen: AppState -> Element Msg
notificationScreen state = column [
    width fill
    , height fill
    , centerX
    , spacing 5
    , padding 5 ]
    [ notificationTabs state.notifications
     , if NotificationState.hasNotifications state.notifications
       then renderNotifications state.timestamp state.cache state.notifications
       else renderEmptyNotificationScreen]
    |> infiniteScroll "notifications" (ChangeNotificationPage (Page.next state.notifications.currentPage))

notificationTabs: NotificationState -> Element Msg
notificationTabs state = row [spacing 5] [
    notifTabButton "Unread" (ChangeNotificationTab UnreadTab) (state.currentTab==UnreadTab)
    , notifTabButton "All" (ChangeNotificationTab AllTab) (state.currentTab==AllTab)
 ]

-- Rendering
renderNotifications: UTCTimestamp -> Cache -> NotificationState -> Element Msg
renderNotifications now cache state = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 10
    ] (NotificationState.getAllUpTo state.currentPage state.latest
        |> Maybe.map (.notifications)
        |> Maybe.withDefault []
        |> List.map (renderNotification now cache)
      )

renderNotification:  UTCTimestamp -> Cache -> Notification -> Element Msg
renderNotification now cache notif = row [
        width fill
        , spacing 10
    ] [renderHeader notif, renderTimestamp now notif, renderContent cache notif]

renderEmptyNotificationScreen: Element Msg
renderEmptyNotificationScreen =
    ScreenUtils.emptyScreen "No new notification"

-- Helpers
renderHeader: Notification -> Element Msg
renderHeader notif =
    el [alignLeft] (notifLogo notif)
    |> notifHeaderStyle

renderContent: Cache -> Notification -> Element Msg
renderContent cache notif = let read = notif.status == Read
                                elmt = el [width fill, alignLeft, (if read then Font.italic else Font.semiBold), Font.size 12]
    in
        case notif.content of
            PlatformMessageNotification msg                       -> text msg                                                       |> elmt
            EventModifiedNotification eventId                     -> text "Event has been updated"                                  |> elmt
            EventParticipationRequestAcceptedNotification eventId -> renderEventParticipationAcceptedNotification cache eventId     |> elmt
            EventParticipationRequestRejectedNotification eventId -> renderEventParticipationRejectedNotification cache eventId     |> elmt
            EventCancelledNotification eventId                    -> renderEventCancelledNotification cache eventId                 |> elmt
            NewFollowerNotification  userId                       -> renderUserString cache userId " started to follow you"         |> elmt
            PostLikedNotification postId userId                   -> renderUserString cache userId " liked your post"               |> elmt
            YouHaveBeenChallengedNotification challengeId         -> renderYouHaveBeenChallengedNotification cache challengeId      |> elmt
            ChallengeAcceptedNotification challengeId userId      -> renderUserString cache userId " has accepted your challenge !" |> elmt
            ChallengeRejectedNotification challengeId userId      -> renderUserString cache userId " has rejected your challenge !" |> elmt
            PollAnsweredNotification pollId userId                -> renderUserString cache userId " has answered your poll"        |> elmt

renderTimestamp: UTCTimestamp -> Notification -> Element Msg
renderTimestamp now notif = el [Font.italic, Font.size 8] (notif.created |> DateUtils.formatRelativeTo now |> text)

renderYouHaveBeenChallengedNotification: Cache -> ChallengeId -> Element Msg
renderYouHaveBeenChallengedNotification cache challengeId =
    let maybeUserId = Cache.getChallenge cache challengeId |> Maybe.map (.author)
        pseudo      = maybeUserId |> Maybe.andThen (\userId -> Cache.getUserPseudo cache userId) |> Maybe.withDefault "<Unknown>"
    in
    row [spacing 5] [renderMaybeUserString pseudo maybeUserId " has challenged you!", viewChallengeButtonStyle challengeId]

renderMaybeUserString: String -> Maybe UserId -> String -> Element Msg
renderMaybeUserString pseudo userId txt = row [] [
    userStyle pseudo userId
    , txt |> text]

renderEventParticipationAcceptedNotification: Cache -> EventId -> Element Msg
renderEventParticipationAcceptedNotification cache eventId =
    row [spacing 5] ["You participation to an event has been accepted" |> text, viewEventButtonStyle eventId]

renderEventParticipationRejectedNotification: Cache -> EventId -> Element Msg
renderEventParticipationRejectedNotification cache eventId =
    row [spacing 5] ["You participation to an event has been rejected" |> text, viewEventButtonStyle eventId]

renderEventCancelledNotification: Cache -> EventId -> Element Msg
renderEventCancelledNotification cache eventId =
    row [spacing 5] ["You were registered to an event, but it has been ** CANCELLED **" |> text, viewEventButtonStyle eventId]

renderUserString: Cache -> UserId -> String -> Element Msg
renderUserString cache userId txt = let pseudo = Cache.getUserPseudo cache userId |> Maybe.withDefault "<Unknown>" in
    row [] [
        userStyle pseudo (Just userId)
        , txt |> text]

notifLogo: Notification -> Element Msg
notifLogo notif =
    let logo = case notif.content of PlatformMessageNotification _                   -> Icons.small |> Icons.platformNotification
                                     EventModifiedNotification _                     -> Icons.small |> Icons.eventNotification
                                     EventParticipationRequestAcceptedNotification _ -> Icons.small |> Icons.eventNotification
                                     EventParticipationRequestRejectedNotification _ -> Icons.small |> Icons.eventNotification
                                     EventCancelledNotification _                    -> Icons.small |> Icons.eventNotification
                                     NewFollowerNotification _                       -> Icons.small |> Icons.newFollowerNotification
                                     PostLikedNotification _ _                       -> Icons.small |> Icons.postNotification
                                     YouHaveBeenChallengedNotification _             -> Icons.small |> Icons.challengeNotification
                                     ChallengeAcceptedNotification _ _               -> Icons.small |> Icons.challengeNotification
                                     ChallengeRejectedNotification _ _               -> Icons.small |> Icons.challengeNotification
                                     PollAnsweredNotification _ _                    -> Icons.small |> Icons.pollNotification
    in Input.button [] {
        onPress = if(Notification.isRead notif) then Nothing else MarkNotificationRead notif.id |> Just,
        label = logo
    }


notifTabButton: String -> Msg -> Bool -> Element Msg
notifTabButton label msg selected = tabButton label msg selected