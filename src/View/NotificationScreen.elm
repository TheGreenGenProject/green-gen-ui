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
import View.InfiniteScroll exposing (infiniteScroll, infiniteScrollWithMoreButton)
import View.ScreenUtils as ScreenUtils
import View.Style exposing (notifHeaderStyle, relFontSize, tabButton, userStyle, viewChallengeButtonStyle, viewEventButtonStyle)
import View.UIStyle as UIStyle exposing (UIStyle)


notificationScreen: AppState -> Element Msg
notificationScreen state = column [
    width fill
    , height fill
    , centerX
    , spacing 5
    , padding 5 ]
    [ notificationTabs state
     , if NotificationState.hasNotifications state.notifications
       then renderNotifications state
       else renderEmptyNotificationScreen state.uiStyle]
    |> infiniteScrollWithMoreButton state.uiStyle (UIStyle.isMobile state.device) "notifications" (ChangeNotificationPage (Page.next state.notifications.currentPage))

notificationTabs: AppState -> Element Msg
notificationTabs state = row [spacing 5] [
    notifTabButton state.uiStyle "Unread" (ChangeNotificationTab UnreadTab) (state.notifications.currentTab==UnreadTab)
    , notifTabButton state.uiStyle "All" (ChangeNotificationTab AllTab) (state.notifications.currentTab==AllTab)
 ]

-- Rendering
renderNotifications: AppState -> Element Msg
renderNotifications state = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 10
    ] (NotificationState.getAllUpTo state.notifications.currentPage state.notifications.latest
        |> Maybe.map (.notifications)
        |> Maybe.withDefault []
        |> List.map (renderNotification state)
      )

renderNotification: AppState -> Notification -> Element Msg
renderNotification state notif = row [
        width fill
        , spacing 10
    ] [renderHeader state notif
       , renderTimestamp state.uiStyle state.timestamp notif
       , renderContent state.uiStyle state.cache notif]

renderEmptyNotificationScreen: UIStyle -> Element Msg
renderEmptyNotificationScreen ui =
    ScreenUtils.emptyScreen ui "No new notification"

-- Helpers
renderHeader: AppState -> Notification -> Element Msg
renderHeader state notif =
    el [alignLeft] (notifLogo state.uiStyle notif)
    |> notifHeaderStyle state.uiStyle

renderContent: UIStyle -> Cache -> Notification -> Element Msg
renderContent ui cache notif = let read = notif.status == Read
                                   elmt = el [width fill, alignLeft, (if read then Font.italic else Font.semiBold), relFontSize ui 2]
    in
        case notif.content of
            PlatformMessageNotification msg                       -> text msg                                                          |> elmt
            EventModifiedNotification eventId                     -> text "Event has been updated"                                     |> elmt
            EventParticipationRequestAcceptedNotification eventId -> renderEventParticipationAcceptedNotification ui eventId           |> elmt
            EventParticipationRequestRejectedNotification eventId -> renderEventParticipationRejectedNotification ui eventId           |> elmt
            EventCancelledNotification eventId                    -> renderEventCancelledNotification ui eventId                       |> elmt
            NewFollowerNotification  userId                       -> renderUserString ui cache userId " started to follow you"         |> elmt
            PostLikedNotification postId userId                   -> renderUserString ui cache userId " liked your post"               |> elmt
            YouHaveBeenChallengedNotification challengeId         -> renderYouHaveBeenChallengedNotification ui cache challengeId      |> elmt
            ChallengeAcceptedNotification challengeId userId      -> renderUserString ui cache userId " has accepted your challenge !" |> elmt
            ChallengeRejectedNotification challengeId userId      -> renderUserString ui cache userId " has rejected your challenge !" |> elmt
            PollAnsweredNotification pollId userId                -> renderUserString ui cache userId " has answered your poll"        |> elmt

renderTimestamp: UIStyle -> UTCTimestamp -> Notification -> Element Msg
renderTimestamp ui now notif = el [Font.italic, relFontSize ui -2] (notif.created |> DateUtils.formatRelativeTo now |> text)

renderYouHaveBeenChallengedNotification: UIStyle -> Cache -> ChallengeId -> Element Msg
renderYouHaveBeenChallengedNotification ui cache challengeId =
    let maybeUserId = Cache.getChallenge cache challengeId |> Maybe.map (.author)
        pseudo      = maybeUserId |> Maybe.andThen (\userId -> Cache.getUserPseudo cache userId) |> Maybe.withDefault "<Unknown>"
    in
    row [spacing 5] [renderMaybeUserString ui pseudo maybeUserId " has challenged you!", viewChallengeButtonStyle ui challengeId]

renderMaybeUserString: UIStyle -> String -> Maybe UserId -> String -> Element Msg
renderMaybeUserString ui pseudo userId txt = row [] [
    userStyle ui pseudo userId
    , txt |> text]

renderEventParticipationAcceptedNotification: UIStyle -> EventId -> Element Msg
renderEventParticipationAcceptedNotification ui eventId =
    row [spacing 5] ["You participation to an event has been accepted" |> text, viewEventButtonStyle ui eventId]

renderEventParticipationRejectedNotification: UIStyle -> EventId -> Element Msg
renderEventParticipationRejectedNotification ui eventId =
    row [spacing 5] ["You participation to an event has been rejected" |> text, viewEventButtonStyle ui eventId]

renderEventCancelledNotification: UIStyle -> EventId -> Element Msg
renderEventCancelledNotification ui eventId =
    row [spacing 5] ["You were registered to an event, but it has been ** CANCELLED **" |> text, viewEventButtonStyle ui eventId]

renderUserString: UIStyle -> Cache -> UserId -> String -> Element Msg
renderUserString ui cache userId txt = let pseudo = Cache.getUserPseudo cache userId |> Maybe.withDefault "<Unknown>" in
    row [] [
        userStyle ui pseudo (Just userId)
        , txt |> text]

notifLogo: UIStyle -> Notification -> Element Msg
notifLogo ui notif =
    let logo = case notif.content of PlatformMessageNotification _                   -> ui.small |> Icons.platformNotification
                                     EventModifiedNotification _                     -> ui.small |> Icons.eventNotification
                                     EventParticipationRequestAcceptedNotification _ -> ui.small |> Icons.eventNotification
                                     EventParticipationRequestRejectedNotification _ -> ui.small |> Icons.eventNotification
                                     EventCancelledNotification _                    -> ui.small |> Icons.eventNotification
                                     NewFollowerNotification _                       -> ui.small |> Icons.newFollowerNotification
                                     PostLikedNotification _ _                       -> ui.small |> Icons.postNotification
                                     YouHaveBeenChallengedNotification _             -> ui.small |> Icons.challengeNotification
                                     ChallengeAcceptedNotification _ _               -> ui.small |> Icons.challengeNotification
                                     ChallengeRejectedNotification _ _               -> ui.small |> Icons.challengeNotification
                                     PollAnsweredNotification _ _                    -> ui.small |> Icons.pollNotification
    in Input.button [] {
        onPress = if(Notification.isRead notif) then Nothing else MarkNotificationRead notif.id |> Just,
        label = logo
    }


notifTabButton: UIStyle -> String -> Msg -> Bool -> Element Msg
notifTabButton ui label msg selected = tabButton ui label msg selected