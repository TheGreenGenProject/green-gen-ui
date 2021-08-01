module Query.Json.NotificationDecoder exposing (
    decodeUnreadNotifications
    , decodeNotifications
    , decodeUnreadNotification
    , decodeNotificationsWithReadStatus)


import Data.Notification exposing (Notification, NotificationContent(..), NotificationId(..), Status(..))
import Json.Decode as Decoder exposing (Decoder, string, succeed)
import Json.Decode.Pipeline as Decoder exposing (required)
import Query.Json.ChallengeDecoder exposing (decodeChallengeId)
import Query.Json.DecoderUtils exposing (decodeTimestamp, decodeUserId, decodeUuid, unitDecoder)
import Query.Json.PostDecoder exposing (decodePostId)
import Tuple exposing (pair)


decodeUnreadNotifications: Decoder (List Notification)
decodeUnreadNotifications = Decoder.list decodeUnreadNotification

decodeNotifications: Decoder (List Notification)
decodeNotifications = Decoder.list decodeNotificationsWithReadStatus

decodeUnreadNotification: Decoder Notification
decodeUnreadNotification =
    Decoder.map4 Notification
        (Decoder.field "id" decodeNotificationId)
        (Decoder.field "content" decodeNotificationContent)
        (Decoder.field "timestamp" decodeTimestamp)
        (Decoder.succeed Unread)

decodeNotificationsWithReadStatus: Decoder Notification
decodeNotificationsWithReadStatus = Decoder.map2 pair
    (Decoder.field "notification" decodeUnreadNotification)
    (Decoder.field "status" decodeReadStatus)
    |> Decoder.map (\(notif, status) -> {notif| status = status} )

-- Helpers

decodeNotificationId: Decoder NotificationId
decodeNotificationId = succeed NotificationId
    |> required "id" decodeUuid

decodeReadStatus: Decoder Status
decodeReadStatus = Decoder.oneOf [
    decodeUnread,
    decodeRead
 ]

decodeRead: Decoder Status
decodeRead = Decoder.field "Read" (Decoder.field "timestamp" decodeTimestamp)
    |> Decoder.map (\_ -> Read)

decodeUnread: Decoder Status
decodeUnread = Decoder.field "Unread" unitDecoder
    |> Decoder.map (\_ -> Unread)

decodeNotificationContent: Decoder NotificationContent
decodeNotificationContent = Decoder.oneOf [
    decodePlatformNotificationContent,
    decodePostLikedNotificationContent,
    decodeNewFollowerNotificationContent,
    decodeYouHaveBeenChallengedNotificationContent,
    decodeChallengeAcceptedNotificationContent,
    decodeChallengeRejectedNotificationContent
  ]

decodePlatformNotificationContent: Decoder NotificationContent
decodePlatformNotificationContent = Decoder.field "PlatformMessageNotification"
    (succeed PlatformMessageNotification |> required "message" string)

decodePostLikedNotificationContent: Decoder NotificationContent
decodePostLikedNotificationContent = Decoder.field "PostLikedNotification"
    (succeed PostLikedNotification
        |> required "postId" decodePostId
        |> required "likedBy" decodeUserId)

decodeNewFollowerNotificationContent: Decoder NotificationContent
decodeNewFollowerNotificationContent = Decoder.field "NewFollowerNotification"
    (succeed NewFollowerNotification
        |> required "follower" decodeUserId)

decodeYouHaveBeenChallengedNotificationContent: Decoder NotificationContent
decodeYouHaveBeenChallengedNotificationContent = Decoder.field "YouHaveBeenChallengedNotification"
    (succeed YouHaveBeenChallengedNotification
        |> required "challengeId" decodeChallengeId)

decodeChallengeAcceptedNotificationContent: Decoder NotificationContent
decodeChallengeAcceptedNotificationContent = Decoder.field "ChallengeAcceptedNotification"
    (succeed ChallengeAcceptedNotification
        |> required "challengeId" decodeChallengeId
        |> required "userId" decodeUserId)

decodeChallengeRejectedNotificationContent: Decoder NotificationContent
decodeChallengeRejectedNotificationContent = Decoder.field "ChallengeRejectedNotification"
    (succeed ChallengeRejectedNotification
        |> required "challengeId" decodeChallengeId
        |> required "userId" decodeUserId)