module Data.Notification exposing (..)

import Data.Challenge exposing (ChallengeId)
import Data.Event exposing (EventId)
import Data.Poll exposing (PollId)
import Data.Post exposing (PostId)
import Data.User exposing (UserId)
import Utils.ListUtils as ListUtils
import Uuid exposing (Uuid)
import Data.Schedule exposing (UTCTimestamp)


type NotificationId = NotificationId Uuid
type NotificationContent = PlatformMessageNotification String
    | EventModifiedNotification EventId
    | EventParticipationRequestAcceptedNotification EventId
    | EventParticipationRequestRejectedNotification EventId
    | EventCancelledNotification EventId
    | NewFollowerNotification UserId
    | PostLikedNotification PostId UserId
    | YouHaveBeenChallengedNotification ChallengeId
    | ChallengeAcceptedNotification ChallengeId UserId
    | ChallengeRejectedNotification ChallengeId UserId
    | PollAnsweredNotification PollId UserId

type Status = Read | Unread

type alias Notification = {
    id: NotificationId,
    content: NotificationContent,
    created: UTCTimestamp,
    status: Status
  }

isRead: Notification -> Bool
isRead notif = notif.status == Read

userFromNotification: Notification -> Maybe UserId
userFromNotification notif = case notif.content of
    NewFollowerNotification userId -> userId |> Just
    PostLikedNotification _ userId -> userId |> Just
    _                              -> Nothing

usersFromNotifications: List Notification -> List UserId
usersFromNotifications = List.concatMap (\n -> n |> userFromNotification |> ListUtils.fromMaybe)

eventFromNotification: Notification -> Maybe EventId
eventFromNotification notif = case notif.content of
    EventModifiedNotification eventId                     -> eventId |> Just
    EventParticipationRequestAcceptedNotification eventId -> eventId |> Just
    EventParticipationRequestRejectedNotification eventId -> eventId |> Just
    EventCancelledNotification eventId                    -> eventId |> Just
    _                                                     -> Nothing

eventsFromNotifications: List Notification -> List EventId
eventsFromNotifications = List.concatMap (\n -> n |> eventFromNotification |> ListUtils.fromMaybe)

eventFromChallenge: Notification -> Maybe ChallengeId
eventFromChallenge notif = case notif.content of
    ChallengeAcceptedNotification challengeId _ -> challengeId |> Just
    ChallengeRejectedNotification challengeId _ -> challengeId |> Just
    _                                           -> Nothing

eventsFromChallenges: List Notification -> List ChallengeId
eventsFromChallenges = List.concatMap (\n -> n |> eventFromChallenge |> ListUtils.fromMaybe)

fromString: String -> Maybe NotificationId
fromString = Maybe.map NotificationId << Uuid.fromString

fromUuid: Uuid -> NotificationId
fromUuid uuid = NotificationId uuid

toString: NotificationId -> String
toString (NotificationId uuid) = Uuid.toString uuid