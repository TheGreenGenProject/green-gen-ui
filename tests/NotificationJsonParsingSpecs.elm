module NotificationJsonParsingSpecs exposing (..)

import Data.Notification as Notification exposing (Notification, NotificationContent(..), Status(..))
import Data.Post as Post
import Data.Schedule exposing (UTCTimestamp(..))
import Data.User as User
import Expect
import Json.Decode as Decoder exposing (Error)
import Query.Json.NotificationDecoder exposing (decodeUnreadNotification)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Notification Json decoders"
        [ describe "should parse correctly"
            [
              test "a platform notification" <|
                \_ ->
                    """{
                        "id": {"id": {"uuid": "349dc629-e401-4fe1-b9a9-290f7f987f61"}},
                        "content": { "PlatformMessageNotification": { "message": "platform msg" } },
                        "timestamp": { "value": 1611510263072 }
                   }"""
                        |> Decoder.decodeString decodeUnreadNotification |> toMaybe
                        |> Expect.equal expectedPlatformNotification,

              test "a post liked notification" <|
                \_ ->
                    """{
                        "id": {"id": {"uuid": "349dc629-e401-4fe1-b9a9-290f7f987f61"}},
                        "content": { "PostLikedNotification": {
                            "postId": { "value": { "uuid": "4ed1c260-fa8e-4540-8a3c-420ff85d9fd6" }},
                            "likedBy": { "value": { "uuid": "f51e84c7-36c7-4cf7-9a8b-11207825e2ba" }}
                         }},
                        "timestamp": { "value": 1611510263072 }
                   }"""
                        |> Decoder.decodeString decodeUnreadNotification |> toMaybe
                        |> Expect.equal expectedPostLikedNotification,

              test "a new follower notification" <|
                \_ ->
                    """{
                        "id": {"id": {"uuid": "349dc629-e401-4fe1-b9a9-290f7f987f61"}},
                        "content": { "NewFollowerNotification": {
                            "follower": { "value": { "uuid": "f51e84c7-36c7-4cf7-9a8b-11207825e2ba" }}
                         }},
                        "timestamp": { "value": 1611510263072 }
                   }"""
                        |> Decoder.decodeString decodeUnreadNotification |> toMaybe
                        |> Expect.equal expectedNewFollowerNotification

            ]
        ]

expectedPlatformNotification: Maybe Notification
expectedPlatformNotification = case (Notification.fromString "349dc629-e401-4fe1-b9a9-290f7f987f61") of
    Just id -> {
        id = id,
        content = PlatformMessageNotification "platform msg",
        created = UTC 1611510263072,
        status = Unread } |> Just
    _ -> Nothing

expectedPostLikedNotification: Maybe Notification
expectedPostLikedNotification = case (Notification.fromString "349dc629-e401-4fe1-b9a9-290f7f987f61",
                                      Post.fromString "4ed1c260-fa8e-4540-8a3c-420ff85d9fd6",
                                      User.fromString "f51e84c7-36c7-4cf7-9a8b-11207825e2ba") of
    (Just id, Just postId, Just userId) -> {
        id = id,
        content = PostLikedNotification postId userId,
        created = UTC 1611510263072,
        status = Unread } |> Just
    _ -> Nothing

expectedNewFollowerNotification: Maybe Notification
expectedNewFollowerNotification = case (Notification.fromString "349dc629-e401-4fe1-b9a9-290f7f987f61",
                                        User.fromString "f51e84c7-36c7-4cf7-9a8b-11207825e2ba") of
    (Just id, Just userId) -> {
        id = id,
        content = NewFollowerNotification userId,
        created = UTC 1611510263072,
        status = Unread } |> Just
    _ -> Nothing

toMaybe: Result Error a -> Maybe a
toMaybe x = case x of
    Err m     -> Debug.log ("Error: " ++ (Debug.toString m)) Nothing
    Ok value -> Just value