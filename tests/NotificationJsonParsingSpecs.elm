module NotificationJsonParsingSpecs exposing (..)

import Data.Notification as Notification exposing (Notification, NotificationContent(..), Status(..))
import Data.Post as Post
import Data.Schedule exposing (UTCTimestamp(..))
import Data.User as User
import Expect
import Json.Decode as Decoder exposing (Error)
import Query.Json.NotificationDecoder exposing (decodeNotifications, decodeNotificationsWithReadStatus, decodeUnreadNotification)
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
                        |> Expect.equal (expectedNewFollowerNotification Unread),

              test "A notification with an Unread status" <|
                \_ ->
                    """{ "notification": {
                            "id": {"id": {"uuid": "349dc629-e401-4fe1-b9a9-290f7f987f61"}},
                            "content": { "NewFollowerNotification": {
                                "follower": { "value": { "uuid": "f51e84c7-36c7-4cf7-9a8b-11207825e2ba" }}
                             }},
                            "timestamp": { "value": 1611510263072 }},
                         "status": {"Unread": {}}
                   }"""
                        |> Decoder.decodeString decodeNotificationsWithReadStatus |> toMaybe
                        |> Expect.equal (expectedNewFollowerNotification Unread),

              test "A notification with an Read status" <|
                \_ ->
                    """{ "notification": {
                            "id": {"id": {"uuid": "349dc629-e401-4fe1-b9a9-290f7f987f61"}},
                            "content": { "NewFollowerNotification": {
                                "follower": { "value": { "uuid": "f51e84c7-36c7-4cf7-9a8b-11207825e2ba" }}
                             }},
                            "timestamp": { "value": 1611510263072 }},
                         "status": {"Read":{"timestamp":{"value":1627824353283}}}
                   }"""
                        |> Decoder.decodeString decodeNotificationsWithReadStatus |> toMaybe
                        |> Expect.equal (expectedNewFollowerNotification Read),

              test "A list of notifications with status" <|
                \_ -> """[
                    {"notification":{"id":{"id":{"uuid":"c88b3205-0a33-4822-926d-d3750b7c157f"}},"content":{"PostLikedNotification":{"postId":{"value":{"uuid":"766a6434-1482-44b5-a175-59ee1c938792"}},"likedBy":{"value":{"uuid":"40970e1b-2696-4b78-a36e-a5703db2a3f1"}}}},"timestamp":{"value":1627824653025}},"status":{"Unread":{}}},
                    {"notification":{"id":{"id":{"uuid":"72ddb305-153c-4afa-ba86-b7237d967bf9"}},"content":{"PlatformMessageNotification":{"message":"This is a GreenGen announcement: tatadatadatada !!!"}},"timestamp":{"value":1627818573426}},"status":{"Unread":{}}},
                    {"notification":{"id":{"id":{"uuid":"23adb64d-a332-4bf0-84de-a14cb1018930"}},"content":{"YouHaveBeenChallengedNotification":{"challengeId":{"value":{"uuid":"c7ccfadb-922b-4a93-8d8e-90ef02951286"}}}},"timestamp":{"value":1627818573351}},"status":{"Read":{"timestamp":{"value":1627824353283}}}},
                    {"notification":{"id":{"id":{"uuid":"d349da49-49e3-4817-a835-0209c08f816f"}},"content":{"NewFollowerNotification":{"follower":{"value":{"uuid":"b29b78c1-e579-4e47-b474-f3b64c9e0bbe"}}}},"timestamp":{"value":1627818572892}},"status":{"Read":{"timestamp":{"value":1627824355418}}}},
                    {"notification":{"id":{"id":{"uuid":"85a9f096-4c37-4220-a55d-69f7490d4b1a"}},"content":{"NewFollowerNotification":{"follower":{"value":{"uuid":"82c6ec2f-901b-434d-ab00-fa37c8e1229b"}}}},"timestamp":{"value":1627818572889}},"status":{"Unread":{}}}
                   ]"""
                    |> Decoder.decodeString decodeNotifications
                    |> toMaybe |> Maybe.map (\xs -> List.length xs == 5) |> Maybe.withDefault False
                    |> Expect.true "The List of notifications should have decoded 5 notifications with the associated Read status"
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

expectedNewFollowerNotification: Status -> Maybe Notification
expectedNewFollowerNotification status = case (Notification.fromString "349dc629-e401-4fe1-b9a9-290f7f987f61",
                                        User.fromString "f51e84c7-36c7-4cf7-9a8b-11207825e2ba") of
    (Just id, Just userId) -> {
        id = id,
        content = NewFollowerNotification userId,
        created = UTC 1611510263072,
        status = status } |> Just
    _ -> Nothing

toMaybe: Result Error a -> Maybe a
toMaybe x = case x of
    Err m     -> Debug.log ("Error: " ++ (Debug.toString m)) Nothing
    Ok value -> Just value