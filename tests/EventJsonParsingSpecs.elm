module EventJsonParsingSpecs exposing (suite)

import Data.Event as Event exposing (Event)
import Data.Location exposing (Country(..), Latitude(..), Location(..), Longitude(..), ZipCode(..))
import Data.Schedule exposing (Duration(..), Schedule(..), UTCTimestamp(..))
import Data.Url exposing (Url(..))
import Data.User as User
import Expect
import Json.Decode as Decoder exposing (Error)
import Query.Json.EventDecoder exposing (decodeEvent)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Event Json decoders"
        [ describe "should parse correctly"
            [
              test "an online event" <|
                \_ ->
                    """{
                            "id": {"value": {"uuid":"d6d5fbd9-cb24-4dab-9dae-f97e77cc0e10"}},
                            "owner": {"value":{"uuid":"835ef0d9-0277-4c97-b7d3-58249669a1b4"}},
                            "maxParticipants": 5,
                            "description": "This is a fake event happening somewhere on earth, organized by a cat - but nowhere near to your place.",
                            "location": { "Online": { "url": { "url": "https://www.green-gen.org/events/somewhere-online"}}},
                            "schedule": {
                                "Recurring": {
                                    "first": { "value":1630249485781},
                                    "duration": {"millis":3600000},
                                    "every": {"millis":604800000},
                                    "until": {"value":1631720714709}
                                }
                            }
                       }
                    """
                        |> Decoder.decodeString decodeEvent |> toMaybe
                        |> Expect.equal expectedOnlineEvent,

              test "an event with a map url" <|
                \_ ->
                    """{
                            "id": {"value": {"uuid":"d6d5fbd9-cb24-4dab-9dae-f97e77cc0e10"}},
                            "owner": {"value":{"uuid":"835ef0d9-0277-4c97-b7d3-58249669a1b4"}},
                            "maxParticipants": 5,
                            "description": "This is a fake event happening somewhere on earth, organized by a cat - but nowhere near to your place.",
                            "location": { "MapUrl": { "url": { "url": "https://www.openstreetmap.org/#map/17/22.3/50.4"}}},
                            "schedule": {
                                "Recurring": {
                                    "first": { "value":1630249485781},
                                    "duration": {"millis":3600000},
                                    "every": {"millis":604800000},
                                    "until": {"value":1631720714709}
                                }
                            }
                       }
                    """
                        |> Decoder.decodeString decodeEvent |> toMaybe
                        |> Expect.equal expectedMapUrlEvent,

              test "an geo-located event" <|
                \_ ->
                    """{
                            "id": {"value": {"uuid":"d6d5fbd9-cb24-4dab-9dae-f97e77cc0e10"}},
                            "owner": {"value": {"uuid":"835ef0d9-0277-4c97-b7d3-58249669a1b4"}},
                            "maxParticipants": 5,
                            "description": "This is a fake event happening somewhere on earth, organized by a cat - but nowhere near to your place.",
                            "location": {
                                "GeoLocation": {
                                    "coordinates": {
                                        "latitude": { "value": 12.5 },
                                        "longitude": { "value": 32.45 }
                                    }
                                }
                            },
                            "schedule": {
                                "Recurring": {
                                    "first": { "value": 1630249485781},
                                    "duration": {"millis": 3600000},
                                    "every": {"millis": 604800000},
                                    "until": {"value": 1631720714709}
                                }
                            }
                       }
                    """
                        |> Decoder.decodeString decodeEvent |> toMaybe
                        |> Expect.equal expectedGeoLocatedEvent,

              test "an event with an address" <|
                \_ ->
                    """{
                            "id": {"value": {"uuid":"d6d5fbd9-cb24-4dab-9dae-f97e77cc0e10"}},
                            "owner": {"value":{"uuid":"835ef0d9-0277-4c97-b7d3-58249669a1b4"}},
                            "maxParticipants": 5,
                            "description": "This is a physical event with an address",
                            "location": {
                                "Address": {
                                    "address": null,
                                    "zipCode": "E2",
                                    "country": {"name": "UK"}
                                }
                            },
                            "schedule": {
                                "Recurring": {
                                    "first": { "value":1630249485781},
                                    "duration": {"millis":3600000},
                                    "every": {"millis":604800000},
                                    "until": {"value":1631720714709}
                                }
                            }
                       }
                    """
                        |> Decoder.decodeString decodeEvent |> toMaybe
                        |> Expect.equal expectedPhysicalEvent
            ]
        ]

expectedOnlineEvent: Maybe Event
expectedOnlineEvent = case (Event.fromString "d6d5fbd9-cb24-4dab-9dae-f97e77cc0e10",
                      User.fromString "835ef0d9-0277-4c97-b7d3-58249669a1b4") of
    (Just id, Just author) -> {
        id = id,
        owner = author,
        maxParticipants = 5,
        description = "This is a fake event happening somewhere on earth, organized by a cat - but nowhere near to your place.",
        location = Online (Url "https://www.green-gen.org/events/somewhere-online"),
        schedule = Recurring (UTC 1630249485781) (Duration 3600000) (Duration 604800000) (UTC 1631720714709)
      } |> Just
    _ -> Nothing

expectedMapUrlEvent: Maybe Event
expectedMapUrlEvent = case (Event.fromString "d6d5fbd9-cb24-4dab-9dae-f97e77cc0e10",
                      User.fromString "835ef0d9-0277-4c97-b7d3-58249669a1b4") of
    (Just id, Just author) -> {
        id = id,
        owner = author,
        maxParticipants = 5,
        description = "This is a fake event happening somewhere on earth, organized by a cat - but nowhere near to your place.",
        location = MapUrl (Url "https://www.openstreetmap.org/#map/17/22.3/50.4"),
        schedule = Recurring (UTC 1630249485781) (Duration 3600000) (Duration 604800000) (UTC 1631720714709)
      } |> Just
    _ -> Nothing

expectedGeoLocatedEvent: Maybe Event
expectedGeoLocatedEvent = case (Event.fromString "d6d5fbd9-cb24-4dab-9dae-f97e77cc0e10",
                      User.fromString "835ef0d9-0277-4c97-b7d3-58249669a1b4") of
    (Just id, Just author) -> {
        id = id,
        owner = author,
        maxParticipants = 5,
        description = "This is a fake event happening somewhere on earth, organized by a cat - but nowhere near to your place.",
        location = GeoLocation (Latitude 12.5) (Longitude 32.45),
        schedule = Recurring (UTC 1630249485781) (Duration 3600000) (Duration 604800000) (UTC 1631720714709)
      } |> Just
    _ -> Nothing

expectedPhysicalEvent: Maybe Event
expectedPhysicalEvent = case (Event.fromString "d6d5fbd9-cb24-4dab-9dae-f97e77cc0e10",
                      User.fromString "835ef0d9-0277-4c97-b7d3-58249669a1b4") of
    (Just id, Just author) -> {
        id = id,
        owner = author,
        maxParticipants = 5,
        description = "This is a physical event with an address",
        location = Address Nothing (Just (ZipCode "E2")) (Country "UK"),
        schedule = Recurring (UTC 1630249485781) (Duration 3600000) (Duration 604800000) (UTC 1631720714709)
      } |> Just
    _ -> Nothing

toMaybe: Result Error a -> Maybe a
toMaybe x = case x of
    Err m     -> Nothing
    Ok value -> Just value