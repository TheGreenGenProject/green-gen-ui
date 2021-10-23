module ChallengeJsonParsingSpecs exposing (suite)

import Data.Challenge as Challenge exposing (Challenge, ChallengeStepStatus(..))
import Data.Schedule exposing (Schedule(..), UTCTimestamp(..))
import Data.User as User
import Expect
import Json.Decode as Decoder exposing (Error, list)
import Query.Json.ChallengeDecoder exposing (decodeChallenge, decodeStepReport)
import Test exposing (Test, describe, test)

suite : Test
suite =
    describe "Challenge Json decoders"
        [ describe "should parse correctly"
            [
              test "a Challenge" <|
                \_ ->
                    """{
                        "id": { "value": { "uuid": "36665bda-4e1b-4989-8e5b-7738b2fa36c8" } },
                        "author": { "value": { "uuid": "c69f2aa8-b802-42da-9618-1c315a2582c2" } },
                        "created": { "value": 1608992036107 },
                        "schedule": {
                            "OneOff": {
                                "start": { "value": 1608992036107 },
                                "end": { "value": 1608995096107 }
                            }},
                        "content": {
                            "title": "One steak a day, death on its way !",
                            "description": "One steak a week no more - for the whole year"
                        },
                        "measure": {
                          "maxFailure": 0,
                          "maxPartial": 0,
                          "maxSkip": 0
                        }
                    }
                    """
                        |> Decoder.decodeString decodeChallenge |> toMaybe
                        |> Expect.equal expectedChallenge
              , test "a successful ChallengeStepReport" <|
                \_ ->
                    """[
                            {"step":4, "status": {"Partial":{}} },
                            {"step":3, "status": {"Skipped":{}} },
                            {"step":2, "status": {"Failure":{}} },
                            {"step":1, "status": {"Success":{}} }
                        ]
                    """
                        |> Decoder.decodeString (list decodeStepReport)
                        |> Expect.equal (Ok [
                            { step = 4, status = PartialSuccess},
                            { step = 3, status = Skipped},
                            { step = 2, status = Failure},
                            { step = 1, status = Success}
                           ])
            ]
        ]

expectedChallenge: Maybe Challenge
expectedChallenge =  case (Challenge.fromString "36665bda-4e1b-4989-8e5b-7738b2fa36c8",
                           User.fromString "c69f2aa8-b802-42da-9618-1c315a2582c2") of
    (Just id, Just author) -> { id = id,
                                author = author,
                                created = UTC 1608992036107,
                                schedule = OneOff (UTC 1608992036107) (UTC 1608995096107),
                                title = "One steak a day, death on its way !",
                                content = "One steak a week no more - for the whole year",
                                measure = { maxFailure = 0, maxPartial = 0, maxSkip = 0}
     } |> Just
    _                      -> Nothing

toMaybe: Result Error a -> Maybe a
toMaybe x = case x of
    Err m     -> Nothing
    Ok value -> Just value