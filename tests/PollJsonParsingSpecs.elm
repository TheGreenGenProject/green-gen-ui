module PollJsonParsingSpecs exposing (suite)

import Data.Poll as Poll exposing (Poll, PollOption(..))
import Data.Schedule exposing (UTCTimestamp(..))
import Data.User as User
import Expect
import Json.Decode as Decoder exposing (Error)
import Query.Json.PollDecoder exposing (decodePoll)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Poll Json decoders"
        [ describe "should parse correctly"
            [
              test "a Poll" <|
                \_ ->
                    """{"id":{"value":{"uuid":"dc9d7f3e-651f-498a-a3c0-4f2c1ae122b0"}},
                        "author":{"value":{"uuid":"212d3a96-034c-4bb9-a023-fef04b8ec006"}},
                        "question":"I wonder if all that is useful. What do you think ?",
                        "options":[
                            {"value":"Maybe"},
                            {"value":"Not sure"},
                            {"value":"We will see !"},
                            {"value":"Yes, great idea !"}
                         ],
                         "timestamp":{"value":1622411388798}
                        }"""
                        |> Decoder.decodeString decodePoll |> Debug.log "Error=" |> toMaybe
                        |> Expect.equal expectedPoll
            ]
        ]

expectedPoll: Maybe Poll
expectedPoll = case (Poll.fromString "dc9d7f3e-651f-498a-a3c0-4f2c1ae122b0",
                    User.fromString "212d3a96-034c-4bb9-a023-fef04b8ec006") of
    (Just id, Just author) -> {
        id = id,
        author = author,
        title = "I wonder if all that is useful. What do you think ?",
        options = [
            PollOption "Maybe",
            PollOption "Not sure",
            PollOption "We will see !",
            PollOption "Yes, great idea !"
        ],
        created = UTC 1622411388798
      } |> Just
    _ -> Nothing

toMaybe: Result Error a -> Maybe a
toMaybe x = case x of
    Err m     -> Nothing
    Ok value -> Just value