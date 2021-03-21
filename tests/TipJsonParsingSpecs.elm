module TipJsonParsingSpecs exposing (suite)

import Data.Schedule exposing (UTCTimestamp(..))
import Data.Tip as Tip exposing (Tip)
import Data.User as User
import Expect
import Json.Decode as Decoder exposing (Error)
import Query.Json.TipDecoder exposing (decodeTip)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Tip Json decoders"
        [ describe "should parse correctly"
            [
              test "a Tip" <|
                \_ ->
                    """{
                            "id": { "value": { "uuid": "96dc8a4d-5454-4e80-9127-b10c8c2be5c4" } },
                            "author": { "value": { "uuid": "b05d2145-15d0-47bc-bed6-3463791bac09" } },
                            "content": "My tip is to share tips ... a lot !",
                            "created": { "value": 1608996509582 },
                            "sources": [ { "MySelf": {} } ]
                        }"""
                        |> Decoder.decodeString decodeTip |> toMaybe
                        |> Expect.equal expectedTip
            ]
        ]

expectedTip: Maybe Tip
expectedTip = case (Tip.fromString "96dc8a4d-5454-4e80-9127-b10c8c2be5c4",
                    User.fromString "b05d2145-15d0-47bc-bed6-3463791bac09") of
    (Just id, Just author) -> { id = id,
                                author = author,
                                content = "My tip is to share tips ... a lot !",
                                created = UTC 1608996509582 } |> Just
    _ -> Nothing

toMaybe: Result Error a -> Maybe a
toMaybe x = case x of
    Err m     -> Nothing
    Ok value -> Just value