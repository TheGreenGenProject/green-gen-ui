module WallJsonParsingSpecs exposing (suite)

import Data.Page as Page exposing (Page)
import Data.User exposing (UserId)
import Data.Wall exposing (Wall)
import Expect
import Json.Decode as Decoder exposing (Decoder, Error)
import Query.Json.WallDecoder exposing (decodeWall)
import Test exposing (Test, describe, test)

suite : Test
suite =
    describe "Wall Json decoder"
        [ describe "should decode correctly"
            [
              test "an empty wall" <|
                \_ ->
                    """{
                           "user": { "value": { "uuid": "44e682d8-3636-4072-b5cd-18cb957a96fc"  } },
                            "posts": []
                        }
                    """
                          |> Decoder.decodeString (maybeDecodeWall userId Page.first) |> isDecoded
                          |> Expect.equal True,

              test "a non-empty wall" <|
                \_ ->
                    """{
                            "user": { "value": { "uuid": "44e682d8-3636-4072-b5cd-18cb957a96fc" } },
                            "posts": [
                                {"value":{"uuid":"eff33234-b5a1-42c3-a501-8891f94e9f05"}},
                                {"value":{"uuid":"5d619509-0857-4b32-9997-8f5746ec6ce7"}},
                                {"value":{"uuid":"b64cf45c-4298-4223-950b-81a40e5812fa"}},
                                {"value":{"uuid":"11718db7-eef9-4b82-8869-2e9eb6f3eff2"}}
                            ]
                        }
                    """
                        |> Decoder.decodeString (maybeDecodeWall userId Page.first) |> isDecoded
                        |> Expect.equal True
            ]
        ]

userId: Maybe UserId
userId = "3f397e2c-a930-42ce-946d-78dd50957e49" |> Data.User.fromString

maybeDecodeWall: Maybe UserId -> Page -> Decoder Wall
maybeDecodeWall maybeUserId page = case maybeUserId of
    Just id -> decodeWall id page
    Nothing -> Decoder.fail "No user id"

isDecoded: Result Error Wall -> Bool
isDecoded res = case res of
    Ok _    -> True
    Err err -> False