module BasicJsonParsingSpecs exposing (suite)

import Data.Post as Post
import Data.Token as Token exposing (Token(..))
import Data.User as User
import Expect
import Json.Decode as Decoder exposing (Error)
import Query.Json.PostDecoder exposing (decodePostId)
import Test exposing (Test, describe, test)
import Query.Json.DecoderUtils exposing (..)


suite : Test
suite =
    describe "Basic Json decoders"
        [ describe "should parse correctly"
            [
              test "a Token" <|
                \_ ->
                    """
                    {
                       "value": {
                           "uuid": "076cace0-1394-444a-ad57-cbb4cd65378d"
                       }
                    }
                    """
                        |> Decoder.decodeString decodeToken |> toMaybe
                        |> Expect.equal (Token.fromString "076cace0-1394-444a-ad57-cbb4cd65378d"),
              test "a UserId" <|
                \_ ->
                    """
                    {
                       "value": {
                           "uuid": "076cace0-1394-444a-ad57-cbb4cd65378d"
                       }
                    }
                    """
                        |> Decoder.decodeString decodeUserId |> toMaybe
                        |> Expect.equal (User.fromString "076cace0-1394-444a-ad57-cbb4cd65378d"),
              test "a PostId" <|
                \_ ->
                    """
                    {
                       "value": {
                           "uuid": "076cace0-1394-444a-ad57-cbb4cd65378d"
                       }
                    }
                    """
                        |> Decoder.decodeString decodePostId |> toMaybe
                        |> Expect.equal (Post.fromString "076cace0-1394-444a-ad57-cbb4cd65378d")
            ]
        ]


toMaybe: Result Error a -> Maybe a
toMaybe x = case x of
    Err m     -> Nothing
    Ok value -> Just value