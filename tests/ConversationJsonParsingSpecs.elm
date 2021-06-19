module ConversationJsonParsingSpecs exposing (suite)


import Data.Conversation as Message exposing (Message)
import Data.Schedule exposing (UTCTimestamp(..))
import Data.User as User
import Expect
import Json.Decode as Decoder exposing (Error)
import Query.Json.ConversationDecoder exposing (decodeMessage)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Conversation Json decoders"
        [ describe "should parse correctly"
            [
              test "a Message" <|
                \_ ->
                    """{
                        "id":{"value":{"uuid":"c0cf9583-66d9-48cf-9390-1e50d60a1743"}},
                        "user":{"value":{"uuid":"fb7a7b07-8a35-436a-a79d-9043450eef55"}},
                        "content":"Boom",
                        "timestamp":{"value":1623789618841}}
                    """ |> Decoder.decodeString decodeMessage |> Debug.log "Error=" |> toMaybe
                        |> Expect.equal expectedMessage
            ]
        ]

expectedMessage: Maybe Message
expectedMessage = case (Message.fromString "c0cf9583-66d9-48cf-9390-1e50d60a1743",
                    User.fromString "fb7a7b07-8a35-436a-a79d-9043450eef55") of
    (Just id, Just author) -> {
        id = id,
        author = author,
        content = "Boom",
        timestamp = UTC 1623789618841
     } |> Just
    _ -> Nothing

toMaybe: Result Error a -> Maybe a
toMaybe x = case x of
    Err m     -> Nothing
    Ok value -> Just value