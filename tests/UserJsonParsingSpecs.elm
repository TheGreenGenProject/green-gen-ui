module UserJsonParsingSpecs exposing (suite)

import Data.Authentication exposing (Authentication(..))
import Data.Schedule exposing (UTCTimestamp(..))
import Data.Token as Token exposing (Token(..))
import Data.User as User exposing (UserId)
import Expect
import Json.Decode as Decoder exposing (Error)
import Query.Json.UserDecoder exposing (decodeAuthentication, decodeUserProfile)
import State.UserState exposing (UserInfo)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Json decoders"
        [ describe "should decode correctly Authentication result: "
            [
              test "NotAuthenticated" <|
                \_ ->
                    """{ "NotAuthenticated": {} }"""
                        |> Decoder.decodeString decodeAuthentication
                        |> Expect.equal (Ok NotAuthenticated),
              test "Authenticated" <|
                \_ ->
                    """{
                        "Authenticated": {
                            "token": { "value": { "uuid": "076cace0-1394-444a-ad57-cbb4cd65378d" } },
                            "user": { "value": { "uuid": "992c8dff-e4fe-4078-84ab-4f745fc77afd" } },
                             "validUntil": { "value": 1607887088835 } }
                    }"""
                        |> Decoder.decodeString decodeAuthentication
                        |> Expect.equal expectedAuthenticated
            ],

          describe "should decode correctly"
            [ test "a user profile into UserInfo" <|
                \_ ->
                    let json = """{
                           "id": {"value": { "uuid": "cd635065-634c-4527-af07-e0be0485ab00"} },
                           "pseudo": { "value": "Chris" },
                           "since": { "value": 1609344955907 },
                           "intro": "The Boss !",
                           "verified": false
                       }""" in
                    (Token.fromString "076cace0-1394-444a-ad57-cbb4cd65378d")
                    |> Maybe.andThen (\tok -> (Decoder.decodeString (decodeUserProfile tok) json|> toMaybe))
                    |> Expect.equal expectedUserInfo
            ]
        ]

expectedAuthenticated: Result Error Authentication
expectedAuthenticated = makeAuthenticatedFrom
    (Token.fromString "076cace0-1394-444a-ad57-cbb4cd65378d")
    (User.fromString "992c8dff-e4fe-4078-84ab-4f745fc77afd")
    (UTC 1607887088835)

makeAuthenticatedFrom: Maybe Token -> Maybe UserId -> UTCTimestamp -> Result Error Authentication
makeAuthenticatedFrom tok id tmstp = case (tok, id, tmstp) of
    (Just token, Just userId, timestamp) -> Authenticated token userId timestamp |> Ok
    _                                    -> NotAuthenticated |> Ok

expectedUserInfo: Maybe UserInfo
expectedUserInfo = makeUserInfoFrom
    (Token.fromString "076cace0-1394-444a-ad57-cbb4cd65378d")
    (User.fromString "cd635065-634c-4527-af07-e0be0485ab00")
    (UTC 1609344955907)


makeUserInfoFrom: Maybe Token -> Maybe UserId -> UTCTimestamp -> Maybe UserInfo
makeUserInfoFrom tok id tmstp = case (tok, id, tmstp) of
    (Just token, Just userId, timestamp) -> {
            id = userId,
            pseudo = "Chris",
            introduction = "The Boss !",
            enabled = True,
            since = timestamp,
            token = token
        } |> Just
    _   -> Nothing

toMaybe: Result Error a -> Maybe a
toMaybe x = case x of
    Err m     -> Nothing
    Ok value -> Just value