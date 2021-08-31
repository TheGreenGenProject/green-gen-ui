module PostJsonParsingSpecs exposing (suite)

import Data.Challenge as Challenge
import Data.Event as Event
import Data.Hashtag exposing (Hashtag(..))
import Data.Post as Post exposing (Post, PostContent(..), PostId, Source(..))
import Data.Schedule exposing (UTCTimestamp(..))
import Data.Tip as Tip
import Data.User as User exposing (UserId)
import Expect
import Json.Decode as Decoder exposing (Error(..), Value)
import Query.Json.PostDecoder exposing (decodeHashtags, decodePost, decodePostContent)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Post Json decoders"
        [ describe "should decode correctly"
            [
              test "a list of hashtags" <|
                \_ ->
                    """[
                          { "value": "ecology" },
                          { "value": "green" },
                          { "value": "environment" }
                        ]"""
                        |> Decoder.decodeString decodeHashtags |> toMaybe
                        |> Expect.equal (Just [Hashtag "ecology", Hashtag "green", Hashtag "environment"]),

              test "the FreeText PostContent from the Post body" <|
                \_ ->
                     """{
                            "unrelated_field" : { "nothing": "interesting" },
                            "content": "This is a post",
                            "sources": [{ "MySelf": {}}]
                         }
                     """
                        |> Decoder.decodeString decodePostContent |> toMaybe
                        |> Expect.equal (FreeTextPost "This is a post" [MySelf] |> Just),

              test "Free text post" <|
                \_ ->
                    """{
                        "FreeTextPost": {
                            "id": { "value": { "uuid": "4ed1c260-fa8e-4540-8a3c-420ff85d9fd6" }},
                            "author": { "value": { "uuid": "f51e84c7-36c7-4cf7-9a8b-11207825e2ba"}},
                            "content": "This is a post",
                            "sources": [{ "MySelf": {} }],
                            "created": { "value": 1608589917658 },
                            "hashtags": [
                                { "value": "ecology" },
                                { "value": "green" },
                                { "value": "environment" }
                            ]
                        }
                    }
                    """
                        |> Decoder.decodeString decodePost |> toMaybe
                        |> Expect.equal expectedFreeTextPost,

              test "Tip post" <|
                \_ -> """{
                         "TipPost": {
                            "id": { "value": { "uuid": "a61ae3aa-0e12-4c53-ba71-c8f20caf2bbd" } },
                            "author": { "value": { "uuid": "c69f2aa8-b802-42da-9618-1c315a2582c2" } },
                            "tip": { "value": { "uuid": "273298e6-6cd2-4174-bd3b-7224eed993ee" } },
                            "created": { "value": 1608992036166 },
                            "hashtags": [
                                { "value": "verde" },
                                { "value": "something-else" },
                                { "value": "tip" }
                              ]
                            }
                         }
                """
                    |> Decoder.decodeString decodePost |> toMaybe
                    |> Expect.equal expectedTipPost,

              test "Challenge post" <|
                \_ -> """{
                         "ChallengePost": {
                            "id": { "value": { "uuid": "a61ae3aa-0e12-4c53-ba71-c8f20caf2bbd" } },
                            "author": { "value": { "uuid": "c69f2aa8-b802-42da-9618-1c315a2582c2" } },
                            "challenge": { "value": { "uuid": "273298e6-6cd2-4174-bd3b-7224eed993ee" } },
                            "created": { "value": 1608992036166 },
                            "hashtags": [
                                { "value": "verde" },
                                { "value": "something-else" },
                                { "value": "challenge" }
                              ]
                            }
                         }
                """
                    |> Decoder.decodeString decodePost |> toMaybe
                    |> Expect.equal expectedChallengePost,

              test "Event post" <|
                \_ -> """{
                          "EventPost": {
                            "id": { "value": {"uuid": "a61ae3aa-0e12-4c53-ba71-c8f20caf2bbd" } },
                            "author" : { "value": {"uuid": "c69f2aa8-b802-42da-9618-1c315a2582c2" } },
                            "event" : { "value": {"uuid": "9522a445-07bc-4547-b3f0-594cfd7be2ef" } },
                            "created" : { "value": 1608992036166},
                            "hashtags":[
                                {"value": "event"},
                                {"value": "meetup"},
                                {"value": "noplastic"}
                               ]
                             }
                          }
                """
                    |> Decoder.decodeString decodePost |> toMaybe
                    |> Expect.equal expectedEventPost,

                test "Repost post" <|
                    \_ -> """{
                             "RePost": {
                                "id": { "value": { "uuid": "323a06f1-76ba-412a-803e-1ef061ec9534" }},
                                "author": { "value": { "uuid": "9a0e14a6-4991-4b26-b570-ff6115ffe823" }},
                                "originalId": { "value": { "uuid": "205380ea-658a-4bbe-b26f-8130fb449098" }},
                                "created": { "value": 1609419129635 },
                                "hashtags": [
                                    { "value": "ecologia" }
                                ]}
                          }"""
                        |> Decoder.decodeString decodePost |> toMaybe
                        |> Expect.equal expectedRePost
            ]
        ]

expectedFreeTextPost: Maybe Post
expectedFreeTextPost = makePost
    (Post.fromString "4ed1c260-fa8e-4540-8a3c-420ff85d9fd6")
    (User.fromString "f51e84c7-36c7-4cf7-9a8b-11207825e2ba")
    (FreeTextPost "This is a post" [MySelf])
    (UTC 1608589917658)
    ([Hashtag "ecology", Hashtag "green", Hashtag "environment"])

expectedTipPost: Maybe Post
expectedTipPost =
    (Tip.fromString "273298e6-6cd2-4174-bd3b-7224eed993ee")
    |> Maybe.map TipPost
    |> Maybe.andThen (\tip ->
        makePost
            (Post.fromString "a61ae3aa-0e12-4c53-ba71-c8f20caf2bbd")
            (User.fromString "c69f2aa8-b802-42da-9618-1c315a2582c2")
            (tip)
            (UTC 1608992036166)
            ([Hashtag "verde", Hashtag "something-else", Hashtag "tip"]))

expectedChallengePost: Maybe Post
expectedChallengePost =
    (Challenge.fromString "273298e6-6cd2-4174-bd3b-7224eed993ee")
    |> Maybe.map ChallengePost
    |> Maybe.andThen (\challenge ->
        makePost
            (Post.fromString "a61ae3aa-0e12-4c53-ba71-c8f20caf2bbd")
            (User.fromString "c69f2aa8-b802-42da-9618-1c315a2582c2")
            (challenge)
            (UTC 1608992036166)
            ([Hashtag "verde", Hashtag "something-else", Hashtag "challenge"]))

expectedEventPost: Maybe Post
expectedEventPost =
    (Event.fromString "9522a445-07bc-4547-b3f0-594cfd7be2ef")
    |> Maybe.map EventPost
    |> Maybe.andThen (\evt ->
        makePost
            (Post.fromString "a61ae3aa-0e12-4c53-ba71-c8f20caf2bbd")
            (User.fromString "c69f2aa8-b802-42da-9618-1c315a2582c2")
            (evt)
            (UTC 1608992036166)
            ([Hashtag "event", Hashtag "meetup", Hashtag "noplastic"]))

expectedRePost: Maybe Post
expectedRePost =
    (Post.fromString "205380ea-658a-4bbe-b26f-8130fb449098")
    |> Maybe.map RePost
    |> Maybe.andThen (\repost ->
        makePost
            (Post.fromString "323a06f1-76ba-412a-803e-1ef061ec9534")
            (User.fromString "9a0e14a6-4991-4b26-b570-ff6115ffe823")
            repost
            (UTC 1609419129635)
            ([Hashtag "ecologia"]))


makePost: Maybe PostId -> Maybe UserId -> PostContent -> UTCTimestamp -> List Hashtag -> Maybe Post
makePost postId userId content tmstp hashtags = case (postId, userId) of
    (Just id, Just author) -> {
         id = id,
         author = author,
         content = content,
         created = tmstp,
         hashtags = hashtags
      } |> Just
    _                      -> Nothing

toMaybe: Result Error a -> Maybe a
toMaybe x = case x of
    Err m    -> Nothing
    Ok value -> Just value