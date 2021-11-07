module AggregatedPostParsingSpecs exposing (suite)

import Data.Token as Token exposing (Token)
import Expect
import Json.Decode as Decoder exposing (Error)
import Query.Json.AggregatedPostDecoder exposing (decodeAggregatedPost)
import Test exposing (Test, describe, test)
import Utils.MaybeUtils as MaybeUtils

suite : Test
suite =
    describe "AggregatedPost Json decoders"
        [ describe "should decode correctly"
            [
              test "An aggregated event post" <|
                \_ ->
                    let json = """
                      {
                        "postId": { "value": { "uuid": "97197b23-d6ff-4ba0-890c-82336c0c03cc" } },
                        "post": {
                          "EventPost": {
                            "id": {
                              "value": { "uuid": "97197b23-d6ff-4ba0-890c-82336c0c03cc" }
                            },
                            "author": {
                              "value": { "uuid": "80ef56db-e13d-4af1-8a8b-62d202f110fa" }
                            },
                            "event": {
                              "value": { "uuid": "32ec8ca6-48ce-4183-a81e-62d19f603a93" }
                            },
                            "created": { "value": 1634485476216 },
                            "hashtags": [
                              { "value": "event" },
                              { "value": "meetup" },
                              { "value": "noplastic" }
                            ]
                          }
                        },
                        "user": {
                          "id": { "value": { "uuid": "80ef56db-e13d-4af1-8a8b-62d202f110fa" } },
                          "pseudo": { "value": "Chris" },
                          "since": { "value": 1634485475353 },
                          "intro": "Feels good to be the Boss !Now leading the green revolution through an amazing online platform !",
                          "verified": false
                        },
                        "pinned": false,
                        "partner": null,
                        "tip": null,
                        "freeText": null,
                        "challenge": null,
                        "event": {
                          "event": {
                            "id": { "value": { "uuid": "32ec8ca6-48ce-4183-a81e-62d19f603a93" } },
                            "owner": { "value": { "uuid": "80ef56db-e13d-4af1-8a8b-62d202f110fa" } },
                            "maxParticipants": 5,
                            "description": "This is a fake #event happening somewhere on #earth, organized by a #cat - but nowhere near to your place.",
                            "location": {
                              "MapUrl": {
                                "url": { "url": "https://www.openstreetmap.org/search?query=London#map=17/51.52246/-0.07352" }
                              }
                            },
                            "schedule": {
                              "OneOff": {
                                "start": { "value": 1634744675961 },
                                "end": { "value": 1634831075961 }
                              }
                            }
                          },
                          "cancelled": false,
                          "participationStatus": false,
                          "participationRequestStatus": false,
                          "participationCount": 1
                        },
                        "poll": null,
                        "repost": null,
                        "liked" : false,
                        "likes": { "value": 0 },
                        "messageCount": 0
                      }
                    """
                        tok = token
                    in tok |> Maybe.andThen (\tk -> (Decoder.decodeString (decodeAggregatedPost tk) json |> toMaybe))
                           |> MaybeUtils.nonEmpty
                           |> Expect.equal True,

              test "An aggregated challenge post" <|
                \_ ->
                    let json = """
                      {
                        "postId": { "value": { "uuid": "24d23744-e78a-42ff-aebd-3a95724cce17" }},
                        "post": {
                          "ChallengePost": {
                            "id": { "value": { "uuid": "24d23744-e78a-42ff-aebd-3a95724cce17" }},
                            "author": { "value": { "uuid": "80ef56db-e13d-4af1-8a8b-62d202f110fa" }},
                            "challenge": { "value": { "uuid": "5255bc53-c91f-4d6a-bb5f-a6a71e93fd6b" }},
                            "created": { "value": 1634485476201 },
                            "hashtags": [
                              { "value": "verdura" },
                              { "value": "ichallengeyou" },
                              { "value": "friendsbattle" },
                              { "value": "noplastic" }
                            ]
                          }
                        },
                        "user": {
                          "id": { "value": { "uuid": "80ef56db-e13d-4af1-8a8b-62d202f110fa" } },
                          "pseudo": { "value": "Chris" },
                          "since": { "value": 1634485475353 },
                          "intro": "Feels good to be the Boss ! Now leading the green revolution through an amazing online platform !",
                          "verified": false
                        },
                        "pinned": false,
                        "partner": null,
                        "tip": null,
                        "freeText": null,
                        "challenge": {
                          "challenge": {
                            "id": { "value": { "uuid": "5255bc53-c91f-4d6a-bb5f-a6a71e93fd6b" }},
                            "author": { "value": { "uuid": "80ef56db-e13d-4af1-8a8b-62d202f110fa" }},
                            "created": { "value": 1634485475956 },
                            "schedule": {
                              "Recurring": {
                                "first": { "value": 1633880675955 },
                                "duration": { "millis": 86400000 },
                                "every": { "millis": 86400000 },
                                "until": { "value": 1635090275955 }
                              }
                            },
                            "content": {
                              "title": "One steak a day, death on its way !",
                              "description": "One steak a week no more - for the whole year"
                            },
                            "measure": {
                              "maxFailure": 1,
                              "maxSkip": 1,
                              "maxPartial": 1
                            }
                          },
                          "status": {
                            "OnGoing": {}
                          },
                          "statusOutcome": {
                            "NotTaken": {}
                          },
                          "statistics": {
                            "acceptedCount": 0,
                            "rejectedCount": 0,
                            "elapsedPeriodCount": 8,
                            "totalPeriodCount": 15,
                            "successCount": 0,
                            "failureCount": 0,
                            "partialSuccessCount": 0,
                            "skippedCount": 0
                          }
                        },
                        "event": null,
                        "poll": null,
                        "repost": null,
                        "liked" : false,
                        "likes": {
                          "value": 0
                        },
                        "messageCount": 0
                      }
                    """
                        tok = token
                    in tok |> Maybe.andThen (\tk -> (Decoder.decodeString (decodeAggregatedPost tk) json |> Debug.log "Result: " |> toMaybe))
                           |> MaybeUtils.nonEmpty
                           |> Expect.equal True,

              test "An aggregated poll post" <|
                \_ ->
                    let json = """
                      {
                        "postId": {
                          "value": {
                            "uuid": "ebf7ec35-e1e1-400a-9349-fb2cbf4dcb57"
                          }
                        },
                        "post": {
                          "PollPost": {
                            "id": { "value": { "uuid": "ebf7ec35-e1e1-400a-9349-fb2cbf4dcb57" }},
                            "author": { "value": { "uuid": "80ef56db-e13d-4af1-8a8b-62d202f110fa" }},
                            "poll": { "value": { "uuid": "69474220-a01e-4aa0-aa8b-7a0b0fa29f24" }},
                            "created": { "value": 1634485476170 },
                            "hashtags": [
                              {"value": "survey"},
                              {"value": "question"},
                              {"value": "randomquestionoftheday"},
                              {"value": "poll"}
                            ]
                          }
                        },
                        "user": {
                          "id": { "value": { "uuid": "80ef56db-e13d-4af1-8a8b-62d202f110fa"}},
                          "pseudo": { "value": "Chris" },
                          "since": { "value": 1634485475353 },
                          "intro": "Feels good to be the Boss ! Now leading the green revolution through an amazing online platform !",
                          "verified": false
                        },
                        "pinned": false,
                        "partner": null,
                        "tip": null,
                        "freeText": null,
                        "challenge": null,
                        "event": null,
                        "poll": {
                          "poll": {
                            "id": { "value": { "uuid": "69474220-a01e-4aa0-aa8b-7a0b0fa29f24" }},
                            "author": { "value": { "uuid": "80ef56db-e13d-4af1-8a8b-62d202f110fa" }},
                            "question": "Do you like cats ?",
                            "options": [
                              {"value": "Yes"},
                              {"value": "No"},
                              {"value": "Maybe"}
                            ],
                            "timestamp": { "value": 1634485475933 }
                          },
                          "answered": false,
                          "statistics": {
                            "pollId": {
                              "value": { "uuid": "69474220-a01e-4aa0-aa8b-7a0b0fa29f24" }
                            },
                            "stats": []
                          }
                        },
                        "repost": null,
                        "liked" : false,
                        "likes": { "value": 0 },
                        "messageCount": 0
                      }
                    """
                        tok = token
                    in tok |> Maybe.andThen (\tk -> (Decoder.decodeString (decodeAggregatedPost tk) json |> Debug.log "Result: " |> toMaybe))
                           |> MaybeUtils.nonEmpty
                           |> Expect.equal True,

              test "An aggregated tip post" <|
                \_ ->
                    let json = """
                      {
                        "postId": {
                          "value": {
                            "uuid": "63c22e92-13d3-4f77-96a1-e4ea6e881add"
                          }
                        },
                        "post": {
                          "TipPost": {
                            "id": {
                              "value": {
                                "uuid": "63c22e92-13d3-4f77-96a1-e4ea6e881add"
                              }
                            },
                            "author": {
                              "value": {
                                "uuid": "80ef56db-e13d-4af1-8a8b-62d202f110fa"
                              }
                            },
                            "tip": {
                              "value": {
                                "uuid": "40525e6c-0447-425e-8be9-8393054a07d3"
                              }
                            },
                            "created": {
                              "value": 1634485476152
                            },
                            "hashtags": [
                              {
                                "value": "verde"
                              },
                              {
                                "value": "something-else"
                              },
                              {
                                "value": "tip"
                              }
                            ]
                          }
                        },
                        "user": {
                          "id": {
                            "value": {
                              "uuid": "80ef56db-e13d-4af1-8a8b-62d202f110fa"
                            }
                          },
                          "pseudo": {
                            "value": "Chris"
                          },
                          "since": {
                            "value": 1634485475353
                          },
                          "intro": "Feels good to be the Boss ! Now leading the green revolution through an amazing online platform !",
                          "verified": false
                        },
                        "pinned": false,
                        "partner": null,
                        "tip": {
                          "tip": {
                            "id": {
                              "value": {
                                "uuid": "40525e6c-0447-425e-8be9-8393054a07d3"
                              }
                            },
                            "author": {
                              "value": {
                                "uuid": "80ef56db-e13d-4af1-8a8b-62d202f110fa"
                              }
                            },
                            "content": "Don't forget the tip, you, bloody french !",
                            "created": {
                              "value": 1634485475919
                            },
                            "sources": [
                              {
                                "MySelf": {}
                              }
                            ]
                          }
                        },
                        "freeText": null,
                        "challenge": null,
                        "event": null,
                        "poll": null,
                        "repost": null,
                        "liked" : false,
                        "likes": {
                          "value": 0
                        },
                        "messageCount": 0
                      }
                    """
                        tok = token
                    in tok |> Maybe.andThen (\tk -> (Decoder.decodeString (decodeAggregatedPost tk) json |> Debug.log "Result: " |> toMaybe))
                           |> MaybeUtils.nonEmpty
                           |> Expect.equal True,

              test "An aggregated free-text post" <|
                \_ ->
                    let json = """
                      {
                        "postId": {
                          "value": {
                            "uuid": "109a9187-1539-48fe-a0a4-850817f773ed"
                          }
                        },
                        "post": {
                          "FreeTextPost": {
                            "id": {
                              "value": {
                                "uuid": "109a9187-1539-48fe-a0a4-850817f773ed"
                              }
                            },
                            "author": {
                              "value": {
                                "uuid": "80ef56db-e13d-4af1-8a8b-62d202f110fa"
                              }
                            },
                            "content": "This is a free-text post about #ecology, #green, #env, #environment, #plantbased, #veganmetal, #noplanetb",
                            "sources": [
                              {
                                "MySelf": {}
                              }
                            ],
                            "created": {
                              "value": 1634485476051
                            },
                            "hashtags": [
                              {
                                "value": "noplanetb"
                              },
                              {
                                "value": "green"
                              },
                              {
                                "value": "vegan-metal"
                              },
                              {
                                "value": "environment"
                              },
                              {
                                "value": "plant-based"
                              },
                              {
                                "value": "env"
                              },
                              {
                                "value": "ecology"
                              }
                            ]
                          }
                        },
                        "user": {
                          "id": {
                            "value": {
                              "uuid": "80ef56db-e13d-4af1-8a8b-62d202f110fa"
                            }
                          },
                          "pseudo": {
                            "value": "Chris"
                          },
                          "since": {
                            "value": 1634485475353
                          },
                          "intro": "Feels good to be the Boss ! Now leading the green revolution through an amazing online platform !",
                          "verified": false
                        },
                        "pinned": false,
                        "partner": null,
                        "tip": null,
                        "freeText": {
                          "post": {
                            "id": {
                              "value": {
                                "uuid": "109a9187-1539-48fe-a0a4-850817f773ed"
                              }
                            },
                            "author": {
                              "value": {
                                "uuid": "80ef56db-e13d-4af1-8a8b-62d202f110fa"
                              }
                            },
                            "content": "This is a free-text post about #ecology, #green, #env, #environment, #plantbased, #veganmetal, #noplanetb",
                            "sources": [
                              {
                                "MySelf": {}
                              }
                            ],
                            "created": {
                              "value": 1634485476051
                            },
                            "hashtags": [
                              {
                                "value": "noplanetb"
                              },
                              {
                                "value": "green"
                              },
                              {
                                "value": "vegan-metal"
                              },
                              {
                                "value": "environment"
                              },
                              {
                                "value": "plant-based"
                              },
                              {
                                "value": "env"
                              },
                              {
                                "value": "ecology"
                              }
                            ]
                          }
                        },
                        "challenge": null,
                        "event": null,
                        "poll": null,
                        "repost": null,
                        "liked" : false,
                        "likes": {
                          "value": 0
                        },
                        "messageCount": 0
                      }
                    """
                        tok = token
                    in tok |> Maybe.andThen (\tk -> (Decoder.decodeString (decodeAggregatedPost tk) json |> Debug.log "Result: " |> toMaybe))
                           |> MaybeUtils.nonEmpty
                           |> Expect.equal True
            ]
        ]

token: Maybe Token
token = Token.fromString "076cace0-1394-444a-ad57-cbb4cd65378d"

toMaybe: Result Error a -> Maybe a
toMaybe x = case x of
    Err m     -> Nothing
    Ok value -> Just value