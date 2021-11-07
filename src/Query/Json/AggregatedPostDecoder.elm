module Query.Json.AggregatedPostDecoder exposing (..)


import Data.Challenge exposing (Challenge, ChallengeOutcomeStatus, ChallengeStatistics, ChallengeStatus)
import Data.Event exposing (Event)
import Data.Partner exposing (Partner)
import Data.Poll exposing (Poll, PollStats)
import Data.Post exposing (Post, PostId)
import Data.Tip exposing (Tip)
import Data.Token exposing (Token)
import Data.User exposing (User)
import Json.Decode as Decoder exposing (Decoder, bool, int, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Query.Json.ChallengeDecoder exposing (decodeChallenge, decodeChallengeOutcomeStatus, decodeChallengeStatistics, decodeChallengeStatus)
import Query.Json.DecoderUtils exposing (decodeIntWithDefault)
import Query.Json.EventDecoder exposing (decodeEvent)
import Query.Json.PartnerDecoder exposing (decodePartner)
import Query.Json.PollDecoder exposing (decodePoll, decodePollStats)
import Query.Json.PostDecoder as PostDecoder exposing (decodePost, decodePostId)
import Query.Json.TipDecoder exposing (decodeTip)
import Query.Json.UserDecoder exposing (decodeUserProfile)
import State.UserState exposing (UserInfo)


decodeAggregatedPosts: Token -> Decoder (List AggregatedPost)
decodeAggregatedPosts token = Decoder.list (decodeAggregatedPost token)


  --{
  --  "postId": {
  --    "value": {
  --      "uuid": "97197b23-d6ff-4ba0-890c-82336c0c03cc"
  --    }
  --  },
  --  "post": {
  --    "EventPost": {
  --      "id": {
  --        "value": {
  --          "uuid": "97197b23-d6ff-4ba0-890c-82336c0c03cc"
  --        }
  --      },
  --      "author": {
  --        "value": {
  --          "uuid": "80ef56db-e13d-4af1-8a8b-62d202f110fa"
  --        }
  --      },
  --      "event": {
  --        "value": {
  --          "uuid": "32ec8ca6-48ce-4183-a81e-62d19f603a93"
  --        }
  --      },
  --      "created": {
  --        "value": 1634485476216
  --      },
  --      "hashtags": [
  --        {
  --          "value": "event"
  --        },
  --        {
  --          "value": "meetup"
  --        },
  --        {
  --          "value": "noplastic"
  --        }
  --      ]
  --    }
  --  },
  --  "user": {
  --    "id": {
  --      "value": {
  --        "uuid": "80ef56db-e13d-4af1-8a8b-62d202f110fa"
  --      }
  --    },
  --    "pseudo": {
  --      "value": "Chris"
  --    },
  --    "since": {
  --      "value": 1634485475353
  --    },
  --    "intro": "Feels good to be the Boss !\nNow leading the green revolution through an amazing online platform !",
  --    "verified": false
  --  },
  --  "pinned": false,
  --  "partner": null,
  --  "tip": null,
  --  "freeText": null,
  --  "challenge": null,
  --  "event": {
  --    "event": {
  --      "id": {
  --        "value": {
  --          "uuid": "32ec8ca6-48ce-4183-a81e-62d19f603a93"
  --        }
  --      },
  --      "owner": {
  --        "value": {
  --          "uuid": "80ef56db-e13d-4af1-8a8b-62d202f110fa"
  --        }
  --      },
  --      "maxParticipants": 5,
  --      "description": "This is a fake #event happening somewhere on #earth, organized by a #cat - but nowhere near to your place.",
  --      "location": {
  --        "MapUrl": {
  --          "url": {
  --            "url": "https://www.openstreetmap.org/search?query=London#map=17/51.52246/-0.07352"
  --          }
  --        }
  --      },
  --      "schedule": {
  --        "OneOff": {
  --          "start": {
  --            "value": 1634744675961
  --          },
  --          "end": {
  --            "value": 1634831075961
  --          }
  --        }
  --      }
  --    },
  --    "cancelled": false,
  --    "participationStatus": false,
  --    "participationRequestStatus": false,
  --    "participationCount": 1
  --  },
  --  "poll": null,
  --  "repost": null,
  --  "liked": false,
  --  "likes": {
  --    "value": 0
  --  },
  --  "messageCount": 0
  --},

type alias AggregatedPost = {
    postId: PostId,
    post: Post,
    user: UserInfo,
    pinned: Bool,
    partner: Maybe Partner,
    tip: Maybe TipInfo,
    freeText: Maybe FreeTextInfo,
    challenge: Maybe ChallengeInfo,
    event: Maybe EventInfo,
    poll: Maybe PollInfo,
    repost: Maybe RepostInfo,
    liked: Bool,
    likes: Int,
    messageCount: Int
 }

type alias EventInfo = {
    event: Event,
    cancelled: Bool,
    participationStatus: Bool,
    participationRequestStatus: Bool,
    participationCount: Int
 }

type alias ChallengeInfo = {
    challenge: Challenge,
    status: ChallengeStatus,
    statusOutcome: ChallengeOutcomeStatus,
    statistics: ChallengeStatistics
 }

type alias PollInfo = {
    poll: Poll,
    answered: Bool,
    statistics: PollStats
 }

type alias TipInfo = {
    tip: Tip
 }

type alias RepostInfo = {
    repost: PostId
 }

type alias FreeTextInfo = {
    content: String
 }


decodeAggregatedPost: Token -> Decoder AggregatedPost
decodeAggregatedPost token = succeed AggregatedPost
    |> required "postId" decodePostId
    |> required "post" decodePost
    |> required "user" (decodeUserProfile token)
    |> required "pinned" bool
    |> required "partner" (maybe decodePartner)
    |> required "tip" (maybe decodeTipInfo)
    |> required "freeText" (maybe decodeFreeTextInfo)
    |> required "challenge" (maybe decodeChallengeInfo)
    |> required "event" (maybe decodeEventInfo)
    |> required "poll" (maybe decodePollInfo)
    |> required "repost" (maybe decodeRepostInfo)
    |> required "liked" bool
    |> required "likes" (decodeIntWithDefault 0)
    |> required "messageCount" int

decodeEventInfo: Decoder EventInfo
decodeEventInfo = succeed EventInfo
    |> required "event" decodeEvent
    |> required "cancelled" bool
    |> required "participationStatus" bool
    |> required "participationRequestStatus" bool
    |> required "participationCount" int

decodeChallengeInfo: Decoder ChallengeInfo
decodeChallengeInfo = succeed ChallengeInfo
    |> required "challenge" decodeChallenge
    |> required "status" decodeChallengeStatus
    |> required "statusOutcome" decodeChallengeOutcomeStatus
    |> required "statistics" decodeChallengeStatistics

decodePollInfo: Decoder PollInfo
decodePollInfo = succeed PollInfo
    |> required "poll" decodePoll
    |> required "answered" bool
    |> required "statistics" decodePollStats

decodeTipInfo: Decoder TipInfo
decodeTipInfo = succeed TipInfo
    |> required "tip" decodeTip

decodeRepostInfo: Decoder RepostInfo
decodeRepostInfo = succeed RepostInfo
    |> required "repost" decodePostId

decodeFreeTextInfo: Decoder FreeTextInfo
decodeFreeTextInfo = succeed FreeTextInfo
    |> required "content" string
