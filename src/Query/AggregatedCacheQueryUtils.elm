module Query.AggregatedCacheQueryUtils exposing (fetchAggregatedAndCacheAll)

import Data.Partner exposing (Partner)
import Data.Post as PostId exposing (Post, PostContent(..), PostId)
import Http
import Query.CacheQueryUtils as CacheQueryUtils
import Query.Json.AggregatedPostDecoder exposing (AggregatedPost, ChallengeInfo, EventInfo, PollInfo, TipInfo, decodeAggregatedPosts)
import Query.Json.DecoderUtils exposing (jsonResolver)
import Query.QueryUtils exposing (authHeader, baseUrl)
import State.Cache as Cache exposing (Cache)
import State.UserState exposing (UserInfo)
import Task exposing (Task)
import Url.Builder exposing (absolute, string)


-- Cache all necessary information from posts
-- This include user and post related info
fetchAggregatedAndCacheAll: Cache -> UserInfo -> List PostId -> Task Http.Error Cache
fetchAggregatedAndCacheAll cache user posts = cache
    |> Task.succeed
    |> Task.andThen (\cache1 -> fetchAndCacheAggregatedPostInfo cache1 user posts)
    |> Task.andThen (\cache2 -> CacheQueryUtils.fetchAndCacheFollowingUsers cache2 user)
    |> Task.andThen (\cache3 -> CacheQueryUtils.fetchAndCacheFollowers cache3 user)
    |> Task.andThen (\cache4 -> CacheQueryUtils.fetchAndCacheFollowingHashtags cache4 user)

fetchAndCacheAggregatedPostInfo: Cache -> UserInfo -> List PostId -> Task Http.Error Cache
fetchAndCacheAggregatedPostInfo cache user postIds = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["post", "by-ids"] [string "post-ids" (makePostIdsArg postIds)]
    , body = Http.emptyBody
    , resolver = jsonResolver <| decodeAggregatedPosts user.token
    , timeout = Nothing
 } |> Task.map (List.foldl (\id acc -> cacheAggregatedPosts acc id) cache)

makePostIdsArg: List PostId -> String
makePostIdsArg xs = xs
    |> List.map (PostId.toString)
    |> String.join "+"

-- FIXME steamline function calls ...
cacheAggregatedPosts: Cache -> AggregatedPost -> Cache
cacheAggregatedPosts cache post =
    let c1 = Cache.addPost cache post.postId post.post
        c2 = Cache.addUser c1 post.user.id post.user
        c3 = if post.liked then Cache.addLike c2 post.postId else c2
        c4 = Cache.setLikeCount c3 post.postId post.likes
        c5 = Cache.addConversationSize c4 post.postId post.messageCount
        c6 = if(post.pinned) then Cache.addPinned c5 post.postId else c5
        c7 = post.partner
            |> Maybe.map (partnership c6 post.postId)
            |> Maybe.withDefault c6
        c8 = post.event
            |> Maybe.map (cacheEventInfo c7 post.postId)
            |> Maybe.withDefault c7
        c9 = post.challenge
            |> Maybe.map (cacheChallengeInfo c8 post.postId)
            |> Maybe.withDefault c8
        c10 = post.poll
            |> Maybe.map (cachePollInfo c9 post.postId)
            |> Maybe.withDefault c9
        c11 = post.tip
            |> Maybe.map (cacheTipInfo c10)
            |> Maybe.withDefault c10
    in c11

partnership: Cache -> PostId -> Partner -> Cache
partnership cache postId partner =
    let c1 = Cache.addPartner cache partner.id partner
        c2 = Cache.addPostWithPartnership c1 postId partner.id
    in c2

cacheEventInfo: Cache -> PostId -> EventInfo -> Cache
cacheEventInfo cache postId event =
    let c1 = Cache.addEvent cache event.event.id event.event
        c2 = Cache.addEventCancelledStatus c1 event.event.id event.cancelled
        c3 = Cache.addEventParticipationStatus c2 event.event.id event.participationStatus
        c4 = Cache.addEventParticipationRequestStatus c3 event.event.id event.participationRequestStatus
        c5 = Cache.addEventParticipantCount c4 event.event.id event.participationCount
        c6 = Cache.addPostIdForEvent c5 event.event.id postId
    in c6

cacheChallengeInfo: Cache -> PostId -> ChallengeInfo -> Cache
cacheChallengeInfo cache postId challenge =
    let c1 = Cache.addChallenge cache challenge.challenge.id challenge.challenge
        c2 = Cache.addChallengeStatus c1 challenge.challenge.id challenge.status
        c3 = Cache.addChallengeOutcomeStatus c2 challenge.challenge.id challenge.statusOutcome
        c4 = Cache.addChallengeStatistics c3 challenge.challenge.id challenge.statistics
    in c4

cachePollInfo: Cache -> PostId -> PollInfo -> Cache
cachePollInfo cache postId poll =
    let c1 = Cache.addPoll cache poll.poll.id poll.poll
        c2 = Cache.addPollAnswered c1 poll.poll.id poll.answered
        c3 = Cache.addPollStats c2 poll.poll.id poll.statistics
    in c3

cacheTipInfo: Cache -> TipInfo -> Cache
cacheTipInfo cache tip =
    Cache.addTip cache tip.tip.id tip.tip