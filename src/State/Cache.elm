module State.Cache exposing (..)

import Data.Challenge exposing (Challenge, ChallengeId, ChallengeOutcomeStatus(..), ChallengeReportSummary, ChallengeStatistics, ChallengeStatus, ChallengeStepReport)
import Data.Event exposing (Event, EventId)
import Data.Hashtag exposing (Hashtag)
import Data.Post exposing (PinnedPost(..), Post, PostContent, PostId)
import Data.Rank exposing (ScoreBreakdown)
import Data.Schedule exposing (UTCTimestamp)
import Data.Tip exposing (Tip, TipId)
import Data.User exposing (UserId)
import Dict exposing (Dict)
import Set exposing (Set)
import State.UserState exposing (UserInfo)
import Utils.DictUtils as DictUtils
import Utils.MaybeUtils as MaybeUtils

type alias PostKey      = String
type alias UserKey      = String
type alias EventKey     = String
type alias TipKey       = String
type alias ChallengeKey = String
type alias HashtagKey   = String


type alias Cache = {
    posts: Dict PostKey Post,
    users: Dict UserKey UserInfo,
    scores: Dict UserKey ScoreBreakdown,
    hashtagTrend: Maybe (List (Int, Hashtag)),
    events: Dict EventKey Event,
    tips: Dict TipKey Tip,
    challenges: Dict ChallengeKey ChallengeCacheEntry,
    followers: Set UserKey,
    followingUsers: Set UserKey,
    followingHashtags: Set HashtagKey,
    likeCount: Dict PostKey Int,
    liked: Set PostKey,
    pinned: Set PostKey
  }


empty: Cache
empty = {
    posts                    = Dict.empty,
    users                    = Dict.empty,
    scores                   = Dict.empty,
    hashtagTrend             = Nothing,
    events                   = Dict.empty,
    tips                     = Dict.empty,
    challenges               = Dict.empty,
    followers                = Set.empty,
    followingUsers           = Set.empty,
    followingHashtags        = Set.empty,
    likeCount                = Dict.empty,
    liked                    = Set.empty,
    pinned                   = Set.empty
  }


type alias ChallengeCacheEntry = {
    challenge: Maybe Challenge,
    status: Maybe ChallengeStatus,
    summary: Maybe ChallengeReportSummary,
    statistics: Maybe ChallengeStatistics,
    outcomeStatus: Maybe ChallengeOutcomeStatus,
    reportDates: List UTCTimestamp,
    stepReports: List ChallengeStepReport
 }

emptyChallengeEntry = {
    challenge = Nothing,
    status = Nothing,
    summary = Nothing,
    statistics = Nothing,
    outcomeStatus = Nothing,
    reportDates = [],
    stepReports = []
 }

merge: Cache -> Cache -> Cache
merge a b = {
    posts                    = Dict.union a.posts b.posts,
    users                    = Dict.union a.users b.users,
    scores                   = Dict.union a.scores b.scores,
    hashtagTrend             = a.hashtagTrend |> MaybeUtils.orElse b.hashtagTrend,
    events                   = Dict.union a.events b.events,
    tips                     = Dict.union a.tips b.tips,
    challenges               = DictUtils.merge mergeChallengeCacheEntries a.challenges b.challenges,
    followers                = Set.union a.followers b.followers,
    followingUsers           = Set.union a.followingUsers b.followingUsers,
    followingHashtags        = Set.union a.followingHashtags b.followingHashtags,
    likeCount                = mergeLikeCounts a.likeCount b.likeCount,
    liked                    = Set.union a.liked b.liked,
    pinned                   = Set.union a.pinned b.pinned
 }

-- FIXME Not ideal as it just keep the highest count ...
mergeLikeCounts: Dict PostKey Int -> Dict PostKey Int -> Dict PostKey Int
mergeLikeCounts a b = let maxValue = (\key -> max (Dict.get key a |> Maybe.withDefault 0) (Dict.get key b |> Maybe.withDefault 0))
                          keys =  Dict.keys a ++ Dict.keys b
    in List.foldl (\key acc -> Dict.insert key (maxValue key) acc) Dict.empty keys

{-- Posts --}
addPost: Cache -> PostId -> Post -> Cache
addPost cache id post = {cache | posts = cache.posts |> Dict.insert (Data.Post.toString id) post }

addPostList: Cache -> List Post -> Cache
addPostList = List.foldl (\post acc -> addPost acc post.id post)

getPost: Cache -> PostId -> Maybe Post
getPost cache id = Dict.get (Data.Post.toString id) cache.posts

getPostList: Cache -> List PostId -> List Post
getPostList cache ids = Debug.log ("Get cached post list " ++ (Debug.toString ids)
  ++ " from set of " ++ (Debug.toString (Dict.keys cache.posts))) (ids
    |> List.map (\id -> case getPost cache id of
        Just x  -> [x]
        Nothing -> [ ]
    ) |> List.concat)

{-- Users --}
addUser: Cache -> UserId -> UserInfo -> Cache
addUser cache id content = {cache | users =  cache.users |> Dict.insert (Data.User.toString id) content }

getUser: Cache -> UserId -> Maybe UserInfo
getUser cache id = Dict.get (Data.User.toString id) cache.users

getUserPseudo: Cache -> UserId -> Maybe String
getUserPseudo cache id = cache.users
    |> Dict.get (Data.User.toString id)
    |> Maybe.map (\x -> x.pseudo)

-- Slow implementation here
getUserByPseudo: Cache -> String -> Maybe UserId
getUserByPseudo cache pseudo = cache.users
    |> Dict.toList
    |> List.filter (\(k,v) -> v.pseudo == pseudo)
    |> List.head
    |> Maybe.map (\(_,v) -> v.id)

{-- Users --}
addScoreBreakdown: Cache -> UserId -> ScoreBreakdown -> Cache
addScoreBreakdown cache id content = {cache | scores = cache.scores |> Dict.insert (Data.User.toString id) content }

getScoreBreakdown: Cache -> UserId -> Maybe ScoreBreakdown
getScoreBreakdown cache id = Dict.get (Data.User.toString id) cache.scores

{-- Hahstags --}
updateHashtagTrend: Cache -> List (Int, Hashtag) -> Cache
updateHashtagTrend cache trend = {cache| hashtagTrend = Just trend}

{-- Events --}
addEvent: Cache -> EventId -> Event -> Cache
addEvent cache id content = {cache | events =  cache.events |> Dict.insert (Data.Event.toString id) content }

getEvent: Cache -> EventId -> Maybe Event
getEvent cache id = Dict.get (Data.Event.toString id) cache.events

{-- Tips --}
addTip: Cache -> TipId -> Tip -> Cache
addTip cache id content = {cache | tips =  cache.tips |> Dict.insert (Data.Tip.toString id) content }

getTip: Cache -> TipId -> Maybe Tip
getTip cache id = Dict.get (Data.Tip.toString id) cache.tips

{-- Challenge --}
addChallenge: Cache -> ChallengeId -> Challenge -> Cache
addChallenge cache id content = let cacheId = (Data.Challenge.toString id)
                                    entry = Dict.get cacheId cache.challenges |> Maybe.withDefault emptyChallengeEntry
                                    updated = {entry| challenge = Just content }
    in {cache| challenges = Dict.insert cacheId updated cache.challenges }

getChallenge: Cache -> ChallengeId -> Maybe Challenge
getChallenge cache id = Dict.get (Data.Challenge.toString id) cache.challenges
    |> Maybe.andThen (.challenge)

addChallengeStatus: Cache -> ChallengeId -> ChallengeStatus -> Cache
addChallengeStatus cache id status = let cacheId = (Data.Challenge.toString id)
                                         entry = Dict.get cacheId cache.challenges |> Maybe.withDefault emptyChallengeEntry
                                         updated = {entry| status = Just status }
    in {cache| challenges = Dict.insert cacheId updated cache.challenges }

getChallengeStatus: Cache -> ChallengeId -> Maybe ChallengeStatus
getChallengeStatus cache id = Dict.get (Data.Challenge.toString id) cache.challenges
    |> Maybe.andThen (.status)

addChallengeOutcomeStatus: Cache -> ChallengeId -> ChallengeOutcomeStatus -> Cache
addChallengeOutcomeStatus cache id status = let cacheId = (Data.Challenge.toString id)
                                                entry = Dict.get cacheId cache.challenges |> Maybe.withDefault emptyChallengeEntry
                                                updated = {entry| outcomeStatus = Just status }
    in {cache| challenges = Dict.insert cacheId updated cache.challenges }

getChallengeOutcomeStatus: Cache -> ChallengeId -> Maybe ChallengeOutcomeStatus
getChallengeOutcomeStatus cache id = Dict.get (Data.Challenge.toString id) cache.challenges
    |> Maybe.andThen (.outcomeStatus)

addChallengeReportDates: Cache -> ChallengeId -> List UTCTimestamp -> Cache
addChallengeReportDates cache id dates = let cacheId = (Data.Challenge.toString id)
                                             entry = Dict.get cacheId cache.challenges |> Maybe.withDefault emptyChallengeEntry
                                             updated = {entry| reportDates = dates }
    in {cache| challenges = Dict.insert cacheId updated cache.challenges }

getChallengeReportDates: Cache -> ChallengeId -> Maybe (List UTCTimestamp)
getChallengeReportDates cache id = Dict.get (Data.Challenge.toString id) cache.challenges
    |> Maybe.map (.reportDates)

addChallengeStepReports: Cache -> ChallengeId -> List ChallengeStepReport -> Cache
addChallengeStepReports cache id steps = let cacheId = (Data.Challenge.toString id)
                                             entry = Dict.get cacheId cache.challenges |> Maybe.withDefault emptyChallengeEntry
                                             updated = {entry| stepReports = steps }
    in {cache| challenges = Dict.insert cacheId updated cache.challenges }

updateChallengeStepReports: Cache -> ChallengeId -> ChallengeStepReport -> Cache
updateChallengeStepReports cache id update = let cacheId = (Data.Challenge.toString id)
                                                 entry = Dict.get cacheId cache.challenges |> Maybe.withDefault emptyChallengeEntry
                                                 filteredStepReport = entry.stepReports
                                                    |> List.filter (\x -> x.step /= update.step)
                                                 updated = {entry| stepReports = update :: filteredStepReport}
    in {cache| challenges = Dict.insert cacheId updated cache.challenges }

getChallengeStepReports: Cache -> ChallengeId -> Maybe (List ChallengeStepReport)
getChallengeStepReports cache id = Dict.get (Data.Challenge.toString id) cache.challenges
    |> Maybe.map (.stepReports)

addChallengeReportSummary: Cache -> ChallengeId -> ChallengeReportSummary -> Cache
addChallengeReportSummary cache id summary = let cacheId = (Data.Challenge.toString id)
                                                 entry = Dict.get cacheId cache.challenges |> Maybe.withDefault emptyChallengeEntry
                                                 updated = {entry| summary = Just summary }
    in {cache| challenges = Dict.insert cacheId updated cache.challenges }

getChallengeReportSummary: Cache -> ChallengeId -> Maybe ChallengeReportSummary
getChallengeReportSummary cache id = Dict.get (Data.Challenge.toString id) cache.challenges
    |> Maybe.andThen (.summary)

addChallengeStatistics: Cache -> ChallengeId -> ChallengeStatistics -> Cache
addChallengeStatistics cache id stats = let cacheId = (Data.Challenge.toString id)
                                            entry = Dict.get cacheId cache.challenges |> Maybe.withDefault emptyChallengeEntry
                                            updated = {entry| statistics = Just stats }
    in {cache| challenges = Dict.insert cacheId updated cache.challenges }

getChallengeStatistics: Cache -> ChallengeId -> Maybe ChallengeStatistics
getChallengeStatistics cache id = Dict.get (Data.Challenge.toString id) cache.challenges
    |> Maybe.andThen (.statistics)

containsChallengeStatistics: Cache -> ChallengeId -> Bool
containsChallengeStatistics cache id = Dict.get (Data.Challenge.toString id) cache.challenges
    |> Maybe.andThen (.statistics)
    |> MaybeUtils.isDefined

{-- Followers --}
addFollower: Cache -> UserId -> Cache
addFollower cache user = {cache| followers = cache.followers |> Set.insert (Data.User.toString user) }

removeFollower: Cache -> UserId -> Cache
removeFollower cache user = {cache| followers = cache.followers |> Set.remove (Data.User.toString user) }

containsFollower: Cache -> UserId -> Bool
containsFollower cache user = Set.member (Data.User.toString user) cache.followers

{-- Following --}
addFollowingUser: Cache -> UserId -> Cache
addFollowingUser cache user = {cache| followingUsers = cache.followingUsers |> Set.insert (Data.User.toString user) }

removeFollowingUser: Cache -> UserId -> Cache
removeFollowingUser cache user = {cache| followingUsers = cache.followingUsers |> Set.remove (Data.User.toString user) }

containsFollowingUser: Cache -> UserId -> Bool
containsFollowingUser cache user = Set.member (Data.User.toString user) cache.followingUsers

addFollowingHashtag: Cache -> Hashtag -> Cache
addFollowingHashtag cache hashtag = {cache| followingHashtags = cache.followingHashtags |> Set.insert (Data.Hashtag.toString hashtag) }

removeFollowingHashtag: Cache -> Hashtag -> Cache
removeFollowingHashtag cache hashtag = {cache| followingHashtags = cache.followingHashtags |> Set.remove (Data.Hashtag.toString hashtag) }

containsFollowingHashtag: Cache -> Hashtag -> Bool
containsFollowingHashtag cache hashtag = Set.member (Data.Hashtag.toString hashtag) cache.followingHashtags

{-- Liked --}
addLike: Cache -> PostId -> Cache
addLike cache postId = {cache|
    likeCount = cache.likeCount |> Dict.update (Data.Post.toString postId) increment
    , liked = cache.liked |> Set.insert (Data.Post.toString postId)
  }

removeLike: Cache -> PostId -> Cache
removeLike cache postId = {cache|
    likeCount = cache.likeCount |> Dict.update (Data.Post.toString postId) decrement
    , liked = cache.liked |> Set.remove (Data.Post.toString postId)
  }

setLikeCount: Cache -> PostId -> Int -> Cache
setLikeCount cache postId count = {cache|
    likeCount = cache.likeCount |> Dict.insert (Data.Post.toString postId) count
  }

getLikeCount: Cache -> PostId -> Int
getLikeCount cache postId = cache.likeCount
    |> Dict.get (Data.Post.toString postId)
    |> Maybe.withDefault 0

containsLike: Cache -> PostId -> Bool
containsLike cache postId = Set.member (Data.Post.toString postId) cache.liked

setLiked: Cache -> PostId -> Cache
setLiked cache postId = Debug.log ("Set post " ++ (Data.Post.toString postId) ++ " as liked") {cache|
    liked = cache.liked |> Set.insert (Data.Post.toString postId)
  }

unsetLiked: Cache -> PostId -> Cache
unsetLiked cache postId = Debug.log ("Set post " ++ (Data.Post.toString postId) ++ " as NOT liked") {cache|
    liked = cache.liked |> Set.remove (Data.Post.toString postId)
  }

{-- Pinned --}
addPinned: Cache -> PostId -> Cache
addPinned cache postId = {cache|
    pinned = cache.pinned |> Set.insert (Data.Post.toString postId)
  }

addPostPinned: Cache -> PinnedPost -> Cache
addPostPinned cache (PinnedPost id _) = {cache|
    pinned = cache.pinned |> Set.insert (Data.Post.toString id)
  }

addAllPostPinned: Cache -> List PinnedPost -> Cache
addAllPostPinned cache ids = List.foldl (\pp c -> addPostPinned c pp) cache ids

removePinned: Cache -> PostId -> Cache
removePinned cache postId = {cache|
    pinned = cache.pinned |> Set.remove (Data.Post.toString postId)
  }

containsPinned: Cache -> PostId -> Bool
containsPinned cache postId = Set.member (Data.Post.toString postId) cache.pinned


-- Helpers

increment: Maybe Int -> Maybe Int
increment counter = case counter of
    Just n  -> n+1 |> Just
    Nothing -> 1   |> Just

decrement: Maybe Int -> Maybe Int
decrement counter = case counter of
        Just n  -> (n - 1) |> Just
        Nothing -> Nothing

-- Merge 2 challenges cache entries
-- In case of collision, we take the the first entry (to be consistent with Dict.union way - but this might be inappropriate)
mergeChallengeCacheEntries: ChallengeCacheEntry -> ChallengeCacheEntry -> ChallengeCacheEntry
mergeChallengeCacheEntries a b = {
    challenge     = mergeMaybe a.challenge b.challenge,
    status        = mergeMaybe a.status b.status,
    summary       = mergeMaybe a.summary b.summary,
    statistics    = mergeMaybe a.statistics b.statistics,
    outcomeStatus = mergeMaybe a.outcomeStatus b.outcomeStatus,
    reportDates   = mergeList a.reportDates b.reportDates,
    stepReports   = mergeList a.stepReports b.stepReports
 }

mergeMaybe: Maybe a -> Maybe a -> Maybe a
mergeMaybe a b = case (a, b) of
    (Just x, Nothing)  -> Just x
    (Nothing, Just x)  -> Just x
    (Just x, Just _)   -> Just x
    (Nothing, Nothing) -> Nothing

mergeList: List a -> List a -> List a
mergeList a b = case (a, b) of
    ([], _)  -> b
    (_ , []) -> a
    _        -> a