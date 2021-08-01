module State.Cache exposing (..)

import Data.Challenge exposing (Challenge, ChallengeId, ChallengeOutcomeStatus(..), ChallengeReportSummary, ChallengeStatistics, ChallengeStatus, ChallengeStepReport)
import Data.Conversation exposing (Message, MessageId)
import Data.Event exposing (Event, EventId)
import Data.Hashtag exposing (Hashtag)
import Data.Poll exposing (Poll, PollId, PollOption, PollStats(..), updatePollStats)
import Data.Post exposing (PinnedPost(..), Post, PostContent, PostId)
import Data.Rank exposing (ScoreBreakdown)
import Data.Schedule exposing (UTCTimestamp, milliseconds)
import Data.Tip exposing (Tip, TipId)
import Data.User exposing (UserId)
import Dict exposing (Dict)
import Set exposing (Set)
import State.UserState exposing (UserInfo)
import Utils.DictUtils as DictUtils
import Utils.ListUtils as ListUtils
import Utils.MaybeUtils as MaybeUtils

type alias PostKey      = String
type alias UserKey      = String
type alias EventKey     = String
type alias TipKey       = String
type alias PollKey      = String
type alias ChallengeKey = String
type alias HashtagKey   = String
type alias MessageKey   = String


type alias Cache = {
    posts: Dict PostKey Post,
    users: Dict UserKey UserInfo,
    scores: Dict UserKey ScoreBreakdown,
    hashtagTrend: Maybe (List (Int, Hashtag)),
    events: Dict EventKey Event,
    tips: Dict TipKey Tip,
    polls: Dict PollKey PollCacheEntry,
    challenges: Dict ChallengeKey ChallengeCacheEntry,
    followers: Set UserKey,
    followingUsers: Set UserKey,
    followingHashtags: Set HashtagKey,
    likeCount: Dict PostKey Int,
    liked: Set PostKey,
    pinned: Set PostKey,
    conversations: Dict PostKey ConversationCacheEntry,
    flaggedMessages: Dict MessageKey FlaggedMessageEntry
  }


empty: Cache
empty = {
    posts                    = Dict.empty,
    users                    = Dict.empty,
    scores                   = Dict.empty,
    hashtagTrend             = Nothing,
    events                   = Dict.empty,
    tips                     = Dict.empty,
    polls                    = Dict.empty,
    challenges               = Dict.empty,
    followers                = Set.empty,
    followingUsers           = Set.empty,
    followingHashtags        = Set.empty,
    likeCount                = Dict.empty,
    liked                    = Set.empty,
    pinned                   = Set.empty,
    conversations            = Dict.empty,
    flaggedMessages          = Dict.empty
  }


type alias PollCacheEntry = {
    poll: Maybe Poll,
    answered: Maybe Bool,
    stats: Maybe PollStats
 }

emptyPollEntry = {
    poll     = Nothing,
    answered = Nothing,
    stats    = Nothing
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

type alias ConversationCacheEntry = {
    postId: Maybe PostId,
    opened: Bool,
    comment: Maybe String,
    messageCount: Int,
    messages: List Message,
    flagged: Set MessageKey
 }

emptyConversationCacheEntry = {
    postId = Nothing,
    opened = False,
    comment = Nothing,
    messageCount = 0,
    messages = [],
    flagged = Set.empty
 }

type alias FlaggedMessageEntry = {
    flagged: Bool,
    flaggedByUser: Bool
 }

emptyFlaggedMessageEntry = {
    flagged = False,
    flaggedByUser = False
 }

merge: Cache -> Cache -> Cache
merge a b = {
    posts                    = Dict.union a.posts b.posts,
    users                    = Dict.union a.users b.users,
    scores                   = Dict.union a.scores b.scores,
    hashtagTrend             = a.hashtagTrend |> MaybeUtils.orElse b.hashtagTrend,
    events                   = Dict.union a.events b.events,
    tips                     = Dict.union a.tips b.tips,
    polls                    = DictUtils.merge mergePollCacheEntries a.polls b.polls,
    challenges               = DictUtils.merge mergeChallengeCacheEntries a.challenges b.challenges,
    followers                = Set.union a.followers b.followers,
    followingUsers           = Set.union a.followingUsers b.followingUsers,
    followingHashtags        = Set.union a.followingHashtags b.followingHashtags,
    likeCount                = mergeLikeCounts a.likeCount b.likeCount,
    liked                    = Set.union a.liked b.liked,
    pinned                   = Set.union a.pinned b.pinned,
    conversations            = DictUtils.merge mergeConversationCacheEntries a.conversations b.conversations,
    flaggedMessages          = DictUtils.merge mergeFlaggedEntries a.flaggedMessages b.flaggedMessages
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
getPostList cache ids = ids
    |> List.map (\id -> case getPost cache id of
        Just x  -> [x]
        Nothing -> [ ]
    ) |> List.concat

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

{-- Polls --}
addPoll: Cache -> PollId -> Poll -> Cache
addPoll cache id content = let cacheId = (Data.Poll.toString id)
                               entry = Dict.get cacheId cache.polls |> Maybe.withDefault emptyPollEntry
                               updated = {entry| poll = Just content }
    in {cache| polls = Dict.insert cacheId updated cache.polls }

getPoll: Cache -> PollId -> Maybe Poll
getPoll cache id = Dict.get (Data.Poll.toString id) cache.polls
    |> Maybe.andThen (.poll)

addPollStats: Cache -> PollId -> PollStats -> Cache
addPollStats cache id stats = let cacheId = (Data.Poll.toString id)
                                  entry = Dict.get cacheId cache.polls |> Maybe.withDefault emptyPollEntry
                                  updated = {entry| stats = Just stats }
    in {cache| polls = Dict.insert cacheId updated cache.polls }

getPollStats: Cache -> PollId -> Maybe PollStats
getPollStats cache id = Dict.get (Data.Poll.toString id) cache.polls
    |> Maybe.andThen (.stats)

addPollAnswered: Cache -> PollId -> Bool -> Cache
addPollAnswered cache id answered = let cacheId = (Data.Poll.toString id)
                                        entry = Dict.get cacheId cache.polls |> Maybe.withDefault emptyPollEntry
                                        updated = {entry| answered = Just answered }
    in {cache| polls = Dict.insert cacheId updated cache.polls }

getPollAnswered: Cache -> PollId -> Maybe Bool
getPollAnswered cache id = Dict.get (Data.Poll.toString id) cache.polls
    |> Maybe.andThen (.answered)

-- Simulating a answer of the Poll
simulatePollAnswer: Cache -> PollId -> PollOption -> Cache
simulatePollAnswer cache pollId option =
    let answered = addPollAnswered cache pollId True
        stats = getPollStats answered pollId |> Maybe.withDefault (PollStats [])
    in addPollStats answered pollId (updatePollStats option stats)



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

-- Simulating a new step report
simulateChallengeStepReports: Cache -> ChallengeId -> ChallengeStepReport -> Cache
simulateChallengeStepReports cache challengeId update =
    let oldSteps = getChallengeStepReports cache challengeId |> Maybe.withDefault []
        newCache = updateChallengeStepReports cache challengeId update
        newSteps = getChallengeStepReports newCache challengeId |> Maybe.withDefault []
        statsToUpdate = getChallengeStatistics newCache challengeId
            |> Maybe.withDefault Data.Challenge.emptyChallengeStatistics
        updatedStats = {statsToUpdate|
            acceptedCount = statsToUpdate.acceptedCount + if List.isEmpty oldSteps then 1 else 0}
            |> Data.Challenge.adjustChallengeStatistics oldSteps newSteps
        simulatedCache = addChallengeStatistics newCache challengeId updatedStats
    in simulatedCache

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
    |> MaybeUtils.nonEmpty

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
setLiked cache postId = { cache|
    liked = cache.liked |> Set.insert (Data.Post.toString postId)
  }

unsetLiked: Cache -> PostId -> Cache
unsetLiked cache postId = { cache|
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


{-- Conversation --}
addConversationSize: Cache -> PostId -> Int -> Cache
addConversationSize cache id count =
    let cacheId = (Data.Post.toString id)
        entry = Dict.get cacheId cache.conversations |> Maybe.withDefault emptyConversationCacheEntry
        updated = {entry| messageCount = count }
    in {cache| conversations = Dict.insert cacheId updated cache.conversations }

getConversationSize: Cache -> PostId -> Int
getConversationSize cache id = Dict.get (Data.Post.toString id) cache.conversations
    |> Maybe.map (.messageCount)
    |> Maybe.withDefault 0

addConversationMessage: Cache -> PostId -> Message -> Cache
addConversationMessage cache id message =
    let cacheId = (Data.Post.toString id)
        entry = Dict.get cacheId cache.conversations |> Maybe.withDefault emptyConversationCacheEntry
        updated = {entry| messages = message :: entry.messages }
    in {cache| conversations = Dict.insert cacheId updated cache.conversations }

addConversationMessages: Cache -> PostId -> List Message -> Cache
addConversationMessages cache id messages =
    let cacheId = (Data.Post.toString id)
        entry = Dict.get cacheId cache.conversations |> Maybe.withDefault emptyConversationCacheEntry
        added = entry.messages ++ messages |> ListUtils.unique |> List.sortBy (.timestamp >> milliseconds >> negate)
        updated = {entry| messages = added }
    in {cache| conversations = Dict.insert cacheId updated cache.conversations }

getConversationMessages: Cache -> PostId -> List Message
getConversationMessages cache id = Dict.get (Data.Post.toString id) cache.conversations
    |> Maybe.map (.messages)
    |> Maybe.withDefault []

setConversationOpened: Cache -> PostId -> Bool -> Cache
setConversationOpened cache id opened =
    let cacheId = (Data.Post.toString id)
        entry = Dict.get cacheId cache.conversations |> Maybe.withDefault emptyConversationCacheEntry
        updated = {entry| opened = opened }
    in {cache| conversations = Dict.insert cacheId updated cache.conversations }

isConversationOpened: Cache -> PostId -> Bool
isConversationOpened cache id = Dict.get (Data.Post.toString id) cache.conversations
    |> Maybe.map (.opened)
    |> Maybe.withDefault False

isConversationClosed: Cache -> PostId -> Bool
isConversationClosed cache id = isConversationOpened cache id |> not

addComment: Cache -> PostId -> String -> Cache
addComment cache id comment =
    let cacheId = (Data.Post.toString id)
        entry = Dict.get cacheId cache.conversations |> Maybe.withDefault emptyConversationCacheEntry
        updated = {entry| comment = MaybeUtils.maybeString comment }
    in {cache| conversations = Dict.insert cacheId updated cache.conversations }

removeComment: Cache -> PostId -> Cache
removeComment cache id =
        let cacheId = (Data.Post.toString id)
            entry = Dict.get cacheId cache.conversations |> Maybe.withDefault emptyConversationCacheEntry
            updated = {entry| comment = Nothing }
        in {cache| conversations = Dict.insert cacheId updated cache.conversations }

getComment: Cache -> PostId -> Maybe String
getComment cache id = Dict.get (Data.Post.toString id) cache.conversations
    |> Maybe.andThen (.comment)

{-- Flagged messages --}
setFlagged: Cache -> MessageId -> Bool -> Cache
setFlagged cache id flag =
    let cacheId = (Data.Conversation.toString id)
        entry = Dict.get cacheId cache.flaggedMessages |> Maybe.withDefault emptyFlaggedMessageEntry
        updated = {entry| flagged = flag }
    in {cache| flaggedMessages = Dict.insert cacheId updated cache.flaggedMessages }

isFlagged: Cache -> MessageId -> Bool
isFlagged cache id = Dict.get (Data.Conversation.toString id) cache.flaggedMessages
    |> Maybe.map (.flagged)
    |> Maybe.withDefault False

setFlaggedByUser: Cache -> MessageId -> Bool -> Cache
setFlaggedByUser cache id flag =
    let cacheId = (Data.Conversation.toString id)
        entry = Dict.get cacheId cache.flaggedMessages |> Maybe.withDefault emptyFlaggedMessageEntry
        updated = {entry| flaggedByUser = flag , flagged = entry.flagged || flag}
    in {cache| flaggedMessages = Dict.insert cacheId updated cache.flaggedMessages }

isFlaggedByUser: Cache -> MessageId -> Bool
isFlaggedByUser cache id = Dict.get (Data.Conversation.toString id) cache.flaggedMessages
    |> Maybe.map (.flaggedByUser)
    |> Maybe.withDefault False


-- Helpers

increment: Maybe Int -> Maybe Int
increment counter = case counter of
    Just n  -> n+1 |> Just
    Nothing -> 1   |> Just

decrement: Maybe Int -> Maybe Int
decrement counter = case counter of
        Just n  -> (n - 1) |> Just
        Nothing -> Nothing

-- Merges 2 challenges cache entries
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

-- Merges 2 Poll cache entries
mergePollCacheEntries: PollCacheEntry -> PollCacheEntry -> PollCacheEntry
mergePollCacheEntries a b = {
    poll     = mergeMaybe a.poll b.poll,
    answered = mergeMaybe a.answered b.answered,
    stats    = mergeMaybe a.stats b.stats
 }

-- Merge 2 Conversation cache entries
mergeConversationCacheEntries: ConversationCacheEntry -> ConversationCacheEntry -> ConversationCacheEntry
mergeConversationCacheEntries a b = {
    postId         = mergeMaybe a.postId b.postId,
    opened         = a.opened || b.opened,
    comment        = mergeMaybe a.comment b.comment,
    messageCount   = max a.messageCount b.messageCount,
    messages       = mergeList a.messages b.messages,
    flagged        = Set.union a.flagged b.flagged
 }

mergeFlaggedEntries: FlaggedMessageEntry -> FlaggedMessageEntry -> FlaggedMessageEntry
mergeFlaggedEntries a b = {
    flagged = a.flagged || b.flagged,
    flaggedByUser = a.flaggedByUser || b.flaggedByUser
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