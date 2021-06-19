module Query.CacheQueryUtils exposing (
    fetchAndCacheUserInfo
    , fetchAndCachePostInfo
    , fetchAndCacheAllUsersFromPosts
    , fetchAndCacheAllUsersFromNotifications
    , fetchAndCacheAllPosts
    , fetchFromIdAndCacheAll
    , fetchAndCacheFollowingUsers
    , fetchAndCacheFollowers
    , fetchAndCacheScoreBreakdown
    , fetchAndCacheLikeForPost
    , fetchAndCacheLikes
    , fetchAndCachePins
    , fetchAndCachePinnedForPost
    , fetchAndCacheAllMessageCounts
    , fetchAndCacheAll
    , fetchAndCacheChallengeStatistics
    , fetchAndCachePoll
    , fetchAndCachePollStatistics)

import Data.Challenge as Challenge exposing (ChallengeId)
import Data.Conversation exposing (Message, MessageId)
import Data.Event exposing (EventId)
import Data.Notification exposing (Notification)
import Data.Poll as Poll exposing (PollId)
import Data.Post as Post exposing (Post, PostContent(..), PostId)
import Data.Tip as Tip exposing (TipId)
import Data.User as User exposing (UserId)
import Http
import Json.Decode as Decoder exposing (bool, int, list)
import Query.Json.ChallengeDecoder exposing (decodeChallenge, decodeChallengeStatistics)
import Query.Json.DecoderUtils exposing (decodeIntWithDefault, jsonResolver)
import Query.Json.PollDecoder exposing (decodePoll, decodePollStats)
import Query.Json.PostDecoder exposing (decodeHashtag, decodePost)
import Query.Json.RankDecoder exposing (decodeBreakdown)
import Query.Json.TipDecoder exposing (decodeTip)
import Query.Json.UserDecoder exposing (decodeUserList, decodeUserProfile)
import Query.QueryUtils exposing (authHeader, baseUrl, fetchAllPosts)
import State.Cache as Cache exposing (Cache)
import State.UserState exposing (UserInfo)
import Task exposing (Task)
import Url.Builder exposing (absolute)


-- Cache all necessary post information from posts ids
-- This includes user and post related info
fetchFromIdAndCacheAll: Cache -> UserInfo -> List PostId -> Task Http.Error Cache
fetchFromIdAndCacheAll cache user ids = fetchAllPosts user ids
    |> Task.andThen (fetchAndCacheAll cache user)

-- Cache all necessary information from posts
-- This include user and post related info
fetchAndCacheAll: Cache -> UserInfo -> List Post -> Task Http.Error Cache
fetchAndCacheAll cache user posts = cache
    |> Task.succeed
    |> Task.andThen (\cache1 -> fetchAndCacheAllUsersFromPosts cache1 user posts)
    |> Task.andThen (\cache2 -> fetchAndCacheAllPosts cache2 user posts)
    |> Task.andThen (\cache3 -> fetchAndCacheLikes cache3 user posts)
    |> Task.andThen (\cache4 -> fetchAndCachePins cache4 user posts)
    |> Task.andThen (\cache5 -> fetchAndCacheFollowingUsers cache5 user)
    |> Task.andThen (\cache6 -> fetchAndCacheFollowers cache6 user)
    |> Task.andThen (\cache7 -> fetchAndCacheFollowingHashtags cache7 user)
    |> Task.andThen (\cache8 -> fetchAndCacheAllMessageCounts cache8 user posts)

-- Cache users we are following
fetchAndCacheFollowingUsers: Cache -> UserInfo -> Task Http.Error Cache
fetchAndCacheFollowingUsers cache user = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["following", "all", user.id |> User.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| decodeUserList
    , timeout = Nothing
  } |> Task.map (List.foldl (\id acc -> Cache.addFollowingUser acc id) cache)

-- Cache followers from user
fetchAndCacheFollowers: Cache -> UserInfo -> Task Http.Error Cache
fetchAndCacheFollowers cache user = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["followers", "all", user.id |> User.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| decodeUserList
    , timeout = Nothing
  } |> Task.map (List.foldl (\id acc -> Cache.addFollower acc id) cache)

-- Cache hashtags we are following
fetchAndCacheFollowingHashtags: Cache -> UserInfo -> Task Http.Error Cache
fetchAndCacheFollowingHashtags cache user = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["hashtag", "followed"] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| list decodeHashtag
    , timeout = Nothing
  } |> Task.map (List.foldl (\id acc -> Cache.addFollowingHashtag acc id) cache)

fetchAndCacheScoreBreakdown: Cache -> UserInfo -> UserId -> Task Http.Error Cache
fetchAndCacheScoreBreakdown cache user targetId = Http.task {
    method     = "GET"
    , headers  = [authHeader user]
    , url      = baseUrl ++ absolute ["rank", "breakdown", targetId |> User.toString] []
    , body     = Http.emptyBody
    , resolver = jsonResolver <| decodeBreakdown
    , timeout  = Nothing
  } |> Task.map (Cache.addScoreBreakdown cache targetId)

-- Cache all user information related to the posts + follower list
fetchAndCacheAllUsersFromPosts: Cache -> UserInfo -> List Post -> Task Http.Error Cache
fetchAndCacheAllUsersFromPosts cache user posts = posts
    |> List.map (\post -> post.author)
    |> List.map (\id -> fetchAndCacheUserInfo cache user id)
    |> Task.sequence
    |> Task.andThen (\xs -> List.foldl Cache.merge cache xs |> Task.succeed)

fetchAndCacheAllUsersFromNotifications: Cache -> UserInfo -> List Notification -> Task Http.Error Cache
fetchAndCacheAllUsersFromNotifications cache user notifs = notifs
    |> Data.Notification.usersFromNotifications
    |> List.map (\id -> fetchAndCacheUserInfo cache user id)
    |> Task.sequence
    |> Task.andThen (\xs -> List.foldl Cache.merge cache xs |> Task.succeed)

fetchAndCacheUserInfo: Cache -> UserInfo -> UserId -> Task Http.Error Cache
fetchAndCacheUserInfo cache user id = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["user", "profile", id |> User.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| (decodeUserProfile user.token)
    , timeout = Nothing
    } |> Task.map (\res -> Cache.addUser cache res.id res)

-- Cache all necessary post information
-- This means content post related content + like count
fetchAndCacheAllPosts: Cache -> UserInfo -> List Post -> Task Http.Error Cache
fetchAndCacheAllPosts cache user posts = let updatedCache = Cache.addPostList cache posts in
    posts
    |> List.map (\post -> fetchAndCachePostInfo updatedCache user post)
    |> Task.sequence
    |> Task.andThen (\xs -> List.foldl Cache.merge updatedCache xs |> Task.succeed)

fetchAndCachePostInfo: Cache -> UserInfo -> Post -> Task Http.Error Cache
fetchAndCachePostInfo cache user post = case post.content of
    RePost id        -> fetchAndCachePost cache user id
    EventPost id     -> fetchAndCacheEvent cache user id
    ChallengePost id -> fetchAndCacheChallenge cache user id
                        |> Task.andThen (\cache1 -> fetchAndCacheChallengeStatistics cache1 user id)
    TipPost id       -> fetchAndCacheTip cache user id
    PollPost id      -> fetchAndCachePoll cache user id
                        |> Task.andThen (\cache1 -> fetchAndCachePollStatistics cache1 user id)
                        |> Task.andThen (\cache2 -> fetchAndCachePollAnswered cache2 user id)
    FreeTextPost _ _ -> Task.succeed cache -- nothing to do here

-- Caching like counts for posts and posts like by the user
fetchAndCacheLikes: Cache -> UserInfo -> List Post -> Task Http.Error Cache
fetchAndCacheLikes cache user posts = posts
    |> List.map (\post -> fetchAndCacheLikeCountForPost cache user post.id)
    |> Task.sequence
    |> Task.andThen (\xs -> List.foldl Cache.merge cache xs |> Task.succeed)
    |> Task.andThen (\cache1 -> List.map (\post -> fetchAndCacheLikeForPost cache1 user post.id) posts |> Task.sequence)
    |> Task.andThen (\xs -> List.foldl Cache.merge cache xs |> Task.succeed)

-- Fetch and cache like count for a single post
fetchAndCacheLikeCountForPost: Cache -> UserInfo -> PostId -> Task Http.Error Cache
fetchAndCacheLikeCountForPost cache user post = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["like", post |> Post.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| (decodeIntWithDefault 0)
    , timeout = Nothing
  } |> Task.map (Cache.setLikeCount cache post)

-- Fetch and cache if user has liked a given post
fetchAndCacheLikeForPost: Cache -> UserInfo -> PostId -> Task Http.Error Cache
fetchAndCacheLikeForPost cache user post = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["like", "is-liked", post |> Post.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| Decoder.bool
    , timeout = Nothing
  } |> Task.map (\isLiked -> (if isLiked then Cache.setLiked else Cache.unsetLiked) cache post)


fetchAndCachePins: Cache -> UserInfo -> List Post -> Task Http.Error Cache
fetchAndCachePins cache user posts = posts
    |> List.map (\post -> fetchAndCachePinnedForPost cache user post.id)
    |> Task.sequence
    |> Task.andThen (\xs -> List.foldl Cache.merge cache xs |> Task.succeed)


-- Fetch and cache if user has pinned a given post
fetchAndCachePinnedForPost: Cache -> UserInfo -> PostId -> Task Http.Error Cache
fetchAndCachePinnedForPost cache user post = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["pin", "is-pinned", post |> Post.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| Decoder.bool
    , timeout = Nothing
  } |> Task.map (\isPinned -> (if isPinned then Cache.addPinned else Cache.removePinned) cache post)

-- Helpers

fetchAndCacheTip: Cache -> UserInfo -> TipId -> Task Http.Error Cache
fetchAndCacheTip cache user id = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["tip", "by-id", id |> Tip.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| decodeTip
    , timeout = Nothing
    } |> Task.map (Cache.addTip cache id)

fetchAndCacheChallenge: Cache -> UserInfo -> ChallengeId -> Task Http.Error Cache
fetchAndCacheChallenge cache user id = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["challenge", "by-id", id |> Challenge.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| decodeChallenge
    , timeout = Nothing
    } |> Task.map (Cache.addChallenge cache id)

fetchAndCacheChallengeStatistics: Cache -> UserInfo -> ChallengeId -> Task Http.Error Cache
fetchAndCacheChallengeStatistics cache user id = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["challenge", "statistics", id |> Challenge.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| decodeChallengeStatistics
    , timeout = Nothing
 } |> Task.map (Cache.addChallengeStatistics cache id)

fetchAndCacheEvent: Cache -> UserInfo -> EventId -> Task Http.Error Cache
fetchAndCacheEvent cache user id = Task.succeed cache
--Http.task {
--    method = "GET"
--    , headers = [authHeader user]
--    , url = baseUrl ++ absolute ["event", "by-id", id |> Event.toString] []
--    , body = Http.emptyBody
--    , resolver = jsonResolver <| (decodeEvent)
--    , timeout = Nothing
--    } |> Task.map (\res -> Cache.addEvent cache id res)

fetchAndCachePost: Cache -> UserInfo -> PostId -> Task Http.Error Cache
fetchAndCachePost cache user id = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["post", "by-id", id |> Post.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| decodePost
    , timeout = Nothing
    } |> Task.map (Cache.addPost cache id)

fetchAndCachePoll: Cache -> UserInfo -> PollId -> Task Http.Error Cache
fetchAndCachePoll cache user id = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["poll", "by-id", id |> Poll.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| decodePoll
    , timeout = Nothing
 } |> Task.map (Cache.addPoll cache id)

fetchAndCachePollStatistics: Cache -> UserInfo -> PollId -> Task Http.Error Cache
fetchAndCachePollStatistics cache user id = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["poll", "statistics", id |> Poll.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| decodePollStats
    , timeout = Nothing
 } |> Task.map (Cache.addPollStats cache id)

fetchAndCachePollAnswered: Cache -> UserInfo -> PollId -> Task Http.Error Cache
fetchAndCachePollAnswered cache user id = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["poll", "has-answered", id |> Poll.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| bool
    , timeout = Nothing
 } |> Task.map (Cache.addPollAnswered cache id)

fetchAndCacheAllMessageCounts: Cache -> UserInfo -> List Post -> Task Http.Error Cache
fetchAndCacheAllMessageCounts cache user posts = posts
    |> List.map (\post -> post.id)
    |> List.map (\id -> fetchAndCacheMessageCount cache user id)
    |> Task.sequence
    |> Task.andThen (\xs -> List.foldl Cache.merge cache xs |> Task.succeed)

fetchAndCacheMessageCount: Cache -> UserInfo -> PostId -> Task Http.Error Cache
fetchAndCacheMessageCount cache user id = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["conversation", "messages", "count", id |> Post.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| int
    , timeout = Nothing
 } |> Task.map (Cache.addConversationSize cache id)
