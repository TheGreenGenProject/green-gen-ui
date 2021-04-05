module Query.Challenge exposing (
    fetchUserChallengePosts
    , fetchChallengeDetails
    , acceptChallenge
    , rejectChallenge
    , reportStepStatus)

import Data.Challenge as Challenge exposing (ChallengeId, ChallengeStepStatus(..))
import Data.Page as Page exposing (Page)
import Data.Post exposing (PostId)
import Data.User as User
import Http
import Json.Decode exposing (list)
import Query.CacheQueryUtils exposing (fetchAndCacheChallengeStatistics, fetchFromIdAndCacheAll)
import Query.Json.ChallengeDecoder exposing (..)
import Query.Json.DecoderUtils exposing (decodeTimestamp, jsonResolver, unitDecoder)
import Query.Json.PostDecoder exposing (decodePostId)
import Query.QueryUtils exposing (authHeader, baseUrl)
import Query.TaskUtils exposing (thread)
import State.Cache as Cache exposing (Cache)
import State.ChallengeState exposing (ChallengePagedTab, ChallengeTab(..))
import State.UserState exposing (UserInfo)
import Task exposing (Task)
import Update.Msg exposing (Msg(..))
import Url.Builder exposing (absolute)


fetchUserChallengePosts: Cache -> UserInfo -> ChallengePagedTab -> Cmd Msg
fetchUserChallengePosts cache user pagedTab = case pagedTab.tab of
    OnGoingTab   -> fetchOnGoingChallengePosts cache user pagedTab.page
    FinishedTab  -> fetchFinishedChallengePosts cache user pagedTab.page
    AuthoredTab  -> fetchAuthoredChallengePosts cache user pagedTab.page
    FailedTab    -> fetchFailedChallengePosts cache user pagedTab.page
    OnTracksTab  -> fetchOnTracksChallengePosts cache user pagedTab.page
    UpcomingTab  -> fetchUpcomingChallengePosts cache user pagedTab.page
    ReportDueTab -> fetchReportDueChallengePosts cache user pagedTab.page

fetchAuthoredChallengePosts: Cache -> UserInfo -> Page -> Cmd Msg
fetchAuthoredChallengePosts cache user page =
    fetchAllAuthoredChallenges user page
    |> Task.andThen (fetchChallengesPosts user)
    |> Task.andThen (\ids -> fetchFromIdAndCacheAll cache user ids |> thread ids)
    |> Task.map (\(cache1, ids) -> (cache1, { tab = AuthoredTab, page = page}, ids))
    |> Task.attempt HttpChallengePostsFetched

fetchUpcomingChallengePosts: Cache -> UserInfo -> Page -> Cmd Msg
fetchUpcomingChallengePosts cache user page =
    fetchAllUpcomingChallenges user page
    |> Task.andThen (fetchChallengesPosts user)
    |> Task.andThen (\ids -> fetchFromIdAndCacheAll cache user ids |> thread ids)
    |> Task.map (\(cache1, ids) -> (cache1, { tab = UpcomingTab, page = page}, ids))
    |> Task.attempt HttpChallengePostsFetched

fetchReportDueChallengePosts: Cache -> UserInfo -> Page -> Cmd Msg
fetchReportDueChallengePosts cache user page =
    fetchAllReportDueChallenges user page
    |> Task.andThen (fetchChallengesPosts user)
    |> Task.andThen (\ids -> fetchFromIdAndCacheAll cache user ids |> thread ids)
    |> Task.map (\(cache1, ids) -> (cache1, { tab = ReportDueTab, page = page}, ids))
    |> Task.attempt HttpChallengePostsFetched

fetchFailedChallengePosts: Cache -> UserInfo -> Page -> Cmd Msg
fetchFailedChallengePosts cache user page =
    fetchAllFailedChallenges user page
    |> Task.andThen (fetchChallengesPosts user)
    |> Task.andThen (\ids -> fetchFromIdAndCacheAll cache user ids |> thread ids)
    |> Task.map (\(cache1, ids) -> (cache1, { tab = FailedTab, page = page}, ids))
    |> Task.attempt HttpChallengePostsFetched

fetchOnTracksChallengePosts: Cache -> UserInfo -> Page -> Cmd Msg
fetchOnTracksChallengePosts cache user page =
    fetchAllOnTracksChallenges user page
    |> Task.andThen (fetchChallengesPosts user)
    |> Task.andThen (\ids -> fetchFromIdAndCacheAll cache user ids |> thread ids)
    |> Task.map (\(cache1, ids) -> (cache1, { tab = OnTracksTab, page = page}, ids))
    |> Task.attempt HttpChallengePostsFetched

fetchOnGoingChallengePosts: Cache -> UserInfo -> Page -> Cmd Msg
fetchOnGoingChallengePosts cache user page =
    fetchAllOnGoingChallenges user page
    |> Task.andThen (fetchChallengesPosts user)
    |> Task.andThen (\ids -> fetchFromIdAndCacheAll cache user ids |> thread ids)
    |> Task.map (\(cache1, ids) -> (cache1, { tab = OnGoingTab, page = page}, ids))
    |> Task.attempt HttpChallengePostsFetched

fetchFinishedChallengePosts: Cache -> UserInfo -> Page -> Cmd Msg
fetchFinishedChallengePosts cache user page =
    fetchAllFinishedChallenges user page
    |> Task.andThen (fetchChallengesPosts user)
    |> Task.andThen (\ids -> fetchFromIdAndCacheAll cache user ids |> thread ids)
    |> Task.map (\(cache1, ids) -> (cache1, { tab = FinishedTab, page = page}, ids))
    |> Task.attempt HttpChallengePostsFetched

fetchChallengeDetails: Cache -> UserInfo -> ChallengeId -> Cmd Msg
fetchChallengeDetails cache user challengeId =
    fetchAndCacheChallengeDetails cache user challengeId
    |> Task.map (\c -> (c, challengeId))
    |> Task.attempt HttpChallengeDetailsFetched

fetchAndCacheChallengeDetails: Cache -> UserInfo -> ChallengeId -> Task Http.Error Cache
fetchAndCacheChallengeDetails cache user challengeId = cache
    |> Task.succeed
    |> Task.andThen (\cache1 -> fetchAndCacheChallenge cache1 user challengeId)
    |> Task.andThen (\cache2 -> fetchAndCacheChallengeStatus cache2 user challengeId)
    |> Task.andThen (\cache3 -> fetchAndCacheChallengeStatusForUser cache3 user challengeId)
    |> Task.andThen (\cache4 -> fetchAndCacheChallengeReportDates cache4 user challengeId)
    |> Task.andThen (\cache5 -> fetchAndCacheChallengeStepReports cache5 user challengeId)
    |> Task.andThen (\cache6 -> fetchAndCacheChallengeStatistics cache6 user challengeId)

fetchAndCacheChallenge: Cache -> UserInfo -> ChallengeId -> Task Http.Error Cache
fetchAndCacheChallenge cache user id = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["challenge", "by-id", id |> Challenge.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| decodeChallenge
    , timeout = Nothing
 } |> Task.map (\res -> Cache.addChallenge cache id res)

fetchAllAuthoredChallenges: UserInfo -> Page -> Task Http.Error (List ChallengeId)
fetchAllAuthoredChallenges user page = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["challenge", "by-author", User.toString user.id, page |> Page.number |> String.fromInt] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| list decodeChallengeId
    , timeout = Nothing
 }

fetchAllOnGoingChallenges: UserInfo -> Page -> Task Http.Error (List ChallengeId)
fetchAllOnGoingChallenges user page = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["challenge", "by-user", "on-going", User.toString user.id, page |> Page.number |> String.fromInt] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| list decodeChallengeId
    , timeout = Nothing
 }

fetchAllUpcomingChallenges: UserInfo -> Page -> Task Http.Error (List ChallengeId)
fetchAllUpcomingChallenges user page = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["challenge", "by-user", "upcoming", User.toString user.id, page |> Page.number |> String.fromInt] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| list decodeChallengeId
    , timeout = Nothing
 }

fetchAllReportDueChallenges: UserInfo -> Page -> Task Http.Error (List ChallengeId)
fetchAllReportDueChallenges user page = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["challenge", "by-user", "report-due", User.toString user.id, page |> Page.number |> String.fromInt] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| list decodeChallengeId
    , timeout = Nothing
 }

fetchAllFinishedChallenges: UserInfo -> Page -> Task Http.Error (List ChallengeId)
fetchAllFinishedChallenges user page = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["challenge", "by-user", "finished", User.toString user.id, page |> Page.number |> String.fromInt] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| list decodeChallengeId
    , timeout = Nothing
 }

fetchAllFailedChallenges: UserInfo -> Page -> Task Http.Error (List ChallengeId)
fetchAllFailedChallenges user page = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["challenge", "by-user", "failed", User.toString user.id, page |> Page.number |> String.fromInt] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| list decodeChallengeId
    , timeout = Nothing
 }

fetchAllOnTracksChallenges: UserInfo -> Page -> Task Http.Error (List ChallengeId)
fetchAllOnTracksChallenges user page = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["challenge", "by-user", "on-tracks", User.toString user.id, page |> Page.number |> String.fromInt] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| list decodeChallengeId
    , timeout = Nothing
 }

fetchChallengesPosts: UserInfo -> List ChallengeId -> Task Http.Error (List PostId)
fetchChallengesPosts user ids = ids
    |> List.map (fetchChallengesPost user)
    |> Task.sequence

fetchChallengesPost: UserInfo -> ChallengeId -> Task Http.Error PostId
fetchChallengesPost user id = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["post", "by-content", "challenge", id |> Challenge.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| decodePostId
    , timeout = Nothing
 }

fetchAndCacheChallengeStatus: Cache -> UserInfo -> ChallengeId -> Task Http.Error Cache
fetchAndCacheChallengeStatus cache user id = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["challenge", "status", id |> Challenge.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| decodeChallengeStatus
    , timeout = Nothing
 } |> Task.map (\res -> Cache.addChallengeStatus cache id res)

fetchAndCacheChallengeStatusForUser: Cache -> UserInfo -> ChallengeId -> Task Http.Error Cache
fetchAndCacheChallengeStatusForUser cache user id = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["challenge", "status", id |> Challenge.toString, "for-user", user.id |> User.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| decodeChallengeOutcomeStatus
    , timeout = Nothing
 } |> Task.map (\res -> Cache.addChallengeOutcomeStatus cache id res)

fetchAndCacheChallengeReportDates: Cache -> UserInfo -> ChallengeId -> Task Http.Error Cache
fetchAndCacheChallengeReportDates cache user id = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["challenge", "report", "dates", id |> Challenge.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| list decodeTimestamp
    , timeout = Nothing
 } |> Task.map (\res -> Cache.addChallengeReportDates cache id res)

fetchAndCacheChallengeStepReports: Cache -> UserInfo -> ChallengeId -> Task Http.Error Cache
fetchAndCacheChallengeStepReports cache user id = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["challenge", "reported", id |> Challenge.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| list decodeStepReport
    , timeout = Nothing
 } |> Task.map (\res -> Cache.addChallengeStepReports cache id (res |> List.sortBy .step))

fetchAndCacheChallengeReportSummary: Cache -> UserInfo -> ChallengeId -> Task Http.Error Cache
fetchAndCacheChallengeReportSummary cache user id = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["challenge", "summary", id |> Challenge.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| decodeChallengeReportSummary
    , timeout = Nothing
 } |> Task.map (\res -> Cache.addChallengeReportSummary cache id res)

acceptChallenge: Cache -> UserInfo -> ChallengeId -> Cmd Msg
acceptChallenge cache user challengeId = Http.task {
    method = "POST"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["challenge", "accept", challengeId |> Challenge.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| unitDecoder
    , timeout = Nothing
 } |> Task.attempt HttpChallengeAccepted

rejectChallenge: Cache -> UserInfo -> ChallengeId -> Cmd Msg
rejectChallenge cache user challengeId = Http.task {
    method = "POST"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["challenge", "reject", challengeId |> Challenge.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| unitDecoder
    , timeout = Nothing
 } |> Task.attempt HttpChallengeRejected

reportStepStatus: UserInfo -> ChallengeId -> Int -> ChallengeStepStatus -> Cmd Msg
reportStepStatus user challengeId step status = Http.task {
    method = "POST"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute [
        "challenge", "report",
        status |> stepStatusAsString,
        step |> String.fromInt,
        challengeId |> Challenge.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| unitDecoder
    , timeout = Nothing
 } |> Task.attempt HttpChallengeStepStatusReported

stepStatusAsString: ChallengeStepStatus -> String
stepStatusAsString status = case status of
    Success        -> "success"
    Failure        -> "failure"
    Skipped        -> "skipped"
    PartialSuccess -> "partial-success"
