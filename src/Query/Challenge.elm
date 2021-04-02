module Query.Challenge exposing (
    fetchChallengeDetails
    , fetchAndCacheChallengeStatistics
    , acceptChallenge
    , rejectChallenge
    , reportStepStatus)

import Data.Challenge as Challenge exposing (ChallengeId, ChallengeStepStatus(..))
import Data.User as User
import Http
import Json.Decode exposing (list)
import Query.Json.ChallengeDecoder exposing (..)
import Query.Json.DecoderUtils exposing (decodeTimestamp, jsonResolver, unitDecoder)
import Query.QueryUtils exposing (authHeader, baseUrl)
import State.Cache as Cache exposing (Cache)
import State.UserState exposing (UserInfo)
import Task exposing (Task)
import Update.Msg exposing (Msg(..))
import Url.Builder exposing (absolute)


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

fetchAndCacheChallengeStatistics: Cache -> UserInfo -> ChallengeId -> Task Http.Error Cache
fetchAndCacheChallengeStatistics cache user id = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["challenge", "statistics", id |> Challenge.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| decodeChallengeStatistics
    , timeout = Nothing
 } |> Task.map (\res -> Cache.addChallengeStatistics cache id res)

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
