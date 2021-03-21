module Query.Json.ChallengeDecoder exposing (
    decodeChallenge
    , decodeChallengeId
    , decodeChallengeStatus
    , decodeChallengeOutcomeStatus
    , decodeChallengeReportSummary
    , decodeChallengeStatistics
    , decodeStepReport)

import Data.Challenge exposing (Challenge, ChallengeId(..), ChallengeOutcomeStatus(..), ChallengeReportSummary, ChallengeStatistics, ChallengeStatus(..), ChallengeStepReport, ChallengeStepStatus(..), SuccessMeasure)
import Json.Decode as Decoder exposing (Decoder, int, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Query.Json.DecoderUtils exposing(..)
import Query.Json.ScheduleDecoder exposing (decodeSchedule)


decodeChallenge: Decoder Challenge
decodeChallenge = Decoder.map7 Challenge
    (Decoder.field "id" decodeChallengeId)
    (Decoder.field "author" decodeUserId)
    (decodeTitle)
    (decodeContent)
    (Decoder.field "created" decodeTimestamp)
    (Decoder.field "schedule" decodeSchedule)
    (Decoder.field "measure" decodeSuccessMeasure)

decodeChallengeId: Decoder ChallengeId
decodeChallengeId = succeed ChallengeId
    |> required "value" decodeUuid

decodeTitle: Decoder String
decodeTitle = Decoder.field "content"
    <| Decoder.field "title" string

decodeContent: Decoder String
decodeContent = Decoder.field "content"
    <| Decoder.field "description" string

decodeSuccessMeasure: Decoder SuccessMeasure
decodeSuccessMeasure = succeed SuccessMeasure
    |> required "maxFailure" int
    |> required "maxPartial" int
    |> required "maxSkip" int

decodeChallengeReportSummary: Decoder ChallengeReportSummary
decodeChallengeReportSummary = succeed ChallengeReportSummary
    |> required "success" int
    |> required "failure" int
    |> required "partial" int
    |> required "skipped" int

decodeChallengeStatistics: Decoder ChallengeStatistics
decodeChallengeStatistics = succeed ChallengeStatistics
    |> required "acceptedCount" int
    |> required "rejectedCount" int
    |> required "elapsedPeriodCount" int
    |> required "totalPeriodCount" int
    |> required "successCount" int
    |> required "failureCount" int
    |> required "partialSuccessCount" int
    |> required "skippedCount" int

decodeStepReport: Decoder ChallengeStepReport
decodeStepReport = succeed ChallengeStepReport
    |> required "step" int
    |> required "status" decodeChallengeStepStatus

decodeChallengeStatus: Decoder ChallengeStatus
decodeChallengeStatus = oneOf [
    decodeNotYetStarted
    , decodeOnGoing
    , decodeClosed
 ]

decodeNotYetStarted: Decoder ChallengeStatus
decodeNotYetStarted = Decoder.field "NotYetStarted" unitDecoder
    |> Decoder.map (\_ -> NotYetStarted)

decodeOnGoing: Decoder ChallengeStatus
decodeOnGoing = Decoder.field "OnGoing" unitDecoder
    |> Decoder.map (\_ -> OnGoing)

decodeClosed: Decoder ChallengeStatus
decodeClosed = Decoder.field "Closed" unitDecoder
    |> Decoder.map (\_ -> Closed)


decodeChallengeOutcomeStatus: Decoder ChallengeOutcomeStatus
decodeChallengeOutcomeStatus = oneOf [
    decodeNotYetTaken
    , decodeNotTaken
    , decodeAccepted
    , decodeRejected
    , decodeCompleted
    , decodeOnTracks
    , decodeFailed
    , decodeCancelled
 ]

decodeNotYetTaken: Decoder ChallengeOutcomeStatus
decodeNotYetTaken = Decoder.field "NotYetTaken" unitDecoder
    |> Decoder.map (\_ -> NotYetTaken)

decodeNotTaken: Decoder ChallengeOutcomeStatus
decodeNotTaken = Decoder.field "NotTaken" unitDecoder
    |> Decoder.map (\_ -> NotTaken)

decodeAccepted: Decoder ChallengeOutcomeStatus
decodeAccepted = Decoder.field "Accepted" unitDecoder
    |> Decoder.map (\_ -> Accepted)

decodeRejected: Decoder ChallengeOutcomeStatus
decodeRejected = Decoder.field "Rejected" unitDecoder
    |> Decoder.map (\_ -> Rejected)

decodeCompleted: Decoder ChallengeOutcomeStatus
decodeCompleted = Decoder.field "Completed" unitDecoder
    |> Decoder.map (\_ -> Completed)

decodeOnTracks: Decoder ChallengeOutcomeStatus
decodeOnTracks = Decoder.field "OnTracks" unitDecoder
    |> Decoder.map (\_ -> OnTracks)

decodeFailed: Decoder ChallengeOutcomeStatus
decodeFailed = Decoder.field "Failed" unitDecoder
    |> Decoder.map (\_ -> Failed)

decodeCancelled: Decoder ChallengeOutcomeStatus
decodeCancelled = Decoder.field "Cancelled" unitDecoder
    |> Decoder.map (\_ -> Cancelled)


decodeChallengeStepStatus: Decoder ChallengeStepStatus
decodeChallengeStepStatus = oneOf [
    decodeStepSuccess
    , decodeStepFailure
    , decodeStepSkipped
    , decodeStepPartialSuccess
 ]

decodeStepSuccess: Decoder ChallengeStepStatus
decodeStepSuccess = Decoder.field "Success" unitDecoder
    |> Decoder.map (\_ -> Success)

decodeStepFailure: Decoder ChallengeStepStatus
decodeStepFailure = Decoder.field "Failure" unitDecoder
    |> Decoder.map (\_ -> Failure)

decodeStepSkipped: Decoder ChallengeStepStatus
decodeStepSkipped = Decoder.field "Skipped" unitDecoder
    |> Decoder.map (\_ -> Skipped)

decodeStepPartialSuccess: Decoder ChallengeStepStatus
decodeStepPartialSuccess = Decoder.field "PartialSuccess" unitDecoder
    |> Decoder.map (\_ -> PartialSuccess)
