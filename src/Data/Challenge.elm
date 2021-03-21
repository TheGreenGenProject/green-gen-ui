module Data.Challenge exposing (..)

import Utils.ListUtils as ListUtils
import Uuid exposing (Uuid)
import Data.User exposing (UserId)
import Data.Schedule as Schedule exposing (Schedule(..), UTCTimestamp(..))

type ChallengeId = ChallengeId Uuid

type alias SuccessMeasure = {
    maxFailure: Int,
    maxPartial: Int,
    maxSkip: Int
 }

type alias Challenge = {
    id: ChallengeId,
    author: UserId,
    title: String,
    content: String,
    created: UTCTimestamp,
    schedule: Schedule,
    measure: SuccessMeasure
 }

-- Global stats for a given challenge
type alias ChallengeStatistics = {
    acceptedCount: Int,
    rejectedCount: Int,
    elapsedPeriodCount: Int,
    totalPeriodCount: Int,
    successCount: Int,
    failureCount: Int,
    partialSuccessCount: Int,
    skippedCount: Int
 }

-- General challenge status
type ChallengeStatus = NotYetStarted
    | OnGoing
    | Closed

-- Challenge status relative to a user
type ChallengeOutcomeStatus = NotYetTaken
    | NotTaken
    | Accepted
    | Rejected
    | Completed
    | OnTracks
    | Failed
    | Cancelled

type ChallengeStepStatus = Success
    | Failure
    | Skipped
    | PartialSuccess

type alias ChallengeStepReport = {
    step: Int,
    status: ChallengeStepStatus
 }

type alias ChallengeReportSummary = {
    success: Int,
    failure: Int,
    partial: Int,
    skipped: Int
 }

successOnly: SuccessMeasure
successOnly = {
    maxFailure = 0,
    maxPartial = 0,
    maxSkip = 0
 }

isChallengeStarted: UTCTimestamp -> Challenge -> Bool
isChallengeStarted (UTC now) challenge =
    let (UTC start) = Schedule.start challenge.schedule in now > start

isChallengeActive: UTCTimestamp -> Challenge -> Bool
isChallengeActive (UTC now) challenge =
    let (UTC end) = Schedule.end challenge.schedule in now < end

isChallengeClosed: UTCTimestamp -> Challenge -> Bool
isChallengeClosed now challenge =
    not (isChallengeStarted now challenge) && not (isChallengeActive now challenge)

stepReport: Int -> List ChallengeStepReport -> Maybe ChallengeStepReport
stepReport step stepReports = ListUtils.find (\x -> x.step == step) stepReports

fromUuid: Uuid -> ChallengeId
fromUuid uuid = ChallengeId uuid

toString: ChallengeId -> String
toString (ChallengeId uuid) = uuid |> Uuid.toString

fromString: String -> Maybe ChallengeId
fromString = Maybe.map ChallengeId << Uuid.fromString