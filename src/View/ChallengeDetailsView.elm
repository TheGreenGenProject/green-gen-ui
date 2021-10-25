module View.ChallengeDetailsView exposing (challengeDetailsScreen)

import Basics as Int
import Data.Challenge exposing (Challenge, ChallengeId, ChallengeOutcomeStatus(..), ChallengeReportSummary, ChallengeStatistics, ChallengeStepReport, ChallengeStepStatus(..), SuccessMeasure, isChallengeClosed, stepReport)
import Data.Schedule as Schedule exposing (Duration(..), Schedule(..), UTCTimestamp(..), formatDuration)
import Data.User exposing (UserId)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import State.AppState exposing (AppState)
import State.Cache as Cache exposing (Cache)
import State.UserState exposing (UserInfo, UserState)
import Update.Msg exposing (Msg(..))
import Utils.DateUtils exposing (formatDate)
import View.Chart.ChartUtils exposing (rightLegendPanel)
import View.Chart.Donut as Donut
import View.Icons as Icons
import View.ScreenUtils
import View.Style exposing (bold, empty, italic, relFontSize, titledElementStyle, titledTextStyle, userStyle)
import View.Theme exposing (darkOrange, darkRed, lightGreen, lightGrey)
import View.UIStyle exposing (UIStyle)


challengeDetailsScreen: AppState -> ChallengeId -> Element Msg
challengeDetailsScreen state challengeId =
    let maybeChallenge = Cache.getChallenge state.cache challengeId
        maybeAuthor    = maybeChallenge |> Maybe.map (.author)
    in column [
        width fill
        , centerX
        , spacing 10
        , padding 20 ]
        (case (maybeChallenge, maybeAuthor) of
            (Just challenge, Just userId) -> [
                renderChallengerHeader state userId challenge
                , renderChallenge state challenge]
            _                             -> [ renderChallengeNotFoundPage state.uiStyle ]
        )

renderChallengerHeader: AppState -> UserId -> Challenge -> Element Msg
renderChallengerHeader state userId challenge =
    if isChallengeClosed state.timestamp challenge
    then el [relFontSize state.uiStyle 5, Font.italic] ("This challenge is now closed." |> text)
    else if isChallengeAccepted state.cache challenge.id
    then el [relFontSize state.uiStyle 5, Font.bold] ("You have accepted this challenge !" |> text)
    else if isChallengeFailed state.cache challenge.id
    then el [relFontSize state.uiStyle 5, Font.bold] ("You have FAILED this challenge." |> text)
    else if Cache.getChallengeOutcomeStatus state.cache challenge.id == Just Rejected
    then el [relFontSize state.uiStyle 5, Font.bold] ("You have rejected this challenge !" |> text)
    else if Cache.getChallengeOutcomeStatus state.cache challenge.id == Just Completed
    then el [relFontSize state.uiStyle 5, Font.bold] ("You have completed this challenge !" |> text)
    else paragraph [paddingXY 10 2] [
        renderNewChallengerHeader state.uiStyle state.cache userId
        , text "  "
        , renderChallengeButtons state.uiStyle challenge]

renderNewChallengerHeader: UIStyle -> Cache -> UserId -> Element Msg
renderNewChallengerHeader ui cache userId =
    let maybeUser        = Cache.getUser cache userId
        pseudo           = maybeUser |> Maybe.map (.pseudo) |> Maybe.withDefault "<Unknown>"
    in row [relFontSize ui 5, Font.bold] ["Will you accept the challenge from  " |> text
            , userStyle ui pseudo (Just userId)
            , " ?" |> text]

renderChallenge: AppState -> Challenge -> Element Msg
renderChallenge state challenge = let isAccepted = isChallengeAccepted state.cache challenge.id in
    row [padding 5, spacing 10, width fill] [
        el [Font.color state.uiStyle.theme.background, alignTop, alignLeft] (Icons.challenge state.uiStyle.large)
        , column [alignLeft, spacing 20, width fill] [
            titledTextStyle state.uiStyle challenge.title challenge.content
            , titledElementStyle state.uiStyle "How your success will be evaluated ?" (renderSuccessMeasure challenge.measure)
            , titledElementStyle state.uiStyle "When do you need to report ?" (renderReportSchedule state.timestamp challenge.schedule)
            , titledElementStyle state.uiStyle "How people are doing ?" (challenge.id |> Cache.getChallengeStatistics state.cache |> renderChallengeStatistics state.uiStyle challenge.measure)
            , if isAccepted
              then titledElementStyle state.uiStyle "Report" (renderReportDates state.uiStyle state.timestamp challenge (reportDates state.cache challenge.id) (reportedSteps state.cache challenge.id))
              else empty
        ]
     ]

renderSuccessMeasure: SuccessMeasure -> Element Msg
renderSuccessMeasure measure = column [] [
    (if measure.maxFailure==0
    then "- You cannot fail any attempt"
    else "- You can fail up to " ++ (measure.maxFailure |> String.fromInt) ++ " attempt(s)") |> text,
    (if measure.maxSkip==0
     then "- You cannot skip any attempt"
     else "- You can skip up to " ++ (measure.maxSkip |> String.fromInt) ++ " attempt(s)") |> text,
    (if measure.maxPartial==0
     then "- You cannot report any partial success"
     else "- You can report up to " ++ (measure.maxPartial |> String.fromInt) ++ " partial success(es)") |> text
 ]

renderReportSchedule: UTCTimestamp -> Schedule -> Element Msg
renderReportSchedule (UTC now) schedule = case schedule of
    OneOff _ end                 -> "One-off, report after " ++ (end |> formatDate) |> text
    Recurring start _ repeat end -> formatDuration repeat                           |> text

renderChallengeStatistics: UIStyle -> SuccessMeasure -> Maybe ChallengeStatistics -> Element Msg
renderChallengeStatistics ui measure maybeStats = case maybeStats of
    Nothing    -> "Not available" |> italic
    Just stats -> let totalReport = stats.successCount + stats.failureCount + stats.skippedCount + stats.partialSuccessCount
                      successRate = if stats.elapsedPeriodCount==0 then 100
                                    else if totalReport == 0 then 0
                                    else ((stats.successCount |> Int.toFloat) + (0.5 * (stats.partialSuccessCount |> Int.toFloat))) / (totalReport |> Int.toFloat) * 100.0
        in
        row [spacing 20] [
            renderChallengeStatisticsDonut ui stats
                |> rightLegendPanel ui [relFontSize ui 0] [
                    ("Skipped: " ++ String.fromInt stats.skippedCount, lightGrey)
                    , ("Failed: " ++ String.fromInt stats.failureCount, darkRed)
                    , ("Partial: " ++ String.fromInt stats.partialSuccessCount, darkOrange)
                    , ("Success: " ++ String.fromInt stats.successCount, lightGreen)
                  ]
              , column [relFontSize ui 0, centerY, height fill] [
                "- There is currently " ++ (stats.acceptedCount |> String.fromInt) ++ " contestant(s)." |> text
                ,"- " ++ (stats.rejectedCount |> String.fromInt) ++ " have rejected this challenge." |> text
                ,"- " ++ (stats.elapsedPeriodCount |> String.fromInt) ++ " report period(s) have passed. " ++
                    (stats.totalPeriodCount - stats.elapsedPeriodCount |> String.fromInt) ++ " remaining." |> text
                ,"- At the moment, success rate is " ++ (successRate |> String.fromFloat) ++ "%." |> text
              ]
            ]

renderChallengeStatisticsDonut: UIStyle -> ChallengeStatistics -> Element Msg
renderChallengeStatisticsDonut ui stats = let contestants = stats.acceptedCount
                                              total   = (stats.elapsedPeriodCount * contestants) |> Int.toFloat
                                              success = (stats.successCount |> Int.toFloat) / total
                                              skipped = (stats.skippedCount |> Int.toFloat) / total
                                              partial = (stats.partialSuccessCount |> Int.toFloat) / total
                                              failure = (stats.failureCount |> Int.toFloat) / total
    in Donut.smallDonut ui [(skipped, lightGrey),(failure, darkRed), (partial, darkOrange),(success, lightGreen)]
        |>  el [centerX, centerY, padding 5]

renderReportDates: UIStyle -> UTCTimestamp -> Challenge -> List UTCTimestamp -> List ChallengeStepReport -> Element Msg
renderReportDates ui now challenge dates reported =
    let cutoff = cutoffReportDate now dates
    in column [width fill, height fill, spacing 2] (dates |> List.indexedMap (\index date ->
        row [width fill, spacing 10, padding 2] [
            formatDate date |> (if Schedule.before date cutoff then bold else italic)
            , if Schedule.before date cutoff
              then renderStepReportButtons ui challenge (index+1) (stepReport (index+1) reported |> Maybe.map .status)
              else empty
        ]
     ))

renderChallengeButtons: UIStyle -> Challenge -> Element Msg
renderChallengeButtons ui challenge = row [width fill, spacing 10] [
     Input.button [relFontSize ui 1, paddingXY 2 2, Border.width 1, Border.rounded 5] {
     onPress = challenge.id |> AcceptChallenge |> Just
     , label = (text "Accept")
   }
   , Input.button [relFontSize ui 1, paddingXY 2 2, Border.width 1, Border.rounded 5] {
        onPress = challenge.id |> RejectChallenge |> Just
        , label = (text "Reject")
   }
 ]

renderFollowingButton: UIStyle -> Cache -> UserId -> Element Msg
renderFollowingButton ui cache userId =
    if Cache.containsFollowingUser cache userId then unfollowButtonStyle ui userId else followButtonStyle ui userId

followButtonStyle: UIStyle -> UserId -> Element Msg
followButtonStyle ui id =
    Input.button [relFontSize ui 1
                  , paddingXY 2 2
                  , Border.width 1
                  , Border.rounded 5]
        { onPress = Just (FollowUser id), label = Element.text "Follow" }

unfollowButtonStyle: UIStyle -> UserId -> Element Msg
unfollowButtonStyle ui id =
    Input.button [relFontSize ui 1
                  , paddingXY 2 2
                  , Border.width 1
                  , Border.rounded 5]
        { onPress = Just (UnfollowUser id), label = Element.text "Unfollow" }

renderChallengeNotFoundPage: UIStyle -> Element Msg
renderChallengeNotFoundPage ui = View.ScreenUtils.emptyScreen ui "Challenge cannot be found"

renderStepReportButtons: UIStyle -> Challenge -> Int -> Maybe ChallengeStepStatus -> Element Msg
renderStepReportButtons ui challenge index maybeReported =
    let measure = challenge.measure
        challengeId = challenge.id
    in row [width fill] [
      successButton ui challengeId index maybeReported
      , if measure.maxPartial > 0 then partialSuccessButton ui challengeId index maybeReported else empty
      , if measure.maxSkip    > 0 then skipButton ui challengeId index maybeReported           else empty
      , failureButton ui challengeId index maybeReported
    ]


successButton: UIStyle -> ChallengeId -> Int -> Maybe ChallengeStepStatus -> Element Msg
successButton ui id index maybeStatus =
    reportButton ui "Success  |" (ReportChallengeStepStatus id index Success) (maybeStatus == Just Success)

skipButton: UIStyle -> ChallengeId -> Int -> Maybe ChallengeStepStatus -> Element Msg
skipButton ui id index maybeStatus =
    reportButton ui "Skip  |" (ReportChallengeStepStatus id index Skipped) (maybeStatus == Just Skipped)

partialSuccessButton: UIStyle -> ChallengeId -> Int -> Maybe ChallengeStepStatus -> Element Msg
partialSuccessButton ui id index maybeStatus =
    reportButton ui "Partial success  |" (ReportChallengeStepStatus id index PartialSuccess) (maybeStatus == Just PartialSuccess)

failureButton: UIStyle -> ChallengeId -> Int -> Maybe ChallengeStepStatus -> Element Msg
failureButton ui id index maybeStatus =
    reportButton ui "Failure" (ReportChallengeStepStatus id index Failure)  (maybeStatus == Just Failure)

reportButton: UIStyle -> String -> Msg -> Bool -> Element Msg
reportButton ui label msg currentStatus =  Input.button [relFontSize ui 0
    , Font.color (if currentStatus then ui.theme.enabledButton else ui.theme.disabledButton)
    , paddingXY 2 2]
    { onPress = Just msg, label = label |> text }

isChallengeAccepted: Cache -> ChallengeId -> Bool
isChallengeAccepted cache id = Cache.getChallengeOutcomeStatus cache id
    |> Maybe.map (\x -> x == Accepted || x ==  OnTracks)
    |> Maybe.withDefault False

isChallengeFailed: Cache -> ChallengeId -> Bool
isChallengeFailed cache id = Cache.getChallengeOutcomeStatus cache id
    |> Maybe.map (\x -> x == Failed)
    |> Maybe.withDefault False

reportDates: Cache -> ChallengeId -> List UTCTimestamp
reportDates cache id = Cache.getChallengeReportDates cache id
    |> Maybe.withDefault []

reportedSteps: Cache -> ChallengeId -> List ChallengeStepReport
reportedSteps cache id = Cache.getChallengeStepReports cache id
    |> Maybe.withDefault []


-- Finds the cutoff date - to display reportable dates
cutoffReportDate: UTCTimestamp -> List UTCTimestamp -> UTCTimestamp
cutoffReportDate (UTC now) dates = case dates of
    []            -> UTC now
    [UTC x]       -> if now < x then UTC now else UTC x
    (UTC x) :: (UTC y) :: xs -> if now >=x && now < y
                                then UTC x
                                else cutoffReportDate (UTC now) ((UTC y) :: xs)
