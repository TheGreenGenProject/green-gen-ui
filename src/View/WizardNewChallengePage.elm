module View.WizardNewChallengePage exposing (newWizardNewChallengeScreen)

import Data.Hashtag exposing (Hashtag(..))
import Data.Schedule as Schedule exposing (Duration(..), Schedule(..))
import Element exposing (Element, alignLeft, alignRight, column, el, fill, height, padding, paragraph, px, row, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (labelHidden)
import State.AppState exposing (AppState)
import State.Cache as Cache exposing (Cache)
import State.FormState exposing (Audience(..), FormState, NewChallengeWizardState, ReportPeriod(..))
import Update.Msg exposing (Msg(..))
import Utils.DateUtils as DateUtils
import Utils.TextUtils as TextUtils
import View.Icons as Icons
import View.Style exposing (dateSpinner, hashtagStyle, intSpinner, options, placeholderStyle, relFontSize, titledTextStyle, userStyle)
import View.UIStyle exposing (UIStyle)


newWizardNewChallengeScreen: AppState -> Element Msg
newWizardNewChallengeScreen state = column [
    alignLeft
    , width fill
    , height fill
    , spacing 10
    , padding 10
    , Border.rounded 20 ]
   (form state)

form : AppState -> List (Element Msg)
form state =
    let wizardStateMaybeDates = state.forms.newChallengeWizard
        startDate = wizardStateMaybeDates.start
                |> Maybe.withDefault (DateUtils.plusDays 1 state.timestamp)
        endDate = wizardStateMaybeDates.end
                |> Maybe.withDefault (DateUtils.plusDays 8 state.timestamp)
                |> DateUtils.max (DateUtils.plusDays 1 startDate)
        wizardState = {wizardStateMaybeDates| start = Just startDate, end = Just endDate }
        posting = wizardState.posting
        isCorrect = check wizardState |> hasError |> not
        postButtonColor = if isCorrect then state.uiStyle.theme.enabledButton else  state.uiStyle.theme.disabledButton
        reportCount = countReports wizardState
        successMeasure = wizardState.successMeasure
    in
    [ (row [padding 5, spacing 10, width fill] [
        el [Font.color state.uiStyle.theme.enabledButton] (Icons.challenge state.uiStyle.large)
        , titledTextStyle state.uiStyle "Create a new Challenge" wizardDescription])
    , row [spacing 10] [
        Icons.calendar state.uiStyle.normal
            |> el [Font.color state.uiStyle.theme.background]
        , "Starts on" |> text |> el [relFontSize state.uiStyle 2, width <| px 75]
        , dateSpinner state.uiStyle (startDate)
            (\localDate -> FillingNewChallengeWizard {wizardState|
                    start = localDate
                    |> DateUtils.toUTCTimestamp }
                    |> Just)
     ]
     , row [spacing 10] [
        Icons.calendar state.uiStyle.normal
            |> el [Font.color state.uiStyle.theme.background]
        , "Ends on" |> text |> el [relFontSize state.uiStyle 2, width <| px 75]
        , dateSpinner state.uiStyle (endDate)
             (\localDate -> FillingNewChallengeWizard {wizardState|
                end = localDate
                    |> DateUtils.toUTCTimestamp }
                    |> Just)
      ]
      , row [spacing 10] [
        Icons.report state.uiStyle.normal
            |> el [Font.color state.uiStyle.theme.background]
        , "Report" |> text |> el [relFontSize state.uiStyle 2, width <| px 75]
        , options state.uiStyle [("Daily", Daily), ("Weekly", Weekly)]
            wizardState.reportPeriod
            (\opt -> FillingNewChallengeWizard {wizardState| reportPeriod = opt})
        , reportCount
            |> Maybe.map (\n -> " - " ++ (String.fromInt n) ++ " report(s)")
            |> Maybe.map (text >> el [relFontSize state.uiStyle 2])
            |> Maybe.withDefault Element.none
      ]
      , row [spacing 10] [
        Icons.followers state.uiStyle.normal
            |> el [Font.color state.uiStyle.theme.background]
        , "Audience" |> text |> el [relFontSize state.uiStyle 2, width <| px 75]
        , options state.uiStyle [("Followers", Followers), ("Me only", Specific [])]
            (if wizardState.audience == Followers then Followers else Specific [])
            (\opt -> FillingNewChallengeWizard {wizardState| audience = opt})
        ]
      , wrappedRow [spacing 10] [
        Icons.successMeasure state.uiStyle.normal
            |> el [Font.color state.uiStyle.theme.background]
        , "Evaluation"      |> text |> el [relFontSize state.uiStyle 2, width <| px 75]
        , intSpinner state.uiStyle 0 (reportCount|> Maybe.withDefault 0) 1 wizardState.successMeasure.maxFailure
                       (\val -> FillingNewChallengeWizard {wizardState| successMeasure = {successMeasure| maxFailure = val}} |> Just)
        ,"failure(s), " |> text |> el [relFontSize state.uiStyle 2]
        , intSpinner state.uiStyle 0 (reportCount |> Maybe.withDefault 0) 1 wizardState.successMeasure.maxSkip
                        (\val -> FillingNewChallengeWizard {wizardState| successMeasure = {successMeasure| maxSkip = val}} |> Just)
        ,"reports skipping" |> text |> el [relFontSize state.uiStyle 2]
        , intSpinner state.uiStyle 0 (reportCount |> Maybe.withDefault 0) 1 wizardState.successMeasure.maxPartial
                                (\val -> FillingNewChallengeWizard {wizardState| successMeasure = {successMeasure| maxPartial = val}} |> Just)
        ,"partial success(es)" |> text |> el [relFontSize state.uiStyle 2]
      ]
    , row [spacing 10, width fill] [
      Icons.challenge state.uiStyle.normal
            |> el [Font.color state.uiStyle.theme.background]
      , "Title" |> text |> el [relFontSize state.uiStyle 2, width <| px 75]
      , Input.text [width fill
                    , Font.color state.uiStyle.theme.textFieldForeground
                    , Background.color state.uiStyle.theme.textFieldBackground] {
            onChange = (updateTitle state.forms.newChallengeWizard)
            , text = (state.forms.newChallengeWizard.title |> Maybe.withDefault "")
            , placeholder = placeholderStyle state.uiStyle "Challenge title"
            , label = labelHidden "Challenge title"
        }]
    , (Input.multiline [width fill
                        , height fill
                        , Font.color state.uiStyle.theme.textFieldForeground
                        , Background.color state.uiStyle.theme.textFieldBackground] {
        onChange = (updateContent state.forms.newChallengeWizard)
        , text = (state.forms.newChallengeWizard.content |> Maybe.withDefault "")
        , placeholder = placeholderStyle state.uiStyle "Enter your Challenge description !"
        , label = labelHidden "Challenge content"
        , spellcheck = True
    })
    , makeHashtagBar state.uiStyle state.forms.newChallengeWizard
    , makeUserBar state.uiStyle state.cache state.forms.newChallengeWizard
    , (Input.button [alignRight, Border.width 2, Border.rounded 5, padding 5, Font.color postButtonColor] {
        onPress = if isCorrect then Just (PostNewChallenge wizardState) else Nothing
        , label = (text (if posting then "Posting ..." else "Post your challenge !"))
    })
 ]


makeHashtagBar: UIStyle -> NewChallengeWizardState -> Element Msg
makeHashtagBar ui state = paragraph [alignLeft
        , spacing 10
        , Font.color ui.theme.foreground
        , Font.italic
        , relFontSize ui 2] [row [spacing 5]
    (state.content |> Maybe.withDefault ""
      |> TextUtils.hashtagsFrom
      |> List.sortBy (\(Hashtag x) -> x)
      |> List.map (hashtagStyle ui))]

makeUserBar: UIStyle -> Cache -> NewChallengeWizardState -> Element Msg
makeUserBar ui cache state = paragraph [alignLeft
        , spacing 10
        , Font.color ui.theme.foreground
        , Font.italic
        , relFontSize ui 2] [row [spacing 5]
    (state.content |> Maybe.withDefault ""
      |> TextUtils.userPseudosFrom
      |> List.map (\pseudo -> userStyle ui pseudo (Cache.getUserByPseudo cache pseudo)))]

updateContent: NewChallengeWizardState -> String -> Msg
updateContent state content = FillingNewChallengeWizard {state| content = Just content }

updateTitle: NewChallengeWizardState -> String -> Msg
updateTitle state title = FillingNewChallengeWizard {state| title = Just title }

check: NewChallengeWizardState -> Result String ()
check state = if state |> getContent |> String.isEmpty then Err "Please the challenge content"
    else if state |> hasHashtags |> not then Err "Enter hashtags to help others to find your challenge"
    else Ok ()

getContent: NewChallengeWizardState -> String
getContent state = state.content |> Maybe.withDefault ""

hasHashtags: NewChallengeWizardState -> Bool
hasHashtags state = state |> getContent |> TextUtils.hashtagsFrom |> List.isEmpty |> not

hasError: Result err ok -> Bool
hasError res = case res of
    Err _ -> True
    Ok _  -> False

wizardDescription =
    """You can use the # symbol in your text to provide hashtags, the @ symbol to reference a user pseudo.
    Your post must contain at least one hashtag."""

-- Counting reports that will have to be made
countReports: NewChallengeWizardState -> Maybe Int
countReports state =
    let daily = Duration (24 * 60 * 60 * 1000)
        weekly = Duration (7 * 24 * 60 * 60 * 1000)
        maybeSchedule = case (state.start, state.end, state.reportPeriod) of
            (Just start, Just end, Weekly) -> Recurring start weekly weekly end |> Just
            (Just start, Just end, Daily)  -> Recurring start daily daily end   |> Just
            _                              -> Nothing
    in maybeSchedule |> Maybe.map (Schedule.dates >> List.length)
