module View.WizardNewPollPage exposing (newWizardNewPollScreen)

import Data.Hashtag exposing (Hashtag(..))
import Data.Poll exposing (PollOption(..))
import Element exposing (Element, alignLeft, alignRight, centerX, column, el, fill, height, maximum, padding, paragraph, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (labelHidden)
import State.AppState exposing (AppState)
import State.Cache as Cache exposing (Cache)
import State.FormState exposing (Audience(..), FormState, NewPollWizardState, ReportPeriod(..))
import Update.Msg exposing (Msg(..))
import Utils.ListUtils as ListUtils
import Utils.TextUtils as TextUtils
import View.Icons as Icons
import View.Style exposing (hashtagStyle, placeholderStyle, titledTextStyle, userStyle)
import View.Theme as Theme exposing (background, foreground)


newWizardNewPollScreen: AppState -> Element Msg
newWizardNewPollScreen state = column [
    centerX
    , width  <| maximum 600 fill
    , height <| maximum 500 fill
    , spacing 10
    , padding 10
    , Border.rounded 20 ]
   (form state)

form : AppState -> List (Element Msg)
form state =
    let pollState = state.forms.newPollWizard
        posting = pollState.posting
        isCorrect = check pollState |> hasError |> not
        postButtonColor = if isCorrect then Theme.enabledButton else Theme.disabledButton
    in
    [ (row [padding 5, spacing 10, width fill] [
        el [Font.color Theme.enabledButton] (Icons.poll Icons.large)
        , titledTextStyle "Create a new Poll" wizardDescription 10])
    , (Input.multiline [width fill
        , height <| maximum 100 fill
        , Font.color Theme.textFieldForeground
        , Background.color Theme.textFieldBackground] {
        onChange = (updateQuestion state.forms.newPollWizard)
        , text = (state.forms.newPollWizard.question |> Maybe.withDefault "")
        , placeholder = placeholderStyle "Enter your Poll question !"
        , label = labelHidden "Question"
        , spellcheck = True
    })
    , makeHashtagBar pollState
    , makeUserBar state.cache pollState] ++
    (pollState.options
        |> Maybe.withDefault ([PollOption ""])
        |> List.indexedMap (\index opt -> renderPollOptionRow pollState index opt)) ++
    [(Input.button [alignRight, Border.width 2, Border.rounded 5, padding 5, Font.color postButtonColor] {
        onPress = if isCorrect then Just (PostNewPoll pollState) else Nothing
        , label = (text (if posting then "Posting ..." else "Post your poll !"))
    })
 ]

makeHashtagBar: NewPollWizardState -> Element Msg
makeHashtagBar state = paragraph [alignLeft
        , spacing 10
        , Font.color foreground
        , Font.italic
        , Font.size 12] [row [spacing 5]
    (state.question |> Maybe.withDefault ""
      |> TextUtils.hashtagsFrom
      |> List.sortBy (\(Hashtag x) -> x)
      |> List.map hashtagStyle)]

makeUserBar: Cache -> NewPollWizardState -> Element Msg
makeUserBar cache state = paragraph [alignLeft
        , spacing 10
        , Font.color foreground
        , Font.italic
        , Font.size 12] [row [spacing 5]
    (state.question |> Maybe.withDefault ""
      |> TextUtils.userPseudosFrom
      |> List.map (\pseudo -> userStyle pseudo (Cache.getUserByPseudo cache pseudo)))]

updateQuestion: NewPollWizardState -> String -> Msg
updateQuestion state question = FillingNewPollWizard {state| question = Just question }

updatePollOption: NewPollWizardState -> Int -> String -> Msg
updatePollOption state index opt =
    let updated = ListUtils.replace (PollOption opt) index (state.options |> Maybe.withDefault [])
    in FillingNewPollWizard {state| options = Just updated }

addNewPollOption: NewPollWizardState -> Msg
addNewPollOption state =
    FillingNewPollWizard {state| options = state.options |> Maybe.map (\xs -> xs ++ [PollOption ""]) }

removePollOption: NewPollWizardState -> Int -> Msg
removePollOption state index =
    FillingNewPollWizard {state|
        options = state.options
            |> Maybe.andThen (\xs -> xs |> ListUtils.delete index |> ListUtils.nonEmptyToMaybe) }

renderPollOptionRow: NewPollWizardState -> Int -> PollOption -> Element Msg
renderPollOptionRow state index (PollOption opt) =
    let lastIndex = state.options |> Maybe.map (List.length) |> Maybe.withDefault 1
        isLast = (index+1) == lastIndex
    in
    row [spacing 5, width fill] [
        Input.text [width fill, Font.color Theme.textFieldForeground, Background.color Theme.textFieldBackground] {
            onChange = (updatePollOption state index)
            , text = opt
            , placeholder = placeholderStyle  ((index+1 |> String.fromInt) ++"- Enter another option")
            , label = labelHidden "hidden option" }
        , if isLast
          then Input.button [alignRight, Border.width 2, Border.rounded 5, padding 5, Font.color background] {
            onPress = Just (addNewPollOption state)
            , label = Icons.plus Icons.normal
          }
          else Input.button [alignRight, Border.width 2, Border.rounded 5, padding 5, Font.color background] {
            onPress = Just (removePollOption state index)
            , label = Icons.minus Icons.normal
          }
        ]

check: NewPollWizardState -> Result String ()
check state = if state |> getQuestion |> String.isEmpty then Err "Please the poll question"
    else if state |> hasHashtags |> not then Err "Enter hashtags to help others to find your poll"
    else if (state |> getPollOptionCount) <= 1 then Err "A Poll needs to have more than one answer"
    else Ok ()

getQuestion: NewPollWizardState -> String
getQuestion state = state.question |> Maybe.withDefault ""

getPollOptionCount: NewPollWizardState -> Int
getPollOptionCount state = state.options |> Maybe.map (List.length) |> Maybe.withDefault 0

hasHashtags: NewPollWizardState -> Bool
hasHashtags state = state |> getQuestion |> TextUtils.hashtagsFrom |> List.isEmpty |> not

hasError: Result err ok -> Bool
hasError res = case res of
    Err _ -> True
    Ok _  -> False

wizardDescription =
    """You can use the # symbol in your text to provide hashtags, the @ symbol to reference a user pseudo.
    Your post must contain at least one hashtag."""
