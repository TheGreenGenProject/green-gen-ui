module View.WizardNewFreeTextPage exposing (newWizardNewFreeTextScreen)

import Data.Hashtag exposing (Hashtag(..))
import Element exposing (Element, alignLeft, alignRight, centerX, column, el, fill, height, maximum, padding, paragraph, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (labelHidden)
import State.AppState exposing (AppState)
import State.Cache as Cache exposing (Cache)
import State.FormState exposing (FormState, NewFreeTextWizardState)
import Update.Msg exposing (Msg(..))
import Utils.TextUtils as TextUtils
import View.Icons as Icons
import View.Style exposing (hashtagStyle, placeholderStyle, titledTextStyle, userStyle)
import View.Theme as Theme exposing (foreground)


newWizardNewFreeTextScreen: AppState -> Element Msg
newWizardNewFreeTextScreen state = column [
    centerX
    , width  <| maximum 600 fill
    , height <| maximum 300 fill
    , spacing 10
    , padding 10
    , Border.rounded 20 ]
   (form state)


form : AppState -> List (Element Msg)
form state =
    let posting = state.forms.newTipWizard.posting
        isCorrect = check state.forms.newFreeTextWizard |> hasError |> not
        postButtonColor = if isCorrect then Theme.enabledButton else Theme.disabledButton
    in
    [ (row [padding 5, spacing 10, width fill] [
        el [Font.color Theme.enabledButton] (Icons.post Icons.large)
        , titledTextStyle "Say what you want ..." wizardDescription 10])
    , (Input.multiline [width fill
        , height fill
        , Font.color Theme.textFieldForeground
        , Background.color Theme.textFieldBackground] {
        onChange = (updateContent state.forms.newFreeTextWizard)
        , text = (state.forms.newFreeTextWizard.content |> Maybe.withDefault "")
        , placeholder = placeholderStyle "Unleash here ..."
        , label = labelHidden "FreeText content"
        , spellcheck = True
    })
    , makeHashtagBar state.forms.newFreeTextWizard
    , makeUserBar state.cache state.forms.newFreeTextWizard
    , (Input.button [alignRight, Border.width 2, Border.rounded 5, padding 5, Font.color postButtonColor] {
        onPress = if isCorrect then Just PostNewFreeText else Nothing
        , label = (text (if posting then "Posting ..." else "Post it !"))
    })
 ]


makeHashtagBar: NewFreeTextWizardState -> Element Msg
makeHashtagBar state = paragraph [alignLeft
        , spacing 10
        , Font.color foreground
        , Font.italic
        , Font.size 12] [row [spacing 5]
    (state.content |> Maybe.withDefault ""
      |> TextUtils.hashtagsFrom
      |> List.sortBy (\(Hashtag x) -> x)
      |> List.map hashtagStyle)]

makeUserBar: Cache -> NewFreeTextWizardState -> Element Msg
makeUserBar cache state = paragraph [alignLeft
        , spacing 10
        , Font.color foreground
        , Font.italic
        , Font.size 12] [row [spacing 5]
    (state.content |> Maybe.withDefault ""
      |> TextUtils.userPseudosFrom
      |> List.map (\pseudo -> userStyle pseudo (Cache.getUserByPseudo cache pseudo)))]

updateContent: NewFreeTextWizardState -> String -> Msg
updateContent state content = FillingNewFreeTextWizard {state| content = Just content }

check: NewFreeTextWizardState -> Result String ()
check state = if state |> getContent |> String.isEmpty then Err "Please enter some content"
    else if state |> hasHashtags |> not then Err "Enter hashtags to help others to find your tip"
    else Ok ()

getContent: NewFreeTextWizardState -> String
getContent state = state.content |> Maybe.withDefault ""

hasHashtags: NewFreeTextWizardState -> Bool
hasHashtags state = state |> getContent |> TextUtils.hashtagsFrom |> List.isEmpty |> not

hasError: Result err ok -> Bool
hasError res = case res of
    Err _ -> True
    Ok _  -> False

wizardDescription =
    """You can use the # symbol in your text to provide hashtags, the @ symbol to reference a user pseudo.
    Source references are enclosed between curly braces. {}.
    Your post must contain at least one hashtag."""