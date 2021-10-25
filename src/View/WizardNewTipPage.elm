module View.WizardNewTipPage exposing (newWizardNewTipScreen)

import Data.Hashtag exposing (Hashtag(..))
import Element exposing (Element, alignLeft, alignRight, column, el, fill, height, padding, paragraph, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (labelHidden)
import State.AppState exposing (AppState)
import State.Cache as Cache exposing (Cache)
import State.FormState exposing (FormState, NewTipWizardState)
import Update.Msg exposing (Msg(..))
import Utils.TextUtils as TextUtils
import View.Icons as Icons
import View.Style exposing (hashtagStyle, placeholderStyle, relFontSize, titledTextStyle, userStyle)
import View.Theme
import View.UIStyle exposing (UIStyle)


newWizardNewTipScreen: AppState -> Element Msg
newWizardNewTipScreen state = column [
    alignLeft
    , width fill
    , height fill
    , spacing 10
    , padding 10
    , Border.rounded 20 ]
   (form state)


form : AppState -> List (Element Msg)
form state =
    let posting = state.forms.newTipWizard.posting
        isCorrect = check state.forms.newTipWizard |> hasError |> not
        postButtonColor = if isCorrect then state.uiStyle.theme.enabledButton else state.uiStyle.theme.disabledButton
    in
    [ (row [padding 5, spacing 10, width fill] [
        el [Font.color state.uiStyle.theme.enabledButton] (Icons.tip state.uiStyle.large)
        , titledTextStyle state.uiStyle "Enter your tip" wizardDescription])
    , (Input.multiline [width fill, height fill, Font.color state.uiStyle.theme.textFieldForeground, Background.color state.uiStyle.theme.textFieldBackground] {
        onChange = (updateContent state.forms.newTipWizard)
        , text = (state.forms.newTipWizard.content |> Maybe.withDefault "")
        , placeholder = placeholderStyle state.uiStyle "Enter your Tip !"
        , label = labelHidden "Tip content"
        , spellcheck = True
    })
    , makeHashtagBar state.uiStyle state.forms.newTipWizard
    , makeUserBar state.uiStyle state.cache state.forms.newTipWizard
    , (Input.button [alignRight, Border.width 2, Border.rounded 5, padding 5, Font.color postButtonColor] {
        onPress = if isCorrect then Just PostNewTip else Nothing
        , label = (text (if posting then "Posting ..." else "Post your tip !"))
    })
 ]


makeHashtagBar: UIStyle -> NewTipWizardState -> Element Msg
makeHashtagBar ui state = paragraph [alignLeft
        , spacing 10
        , Font.color ui.theme.foreground
        , Font.italic
        , relFontSize ui 2] [row [spacing 5]
    (state.content |> Maybe.withDefault ""
      |> TextUtils.hashtagsFrom
      |> List.sortBy (\(Hashtag x) -> x)
      |> List.map (hashtagStyle ui))]

makeUserBar: UIStyle -> Cache -> NewTipWizardState -> Element Msg
makeUserBar ui cache state = paragraph [alignLeft
        , spacing 10
        , Font.color ui.theme.foreground
        , Font.italic
        , relFontSize ui 2] [row [spacing 5]
    (state.content |> Maybe.withDefault ""
      |> TextUtils.userPseudosFrom
      |> List.map (\pseudo -> userStyle ui pseudo (Cache.getUserByPseudo cache pseudo)))]

updateContent: NewTipWizardState -> String -> Msg
updateContent state content = FillingNewTipWizard {state| content = Just content }

check: NewTipWizardState -> Result String ()
check state = if state |> getContent |> String.isEmpty then Err "Please enter some content"
    else if state |> hasHashtags |> not then Err "Enter hashtags to help others to find your tip"
    else Ok ()

getContent: NewTipWizardState -> String
getContent state = state.content |> Maybe.withDefault ""

hasHashtags: NewTipWizardState -> Bool
hasHashtags state = state |> getContent |> TextUtils.hashtagsFrom |> List.isEmpty |> not

hasError: Result err ok -> Bool
hasError res = case res of
    Err _ -> True
    Ok _  -> False

wizardDescription =
    """You can use the # symbol in your text to provide hashtags, the @ symbol to reference a user pseudo.
    Source references are enclosed between curly braces. {}.
    Your post must contain at least one hashtag."""