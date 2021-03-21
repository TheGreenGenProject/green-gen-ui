module View.WizardNewTipPage exposing (newWizardNewTipScreen)

import Data.Hashtag exposing (Hashtag(..))
import Element exposing (Element, alignLeft, alignRight, centerX, column, el, fill, height, maximum, padding, paragraph, row, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (labelHidden)
import State.AppState exposing (AppState)
import State.Cache as Cache exposing (Cache)
import State.FormState exposing (FormState, NewTipWizardState)
import Update.Msg exposing (Msg(..))
import Utils.TextUtils as TextUtils
import View.Icons as Icons
import View.Style exposing (hashtagStyle, placeholderStyle, titledTextStyle, userStyle)
import View.Theme as Theme exposing (black)


newWizardNewTipScreen: AppState -> Element Msg
newWizardNewTipScreen state = column [
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
        isCorrect = check state.forms.newTipWizard |> hasError |> not
        postButtonColor = if isCorrect then Theme.background else Theme.disabled
    in
    [ (row [padding 5, spacing 10, width fill] [
        el [Font.color Theme.background] (Icons.tip Icons.large)
        , titledTextStyle "Enter your tip" wizardDescription 10])
    , (Input.multiline [width fill, height fill] {
        onChange = (updateContent state.forms.newTipWizard)
        , text = (state.forms.newTipWizard.content |> Maybe.withDefault "")
        , placeholder = placeholderStyle "Enter your Tip !"
        , label = labelHidden "Tip content"
        , spellcheck = True
    })
    , makeHashtagBar state.forms.newTipWizard
    , makeUserBar state.cache state.forms.newTipWizard
    , (Input.button [alignRight, Border.width 2, Border.rounded 5, padding 5, Font.color postButtonColor] {
        onPress = if isCorrect then Just PostNewTip else Nothing
        , label = (text (if posting then "Posting ..." else "Post your tip !"))
    })
 ]


makeHashtagBar: NewTipWizardState -> Element Msg
makeHashtagBar state = paragraph [alignLeft
        , spacing 10
        , Font.color black
        , Font.italic
        , Font.size 12] [row [spacing 5]
    (state.content |> Maybe.withDefault ""
      |> TextUtils.hashtagsFrom
      |> List.sortBy (\(Hashtag x) -> x)
      |> List.map hashtagStyle)]

makeUserBar: Cache -> NewTipWizardState -> Element Msg
makeUserBar cache state = paragraph [alignLeft
        , spacing 10
        , Font.color black
        , Font.italic
        , Font.size 12] [row [spacing 5]
    (state.content |> Maybe.withDefault ""
      |> TextUtils.userPseudosFrom
      |> List.map (\pseudo -> userStyle pseudo (Cache.getUserByPseudo cache pseudo)))]

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