module View.WizardRepostPage exposing (newWizardRepostScreen)

import Element exposing (Element, alignRight, centerX, column, el, fill, height, maximum, padding, row, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import State.AppState exposing (AppState)
import State.FormState exposing (FormState, NewRepostWizardState, NewTipWizardState)
import Update.Msg exposing (Msg(..))
import Utils.MaybeUtils as MaybeUtils
import View.Icons as Icons
import View.PostRenderer exposing (renderPostId)
import View.Style exposing (titledTextStyle)
import View.Theme as Theme


newWizardRepostScreen: AppState -> Element Msg
newWizardRepostScreen state = column [
    centerX
    , width  <| maximum 600 fill
    , height <| maximum 300 fill
    , spacing 10
    , padding 10
    , Border.rounded 20 ]
   (form state)

form : AppState -> List (Element Msg)
form state =
    let posting = state.forms.newRepostWizard.posting
        postId = state.forms.newRepostWizard.repost
        isCorrect = check state.forms.newRepostWizard |> hasError |> not
        postButtonColor = if isCorrect then Theme.background else Theme.disabled
    in
    [ (row [padding 5, spacing 10, width fill] [
        el [Font.color Theme.background] (Icons.repost Icons.large)
        , titledTextStyle "Repost" wizardDescription 10])
    , postId |> Maybe.map (renderPostId state.timestamp state.cache)
             |> Maybe.withDefault Element.none
    , (Input.button [alignRight, Border.width 2, Border.rounded 5, padding 5, Font.color postButtonColor] {
        onPress = if isCorrect then Just PostNewRepost else Nothing
        , label = (text (if posting then "Reposting ..." else "Repost"))
    })
 ]

check: NewRepostWizardState -> Result String ()
check state = if state.repost |> MaybeUtils.isEmpty
              then Err "No post has been selected"
              else Ok ()

hasError: Result err ok -> Bool
hasError res = case res of
    Err _ -> True
    Ok _  -> False

wizardDescription =
    """Reposting allow you to share the content created by other users with your followers."""