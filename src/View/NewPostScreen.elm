module View.NewPostScreen exposing (newPostScreen)

import Element exposing (Element, centerX, column, fill, height, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import State.AppState exposing (AppState, Display(..))
import Update.Msg exposing (Msg(..))
import View.Icons as Icons
import View.Style exposing (relFontSize)
import View.Theme
import View.UIStyle exposing (UIStyle)


newPostScreen: AppState -> Element Msg
newPostScreen state = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 20 ]
    [ newTipButton state.uiStyle
     , newChallengeButton state.uiStyle
     , newEventButton state.uiStyle
     , newPollButton state.uiStyle
     , newFreeTextButton state.uiStyle]

newTipButton: UIStyle -> Element Msg
newTipButton ui =
    wizardButton ui "Create a new Tip" (Icons.tip ui.large) (DisplayPage WizardNewTipPage)

newPollButton: UIStyle -> Element Msg
newPollButton ui =
    wizardButton ui "Create a new Poll" (Icons.poll ui.large) (DisplayPage WizardNewPollPage)

newFreeTextButton: UIStyle -> Element Msg
newFreeTextButton ui =
    wizardButton ui "Create a new free text post " (Icons.post ui.large) (DisplayPage WizardNewFreePostPage)

newChallengeButton: UIStyle -> Element Msg
newChallengeButton ui =
    wizardButton ui "Create a new challenge" (Icons.challenge ui.large) (DisplayPage WizardNewChallengePage)

newEventButton: UIStyle -> Element Msg
newEventButton ui =
    wizardButton ui "Create an event" (Icons.event ui.large) (DisplayPage WizardNewEventPage)

wizardButton: UIStyle -> String -> Element Msg -> Msg -> Element Msg
wizardButton ui txt icon onClick =  Input.button
    [width fill
    , padding 15
    , Font.color ui.theme.foreground
    , Background.color ui.theme.background
    , Border.rounded 20]
    { onPress = Just onClick, label = row [padding 5, spacing 20, relFontSize ui 10] [icon, text txt] }
