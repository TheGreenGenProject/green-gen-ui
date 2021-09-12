module View.NewPostScreen exposing (newPostScreen)

import Element exposing (Element, centerX, column, fill, height, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import State.AppState exposing (AppState, Display(..))
import Update.Msg exposing (Msg(..))
import View.Icons as Icons
import View.Theme exposing (background, foreground)


newPostScreen: AppState -> Element Msg
newPostScreen _ = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 20 ]
    [ newTipButton
     , newChallengeButton
     , newEventButton
     , newPollButton
     , newFreeTextButton]

newTipButton: Element Msg
newTipButton =
    wizardButton "Create a new Tip" (Icons.tip Icons.large) (DisplayPage WizardNewTipPage)

newPollButton: Element Msg
newPollButton =
    wizardButton "Create a new Poll" (Icons.poll Icons.large) (DisplayPage WizardNewPollPage)

newFreeTextButton: Element Msg
newFreeTextButton =
    wizardButton "Create a new free text post " (Icons.post Icons.large) (DisplayPage WizardNewFreePostPage)

newChallengeButton: Element Msg
newChallengeButton =
    wizardButton "Create a new challenge" (Icons.challenge Icons.large) (DisplayPage WizardNewChallengePage)

newEventButton: Element Msg
newEventButton =
    wizardButton "Create an event" (Icons.event Icons.large) (DisplayPage WizardNewEventPage)

wizardButton: String -> Element Msg -> Msg -> Element Msg
wizardButton txt icon onClick =  Input.button
    [width fill
    , padding 15
    , Font.color foreground
    , Background.color background
    , Border.rounded 20]
    { onPress = Just onClick, label = row [padding 5, spacing 20, Font.size 20] [icon, text txt] }
