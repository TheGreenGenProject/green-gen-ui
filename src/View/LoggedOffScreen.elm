module View.LoggedOffScreen exposing (..)

import Element exposing (..)
import Element.Font as Font
import State.AppState exposing (AppState)
import Update.Msg exposing (Msg(..))
import View.Style exposing (relFontSize)

logoffScreen: AppState -> Element Msg
logoffScreen state =  paragraph [ relFontSize state.uiStyle 38, Font.center ]
    [ text "You are ", el [ Font.italic ] <| text "Logged off" ]