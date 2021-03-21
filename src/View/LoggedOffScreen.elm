module View.LoggedOffScreen exposing (..)

import Element exposing (..)
import Element.Font as Font
import State.AppState exposing (AppState)
import Update.Msg exposing (Msg(..))

logoffScreen: AppState -> Element Msg
logoffScreen _ =  paragraph [ Font.size 48, Font.center ]
    [ text "You are ", el [ Font.italic ] <| text "Logged off" ]