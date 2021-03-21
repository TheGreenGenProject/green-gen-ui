module View.ScreenUtils exposing (..)

import Element exposing (Element, centerX, centerY, el, text)
import Element.Font as Font
import Update.Msg exposing (Msg)
import View.Theme as Theme


emptyScreen: String -> Element Msg
emptyScreen txt = el [
    centerX, centerY,
    Font.size 48, Font.center, Font.color Theme.background, Font.italic]
    <| text txt

neverElement: Element Msg
neverElement = el [ Font.italic ] <| text "You should NEVER see this"