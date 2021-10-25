module View.ScreenUtils exposing (..)

import Element exposing (Element, centerX, centerY, el, paragraph, text)
import Element.Font as Font
import Update.Msg exposing (Msg)
import View.Style exposing (relFontSize)
import View.Theme as Theme
import View.UIStyle exposing (UIStyle)


emptyScreen: UIStyle -> String -> Element Msg
emptyScreen ui txt = paragraph [
    centerX, centerY,
    relFontSize ui 36, Font.center, Font.color ui.theme.background, Font.italic]
    [text txt]

neverElement: Element Msg
neverElement = el [ Font.italic ] <| text "You should NEVER see this"