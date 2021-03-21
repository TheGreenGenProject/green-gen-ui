module View.Chart.ChartUtils exposing (legend)

import Element exposing (Attribute, Color, Element, alignLeft, centerX, centerY, column, el, fill, padding, row, text, width)
import Element.Font as Font
import View.Icons as Icons


legend: List (Attribute msg)  -> List (String, Color) -> Element msg
legend attrs entries = column [alignLeft, width fill] (entries |> List.map (legendEntry attrs))

legendEntry: List (Attribute msg) -> (String, Color) -> Element msg
legendEntry attrs (label, color) = row [alignLeft] [
    el [centerX, centerY, Font.color color, padding 1] (Icons.square Icons.tiny),
    label |> text |> el attrs
 ]