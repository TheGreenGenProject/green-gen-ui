module View.Chart.ChartUtils exposing (legend, rightLegendPanel)

import Element exposing (Attribute, Color, Element, alignLeft, centerX, centerY, column, el, fill, padding, row, spacing, text, width)
import Element.Font as Font
import View.Icons as Icons
import View.UIStyle exposing (UIStyle)


legend: UIStyle -> List (Attribute msg)  -> List (String, Color) -> Element msg
legend ui attrs entries = column [alignLeft, width fill] (entries |> List.map (legendEntry ui attrs))

rightLegendPanel: UIStyle -> List (Attribute msg)  -> List (String, Color) -> Element msg -> Element msg
rightLegendPanel ui attrs entries comp = row [spacing 10] [ comp, legend ui attrs entries]

legendEntry: UIStyle -> List (Attribute msg) -> (String, Color) -> Element msg
legendEntry ui attrs (label, color) = row [alignLeft] [
    el [centerX, centerY, Font.color color, padding 1] (Icons.square ui.tiny),
    label |> text |> el attrs
 ]