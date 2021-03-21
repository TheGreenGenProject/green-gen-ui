module View.Chart.Heatmap exposing (..)

import Element exposing (Element, column, el, row, text)
import Element.Background as Background
import View.Chart.ColorScheme as ColorScheme exposing (ColorScheme)



heatmap: ColorScheme -> Int -> Int -> List String -> List String -> List (List Int) -> Element msg
heatmap scheme min max xLabels yLabels rows =
    let makeCell = cell scheme min max in
    column [] (List.map2 (\label values -> row [] ([label |> text] ++ (values |> List.map makeCell))) xLabels rows)

cell: ColorScheme -> Int -> Int -> Int -> Element msg
cell scheme min max value = let color = ColorScheme.colorFor min max value scheme in
    el [Background.color color] (text " ")
