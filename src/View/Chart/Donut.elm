module View.Chart.Donut exposing (donut)

import Color
import Element exposing (Element)
import Path
import Shape exposing (defaultPieConfig)
import Tuple
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes exposing (fill, stroke, transform)
import TypedSvg.Attributes.InPx exposing (height, width)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..), Transform(..))


type alias ChartConfig =
    { outerRadius : Float
    , innerRadius : Float
    , padAngle : Float
    , cornerRadius : Float
 }

defaultConfig = {
    outerRadius = 48
    , innerRadius = 24
    , padAngle = 0.0
    , cornerRadius = 0.0
 }


donut: Int -> List (Float, Element.Color) -> Element msg
donut radius data = let config = {defaultConfig| outerRadius = radius, innerRadius = 24} in
    Element.html (drawChart defaultConfig data)


drawChart : ChartConfig -> List (Float, Element.Color) -> Svg msg
drawChart config model =
    let
        radius    = config.outerRadius
        colorData = model |> List.map (Tuple.second >> convertColor)
        pieData   =
            model |> List.map Tuple.first
                  |> Shape.pie
                    { defaultPieConfig |
                      innerRadius = config.innerRadius
                      , outerRadius = config.outerRadius
                      , padAngle = config.padAngle
                      , cornerRadius = config.cornerRadius
                      , sortingFn = \_ _ -> EQ
                    }
        zippedData = List.map2 Tuple.pair colorData pieData
        makeSlice (color, datum) =
            Path.element (Shape.arc datum) [ fill <| Paint <| color, stroke <| Paint Color.white ]
    in
    svg [ width (radius * 2), height (radius * 2) ]
        [ g [ transform [ Translate radius radius ] ]
            [ g [] <| List.map makeSlice zippedData  ] ]

convertColor: Element.Color -> Color.Color
convertColor color = let {red, green, blue, alpha} = (Element.toRgb color) in
    Color.fromRgba { red = red, green = green, blue = blue, alpha = alpha }