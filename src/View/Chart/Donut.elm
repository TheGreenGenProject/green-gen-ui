module View.Chart.Donut exposing (
    donut
    , doubleDonut
    , smallDonut
    , doubleSmallDonut
    , tinyDonut
    , doubleTinyDonut
    , ChartConfig
    , donutWithConfig
    , doubleDonutWithConfig)

import Basics
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


donut: List (Float, Element.Color) -> Element msg
donut  data = let config = {defaultConfig| outerRadius = 48.0, innerRadius = 32.0} in
    donutWithConfig config data

smallDonut: List (Float, Element.Color) -> Element msg
smallDonut data = let config = {defaultConfig| outerRadius = 32.0, innerRadius = 24.0} in
    donutWithConfig config data

tinyDonut: List (Float, Element.Color) -> Element msg
tinyDonut data = let config = {defaultConfig| outerRadius = 24.0, innerRadius = 16.0} in
    donutWithConfig config data

donutWithConfig: ChartConfig -> List (Float, Element.Color) -> Element msg
donutWithConfig config data = Element.html (drawChart config data)


doubleDonut: List (Float, Element.Color) -> List (Float, Element.Color) -> Element msg
doubleDonut outerData innerData = let outerConfig = {defaultConfig| outerRadius = 48.0, innerRadius = 32.0 }
                                      innerConfig = {defaultConfig| outerRadius = 32.0, innerRadius = 16.0 }
    in doubleDonutWithConfig outerConfig outerData innerConfig innerData

doubleSmallDonut: List (Float, Element.Color) -> List (Float, Element.Color) -> Element msg
doubleSmallDonut outerData innerData = let outerConfig = {defaultConfig| outerRadius = 32.0, innerRadius = 24.0 }
                                           innerConfig = {defaultConfig| outerRadius = 24.0, innerRadius = 16.0 }
    in doubleDonutWithConfig outerConfig outerData innerConfig innerData

doubleTinyDonut: List (Float, Element.Color) -> List (Float, Element.Color) -> Element msg
doubleTinyDonut outerData innerData = let outerConfig = {defaultConfig| outerRadius = 24.0, innerRadius = 16.0 }
                                          innerConfig = {defaultConfig| outerRadius = 16.0, innerRadius = 8.0 }
    in doubleDonutWithConfig outerConfig outerData innerConfig innerData

doubleDonutWithConfig: ChartConfig -> List (Float, Element.Color) -> ChartConfig -> List (Float, Element.Color) -> Element msg
doubleDonutWithConfig outerconfig outerData innerconfig innerData =
    Element.html (drawDoubleChart outerconfig outerData innerconfig innerData)



-- Draw a simple donut
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

-- Draw a simple donut
drawDoubleChart : ChartConfig ->
                  List (Float, Element.Color) ->
                  ChartConfig ->
                  List (Float, Element.Color) ->
                  Svg msg
drawDoubleChart outerConfig outerModel innerConfig innerModel =
    let
        outerRadius    = outerConfig.outerRadius
        outerColorData = outerModel |> List.map (Tuple.second >> convertColor)
        outerPieData   =
            outerModel |> List.map Tuple.first
                       |> Shape.pie
                            { defaultPieConfig |
                              innerRadius = outerConfig.innerRadius
                              , outerRadius = outerConfig.outerRadius
                              , padAngle = outerConfig.padAngle
                              , cornerRadius = outerConfig.cornerRadius
                              , sortingFn = \_ _ -> EQ
                            }
        outerZippedData = List.map2 Tuple.pair outerColorData outerPieData
        innerRadius    = innerConfig.outerRadius
        innerColorData = innerModel |> List.map (Tuple.second >> convertColor)
        innerPieData   =
            innerModel |> List.map Tuple.first
                       |> Shape.pie
                            { defaultPieConfig |
                              innerRadius = innerConfig.innerRadius
                              , outerRadius = innerConfig.outerRadius
                              , padAngle = innerConfig.padAngle
                              , cornerRadius = innerConfig.cornerRadius
                              , sortingFn = \_ _ -> EQ
                            }
        innerZippedData = List.map2 Tuple.pair innerColorData innerPieData
        makeSlice (color, datum) =
            Path.element (Shape.arc datum) [ fill <| Paint <| color, stroke <| Paint Color.white ]
    in
    svg [ width (outerRadius * 2), height (outerRadius * 2) ]
        [ g [ transform [ Translate outerRadius outerRadius ] ]
        [ g [] <| (List.map makeSlice outerZippedData) ++ (List.map makeSlice innerZippedData) ] ]

convertColor: Element.Color -> Color.Color
convertColor color = let {red, green, blue, alpha} = (Element.toRgb color) in
    Color.fromRgba { red = red, green = green, blue = blue, alpha = alpha }