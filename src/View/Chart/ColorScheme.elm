module View.Chart.ColorScheme exposing (..)

import Basics as Int
import Element exposing (Color)
import Tuple exposing (second)
import Utils.ListUtils as ListUtils
import View.Theme exposing (..)


type alias ColorScheme = {
    default: Color,
    colors: List Color
 }


{-- Standard ColorScheme --}
shadesOfGreen = deriveFrom darkGreen background 7

wordCloudScheme = {
    default = darkGreen,
    colors = [
        lightBrown
        , darkBrown
        , darkBlue
        , lightBlue
        , lightOrange
        , green
        , orange
        , darkGreen
        , darkOrange
    ]
 }

pollPieChartScheme = {
    default = purple,
    colors = [
        darkOrange
        , lightGreen
        , charcoal
        , darkRed
        , lightBlue
        , yellow
        , lightPurple
        , lightBrown
    ]
 }

orangeToGreen = {
    default = lightOrange,
    colors = [
        lightOrange
        , orange
        , darkOrange
        , lightYellow
        , yellow
        , darkYellow
        , lightGreen
        , green
        , darkGreen
    ]
 }
greenToOrange = orangeToGreen |> reverse

redToGreen = {
    default = darkRed,
    colors = [
        darkRed
        , red
        , lightRed
        , darkOrange
        , orange
        , lightOrange
        , lightYellow
        , yellow
        , darkYellow
        , lightGreen
        , green
        , darkGreen
    ]
 }
greenToRed = redToGreen |> reverse


-- Derives a n color scheme for colors between start and end
deriveFrom: Color -> Color -> Int -> ColorScheme
deriveFrom start end n =
    let sr = start |> Element.toRgb |> .red
        sg = start |> Element.toRgb |> .green
        sb = start |> Element.toRgb |> .blue
        sa = start |> Element.toRgb |> .alpha
        er = end   |> Element.toRgb |> .red
        eg = end   |> Element.toRgb |> .green
        eb = end   |> Element.toRgb |> .blue
        ea = end   |> Element.toRgb |> .alpha
        nf = n |> Int.toFloat
        redStep   = (er - sr) / nf
        greenStep = (eg - sg) / nf
        blueStep  = (eb - sb) / nf
        alphaStep = (ea - sa) / nf
        bounded = min 1.0 << max 0.0
        increment col = Element.fromRgb {
            red   = (col |> Element.toRgb |> .red)   + redStep   |> bounded,
            green = (col |> Element.toRgb |> .green) + greenStep |> bounded,
            blue  = (col |> Element.toRgb |> .blue)  + blueStep  |> bounded,
            alpha = (col |> Element.toRgb |> .alpha) + alphaStep |> bounded }
    in {
    default = start
    , colors = (ListUtils.fix (1,start) (\(i,col) -> (i+1, increment col)) (\(i,_) -> i==n))
        |> List.map second
 }

reverse: ColorScheme -> ColorScheme
reverse { default , colors } = let reversed = colors |> List.reverse in {
    default = reversed |> List.head |> Maybe.withDefault default,
    colors = reversed
 }

size: ColorScheme -> Int
size scheme = scheme.colors
    |> List.length

colorAt: Int -> ColorScheme -> Color
colorAt n scheme = scheme.colors
    |> ListUtils.nth n
    |> Maybe.withDefault scheme.default

cycledColorAt: Int -> ColorScheme -> Color
cycledColorAt n scheme = colorAt ((modBy (size scheme) n)) scheme

colorFor: Int -> Int -> Int -> ColorScheme -> Color
colorFor minValue maxValue n scheme =
    let safeN = max n minValue      |> min maxValue
        width = maxValue - minValue |> abs
        ratio = (safeN + minValue) // width
    in colorAt ratio scheme
