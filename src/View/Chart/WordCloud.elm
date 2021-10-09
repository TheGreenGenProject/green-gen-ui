module View.Chart.WordCloud exposing (wordCloud, hashtagCloud)

import Data.Hashtag exposing (Hashtag(..))
import Element exposing (Element, alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, column, el, fill, height, row, spacing, width)
import Element.Font as Font
import Tuple exposing (first)
import Update.Msg exposing (Msg)
import Utils.ListUtils as ListUtils
import View.Chart.ColorScheme as ColorScheme exposing (ColorScheme)
import View.Style exposing (hashtagCloudStyle)


-- Hashtag cloud, triggering a search on a click on a given Hashtag
hashtagCloud: ColorScheme -> Int -> List (Int, Hashtag) -> Element Msg
hashtagCloud scheme maxSize tags = wordCloud
    (\ht -> hashtagCloudStyle ht)
    scheme
    maxSize
    tags

-- Word cloud
wordCloud: (a -> Element msg) -> ColorScheme -> Int -> List (Int, a) -> Element msg
wordCloud toElement scheme maxSize tags = let desc = tags |> List.sortBy first |> List.reverse in
    recWordCloud toElement scheme maxSize desc
    |> el [width fill, height fill, centerX, centerY]

recWordCloud: (a -> Element msg) -> ColorScheme -> Int ->  List (Int, a) -> Element msg
recWordCloud toElement scheme maxSize tags = let size = maxSize |> max 5 in
    case tags of
        []         -> Element.none
        (_, ht) :: rest -> let {first, second, third, fourth } = split4 rest in
            column [width fill, height fill, centerX, centerY] [
                row [alignBottom, spacing 10] [
                   el [alignLeft, alignBottom] (recWordCloud toElement (cycle 1 scheme) (size // 2 + 1) first)
                   , el [alignRight, alignTop] (recWordCloud toElement (cycle 4 scheme) (size // 2 - 2) fourth)]
                , row [centerX, centerY] [el [Font.size size, Font.color (ColorScheme.colorAt 1 scheme)] (toElement ht)]
                , row [alignBottom, spacing 10] [
                   el [alignLeft, alignTop] (recWordCloud toElement (cycle 3 scheme) (size // 2 - 2) third)
                   , el [alignRight, alignBottom] (recWordCloud toElement (cycle 4 scheme) (size // 2 + 1) second)]
                ]


{-- Helpers --}

split4: List a -> {
    first: List a
    , second: List a
    , third: List a
    , fourth: List a }
split4 xs = let (first, second) = ListUtils.split2 xs
                ((s1, s2), (s3, s4)) = (ListUtils.split2 first, ListUtils.split2 second)
    in { first = s1, second = s2, third = s3, fourth = s4 }

cycle: Int -> ColorScheme -> ColorScheme
cycle n {default, colors } = {
    default = default
    , colors = ListUtils.rotaten n colors
 }
