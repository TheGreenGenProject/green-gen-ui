module View.Style exposing (..)

import Data.Challenge exposing (ChallengeId)
import Data.Hashtag exposing (Hashtag(..))
import Data.Page as Page exposing (Page)
import Data.Post exposing (PostId)
import Data.Schedule exposing (UTCTimestamp(..))
import Data.Url exposing (Url(..))
import Data.User exposing (UserId)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (Placeholder, labelHidden, placeholder)
import Html.Events
import Json.Decode as Decode
import State.AppState exposing (AppState, Display(..))
import Update.Msg exposing (Msg(..))
import Utils.DateUtils exposing (LocalDate, toLocalDate)
import Utils.TextUtils as TextUtils exposing (QuotedString(..), format2Digits)
import View.Icons as Icons
import View.Theme as Theme exposing (background, blue, darkRed, errorForeground, foreground, grey)

empty: Element Msg
empty = Element.none

center: Element Msg -> Element Msg
center = el [centerY, centerY]

normal: String -> Element Msg
normal txt = txt |> text

italic: String -> Element Msg
italic txt = txt |> text |> el [Font.italic]

bold: String -> Element Msg
bold txt = txt |> text |> el [Font.bold]

semiBold: String -> Element Msg
semiBold txt = txt |> text |> el [Font.semiBold]

space: Int -> Element Msg -> Element Msg
space sp x = el [paddingEach { top = 0, bottom = 0, left = 0, right = sp}] x

size: Int -> Element Msg -> Element Msg
size sz x = el [Font.size sz] x

horizontalSeparator: Int -> Color -> Element Msg
horizontalSeparator thickness color = column [width fill] [
    el [width fill, Font.color color, Border.widthEach { bottom = thickness, top = 0, left = 0, right = 0}] empty
    , el [width fill] (empty)
 ]

verticalSeparator: Int -> Color -> Element Msg
verticalSeparator thickness color = row [height fill] [
    el [height fill, Font.color color, Border.widthEach {left = 0, top = 0, bottom = 0, right = thickness} ] empty
    , el [ ] (empty)
 ]

standard: Element Msg -> Element Msg
standard = el [Background.color background, Font.color foreground]

invert: Element Msg -> Element Msg
invert = el [Background.color foreground, Font.color background]

multiLineQuotedText: String -> Element Msg
multiLineQuotedText txt = let lines = String.split ("\n") txt in
    column [spacing 2] (lines |> List.map (quotedText))

quotedText: String -> Element Msg
quotedText str =
    let quoted = TextUtils.parseQuotedText str
        quoteToString = \s -> case s of
            Str x -> x |> text
            UserQuote x -> userStyle (String.dropLeft 1 x) Nothing
            HashtagQuote x -> x |> String.dropLeft 1 |> Hashtag |> hashtagStyle
    in quoted |> List.map (quoteToString) |> List.intersperse (" " |> text) |> paragraph []

tabStyle: Element msg -> Element msg
tabStyle e = el
    [ Background.color background
      , Font.color foreground
      , Border.rounded 5
      , padding 5
    ] e

panelStyle: Element msg -> Element msg
panelStyle e = el
    [ Background.color background
        , Font.color foreground
        , Border.rounded 5
        , padding 5
    ] e

buttonStyle: String -> msg -> Element msg
buttonStyle txt msg =
    Input.button [Background.color background
                  , Font.color foreground
                  , paddingXY 20 20
                  , Border.rounded 3]
        { onPress = Just msg, label = Element.text txt }

followButtonStyle: UserId -> Element Msg
followButtonStyle id =
    Input.button [Font.size 10
                  , paddingXY 2 2
                  , Border.width 1
                  , Border.rounded 4]
        { onPress = Just (FollowUser id), label = Element.text "Follow" }

unfollowButtonStyle: UserId -> Element Msg
unfollowButtonStyle id =
    Input.button [Font.size 10
                  , paddingXY 2 2
                  , Border.width 1
                  , Border.rounded 4]
        { onPress = Just (UnfollowUser id), label = Element.text "Unfollow" }

likeButtonStyle: PostId -> Element Msg
likeButtonStyle id =
    Input.button [Font.size 10
                  , paddingXY 1 1]
        { onPress = Just (LikePost id), label = Icons.unlike <| Icons.tiny }

unlikeButtonStyle: PostId -> Element Msg
unlikeButtonStyle id =
    Input.button [Font.size 10
                  , paddingXY 1 1]
        { onPress = Just (UnlikePost id), label = Icons.like <| Icons.tiny }

pinButtonStyle: PostId -> Element Msg
pinButtonStyle id =
    Input.button [Font.size 10
                  , paddingXY 1 1]
        { onPress = Just (PinPost id), label = Icons.unpinned <| Icons.tiny }

unpinButtonStyle: PostId -> Element Msg
unpinButtonStyle id =
    Input.button [Font.size 10
                  , paddingXY 1 1]
        { onPress = Just (UnpinPost id), label = Icons.pinned <| Icons.tiny }

openConversationButtonStyle: PostId -> Element Msg
openConversationButtonStyle id =
    Input.button [Font.size 10
                  , paddingXY 1 1]
            { onPress = Just (OpenPostConversation id), label = Icons.openConversation <| Icons.tiny }

closeConversationButtonStyle: PostId -> Element Msg
closeConversationButtonStyle id =
    Input.button [Font.size 10
                  , paddingXY 1 1]
            { onPress = Just (ClosePostConversation id), label = Icons.closeConversation <| Icons.tiny }

viewChallengeButtonStyle: ChallengeId -> Element Msg
viewChallengeButtonStyle id =
    Input.button [Font.size 11
                  , paddingXY 2 2
                  , Border.width 1
                  , Border.rounded 5]
        { onPress = Just (DisplayPage (ChallengeDetailsPage id)), label = Element.text "View challenge" }

buttonBarStyle: List (Attribute Msg) -> List (String, Msg) -> Element Msg
buttonBarStyle attrs buttons = buttons
    |> List.map (\(txt, action) -> Input.button attrs { onPress = Just action, label = txt |> text })
    |> List.intersperse (el attrs ("|" |> text))
    |> row attrs

tabButton: String -> msg -> Element msg
tabButton txt msg =
    Input.button [ Background.color background
                  , Font.color foreground
                  , Border.rounded 5
                  , padding 5]
        { onPress = Just msg, label = Element.text txt }

tabIconButton: Element msg -> msg -> Element msg
tabIconButton icon msg =
    Input.button [ Background.color background
                  , Font.color foreground
                  , Border.rounded 5
                  , padding 5]
        { onPress = Just msg, label = icon }

screenTabButton: AppState -> Display -> String -> msg -> Element msg
screenTabButton state display txt msg =
    Input.button [ Background.color (if state.display==display then foreground else background)
                  , Font.color (if state.display==display then background else foreground)
                  , Border.rounded 5
                  , padding 5]
        { onPress = Just msg, label = Element.text txt }

screenTabIcon: AppState -> Display -> Element msg -> msg -> Element msg
screenTabIcon state display icon msg =
    Input.button [ Background.color (if state.display==display then foreground else background)
                  , Font.color (if state.display==display then background else foreground)
                  , Border.rounded 5
                  , padding 5]
        { onPress = Just msg, label = icon }

-- Ads a distinctive sign if there is something new (notifications, new posts, ...)
screenTabIconWithRefresh: AppState -> Display -> Bool -> Element msg -> msg -> Element msg
screenTabIconWithRefresh state display needsRefresh icon msg =
    let refreshColor = if needsRefresh then darkRed else background
    in Input.button [ Background.color (if state.display==display then foreground else refreshColor)
                  , Font.color (if state.display==display then refreshColor else foreground)
                  , Border.rounded 5
                  , padding 5]
        { onPress = Just msg, label = icon }

searchBar: AppState -> Element Msg
searchBar state = row [
        width fill
        , Background.color foreground
        , Border.rounded 5
        , Border.width 0
        , spacing 10]
        [searchField state]

searchField: AppState -> Element Msg
searchField state =
    let onEnter : msg -> Element.Attribute msg
        onEnter msg = Element.htmlAttribute
            (Html.Events.on "keyup"
                (Decode.field "key" Decode.string
                    |> Decode.andThen (\key -> if key == "Enter" then Decode.succeed msg else Decode.fail "Not the enter key")))
    in Input.search [ Font.size 11, Border.color background, Border.rounded 5, onEnter PerformSearchFromField]
     { onChange = (\txt -> EnteringSearch txt)
       , text = state.search.field
       , placeholder = placeholderStyle "Search..."
       , label = labelHidden "" }

hashtagStyle: Hashtag -> Element Msg
hashtagStyle (Hashtag tag as ht) =
    Input.button [Font.italic, Font.semiBold]
        { onPress = Just <| PerformSearchFromHashtag ht, label = Element.text ("#" ++ tag) }

followHashtagStyle: Hashtag -> Element Msg
followHashtagStyle ht = row [spacing 5] [
    hashtagStyle ht
    , Input.button [Font.size 11
        , paddingXY 2 2
        , Border.width 1
        , Border.rounded 5]
      { onPress = Just <| FollowHashtag ht, label = "Follow" |> text }
 ]

unfollowHashtagStyle: Hashtag -> Element Msg
unfollowHashtagStyle ht = row [spacing 5] [
    hashtagStyle ht
    , Input.button [Font.size 11
        , paddingXY 2 2
        , Border.width 1
        , Border.rounded 5]
      { onPress = Just <| UnfollowHashtag ht, label = "Unfollow" |> text }
 ]

userStyle: String -> Maybe UserId -> Element Msg
userStyle pseudo userId =
    let page = userId |> Maybe.map (UserPage) |> Maybe.withDefault (PseudoPage pseudo) in
    Input.button [Font.italic, Font.semiBold, Font.color blue]
        { onPress = DisplayPage page |> Just, label = Element.text ("@" ++ pseudo) }

userPseudoStyle: String -> Maybe UserId -> Element Msg
userPseudoStyle pseudo userId =
    let page = userId |> Maybe.map (UserPage) |> Maybe.withDefault (PseudoPage pseudo) in
    Input.button [Font.italic, Font.semiBold]
        { onPress = DisplayPage page |> Just, label = Element.text ("@" ++ pseudo) }

linkStyle: Url -> String -> Element msg
linkStyle (Url url) txt =
    link [Background.color background
          , Font.color foreground
          , Font.italic]
        { url = url, label = Element.text txt }

internalPageLinkStyle: Display -> String -> Element Msg
internalPageLinkStyle page txt =
    Input.button [Font.italic, Font.semiBold]
    { onPress = DisplayPage page |> Just , label = Element.text txt }

italicTextStyle: String -> Element msg
italicTextStyle txt =
    el [Background.color background
        , Font.color foreground
        , Font.italic]
    (text txt)

normalTextStyle: String -> Element msg
normalTextStyle txt =
    el [Background.color background
        , Font.color foreground]
    (text txt)

errorTextStyle: String -> Element msg
errorTextStyle txt =
    el [Font.color errorForeground, Font.italic]
    (text txt)

titledTextStyle: String -> String -> Int -> Element Msg
titledTextStyle title content fontSize = column [width fill,height fill, spacing 5] [
    el [Font.semiBold, Font.size (fontSize + 2)] (quotedText title)
    , el [Font.size fontSize] (multiLineQuotedText content)
 ]

titledElementStyle: String -> Element Msg -> Int -> Element Msg
titledElementStyle title content fontSize = column [width fill, height fill, spacing 5] [
    el [Font.semiBold, Font.size (fontSize + 2)] (text title)
    , row [Font.size fontSize] [content]
 ]

checkListStyle: List (Attribute Msg) -> List (Bool, String) -> Element Msg
checkListStyle attrs items = items
    |> List.map (\(checked, item) -> row [alignLeft] [
        el [centerX, centerY, padding 1, Font.color (if checked then background else grey)] (Icons.square Icons.tiny),
        item |> text
      ])
    |> column attrs

placeholderStyle: String -> Maybe (Placeholder msg)
placeholderStyle txt = Just (placeholder [Font.italic, Font.color Theme.lightBlue] (text txt))

postHeaderStyle: Element msg -> Element msg
postHeaderStyle e = el
    [ width fill
     , Background.color background
     , Font.color foreground
     , Font.size 12
     , Border.rounded 5
     , padding 5
    ] e

headerDateStyle: Element msg -> Element msg
headerDateStyle = el [Font.size 8]

postBodyStyle: Element msg -> Element msg
postBodyStyle e = paragraph [width fill, Font.size 10] [e]

postFooterStyle: Element msg -> Element msg
postFooterStyle e = el
    [ width fill
     , Font.size 10
     , Border.rounded 5
     , padding 5
    ] e

notifHeaderStyle: Element msg -> Element msg
notifHeaderStyle e = el
    [Font.color background, Border.rounded 3] e

paged: Page -> (Page -> Msg) -> Bool -> Element Msg -> Element Msg
paged page f showNext comp = column [width fill, height fill, centerX] [comp, pageBar page f showNext]

pageBar: Page -> (Page -> Msg) -> Bool -> Element Msg
pageBar page f showNext = row [
        width fill
        , centerX
        , padding 5
        , spacing 5
        , Border.widthEach { top=1, bottom=0, left=0, right=0 }
        , Border.color background]
        (( if Page.isFirst page then []
           else [Input.button [Background.color foreground
                              , Font.color background
                              , Font.size 14
                              , Font.semiBold
                              , Border.rounded 5
                              , height fill]
                              { onPress = page |> Page.previous |> Maybe.map f, label = Element.text "<<" }]) ++
        [el [centerX, Font.color background, Font.size 14] (page |> Page.number |> String.fromInt |> (++) "Page " |> text)] ++
        ( if not showNext then []
           else [Input.button
                [Background.color foreground
                , Font.color background
                , Font.size 14
                , Font.semiBold
                , Border.rounded 5
                , height fill]
                { onPress = if showNext then page |> Page.next |> f |> Just else Nothing, label = Element.text ">>" }
        ]))


-- Trying to implement a simplistic and decent date spinner ...

dateSpinner: UTCTimestamp ->
             (LocalDate -> Maybe Msg) ->
             Element Msg
dateSpinner timestamp onChange =
    let leapYear yyyy = ((modBy 4 yyyy == 0) || (modBy 100 yyyy == 0)) &&  not (modBy 400 yyyy == 0)
        maxDayOfMonth date = if date.month == 2 && leapYear date.year then 29
            else if date.month == 2 then 28
            else if [1 , 3, 5 , 7, 8, 10, 12] |> List.member date.month then 31
            else 30
        ensureMaxDay date = {day = min date.day (maxDayOfMonth date), month = date.month, year = date.year }
        spinYear date = {day = date.day, month = date.month, year = date.year + 1 }
        spinMonth date = {day = date.day, month = (modBy 12 date.month) + 1, year = date.year }
            |> ensureMaxDay
        spinDay date = let maxDay = maxDayOfMonth date in {day = (modBy maxDay date.day) + 1, month = date.month, year = date.year }
        {day, month, year} = toLocalDate timestamp
    in row [Border.width 3, Border.rounded 3, Border.color Theme.background, spacing 5] [
        -- Day
        Input.button [Background.color foreground
            , padding 4
            , Font.size 14
            , Font.color foreground
            , Background.color background
            , width <| px 30
            , height fill
            , Border.color background
            , Border.widthEach { right = 1, top = 0, bottom = 0, left = 0 } ]
        { onPress = onChange (spinDay { day = day , month = month, year = year })
          , label = day |> format2Digits |> Element.text |> el [centerX] }
        -- Month
        , Input.button [Background.color foreground
            , padding 4
            , Font.size 14
            , Font.color foreground
            , Background.color background
            , width <| px 30
            , height fill
            , Border.color background
            , Border.widthEach { right = 1, top = 0, bottom = 0, left = 0 }]
         { onPress = onChange (spinMonth { day = day, month = month, year = year })
           , label = month |> format2Digits |> Element.text |> el [centerX] }
        -- Year
        , Input.button [Background.color foreground
            , Font.color foreground
            , Background.color background
            , padding 4
            , Font.size 14
            , height fill
            , width <| px 50]
         { onPress = onChange (spinYear { day = day, month = month, year = year })
          , label = year |> String.fromInt |> Element.text |> el [centerX] }
    ]

intSpinner: Int -> Int -> Int -> Int -> (Int -> Maybe Msg) -> Element Msg
intSpinner mn mx step value onChange =
    let checked v = min mx v |> max mn
        checkedValue = checked value
    in
    row [spacing 5] [
        String.fromInt checkedValue |> text
        , column [centerX, centerY] [
            Input.button [centerX, centerY, Font.size 10]
                     { onPress = onChange ((checkedValue + step) |> checked), label = "▲" |> text}
            , Input.button [centerX, centerY, Font.size 10]
                     { onPress = onChange ((checkedValue - step) |> checked), label = "▼" |> text}
        ]
      ]


-- Option selection

type ButtonPosition
    = First
    | Mid
    | Last

options: List (String, a) -> a -> (a -> Msg) -> Element Msg
options opts selected onChange =
    Input.radioRow
        [ Border.rounded 6
          , Border.shadow { offset = ( 0, 0 ), size = 3, blur = 10, color = Theme.lightGrey }
        ]
        { onChange = onChange
          , selected = Just selected
          , label = Input.labelHidden ""
          , options = opts
            |> List.map (\(label, opt) -> Input.optionWith opt <| button Mid label )
        }

button : ButtonPosition -> String -> Input.OptionState -> Element msg
button position label state =
    let borders =
            case position of
                First -> { left = 2, right = 2, top = 2, bottom = 2 }
                Mid -> { left = 0, right = 2, top = 2, bottom = 2 }
                Last -> { left = 0, right = 2, top = 2, bottom = 2 }
        corners =
            case position of
                First -> { topLeft = 6, bottomLeft = 6, topRight = 0, bottomRight = 0 }
                Mid -> { topLeft = 0, bottomLeft = 0, topRight = 0, bottomRight = 0 }
                Last -> { topLeft = 0, bottomLeft = 0, topRight = 6, bottomRight = 6 }
    in el [ paddingEach { left = 3, right = 3, top = 3, bottom = 3 }
            , Border.roundEach corners
            , Border.widthEach borders
            , Border.color Theme.background
            , Font.color <|
                if state == Input.Selected then  Theme.white
                else Theme.background
            , Background.color <|
                if state == Input.Selected then  Theme.background
                else Theme.white
            ] <| el [ centerX, centerY, Font.size 14 ] <| text label
