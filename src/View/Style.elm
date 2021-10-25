module View.Style exposing (..)

import Data.Challenge exposing (ChallengeId)
import Data.Event exposing (EventId)
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
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import State.AppState exposing (AppState, Display(..))
import Update.Msg exposing (Msg(..))
import Utils.DateUtils exposing (LocalDate, LocalTime, toLocalDate, toLocalTime)
import Utils.TextUtils as TextUtils exposing (QuotedString(..), format2Digits)
import View.Icons as Icons
import View.UIStyle exposing (UIStyle)

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

relFontSize: UIStyle -> Int -> Attribute msg
relFontSize ui sz = Font.size (ui.defaultFontSize + sz)

rightGap: Int -> Element Msg -> Element Msg
rightGap sp x = el [width fill, paddingEach { top = 0, bottom = 0, left = 0, right = sp}] x

leftGap: Int -> Element Msg -> Element Msg
leftGap sp x = el [width fill, paddingEach { top = 0, bottom = 0, left = sp, right = 0}] x

topGap: Int -> Element Msg -> Element Msg
topGap sp x = el [width fill, paddingEach { top = sp, bottom = 0, left = 0, right = 0}] x

bottomGap: Int -> Element Msg -> Element Msg
bottomGap sp x = el [width fill, paddingEach { top = 0, bottom = sp, left = 0, right = 0}] x

relSize: UIStyle -> Int -> Element Msg -> Element Msg
relSize ui sz x = el [relFontSize ui sz] x

noSelectionEffect: Attribute Msg
noSelectionEffect = Element.htmlAttribute <| Html.Attributes.style "box-shadow" "none"

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

loadingFixedTextLine: UIStyle -> Int -> Int -> Element Msg
loadingFixedTextLine ui font w = "" |> text
    |> el [width <| px w, Background.color ui.theme.background, Border.rounded 8, Font.size font]
    |> el [padding 4]

loadingTextLine: UIStyle -> Int -> Element Msg
loadingTextLine ui font = "" |> text
    |> el [width fill, Background.color ui.theme.background, Border.rounded 8, Font.size font]
    |> el [width fill, padding 4]

loadingTextBlock: UIStyle -> Int -> Int -> Element Msg
loadingTextBlock ui font rowCount = column [width fill, height fill, alignTop]
    (loadingFixedTextLine ui font 100 :: (List.range 1 (rowCount - 1) |> List.map (\_ -> loadingTextLine ui font)))


standard: UIStyle -> Element Msg -> Element Msg
standard ui = el [Background.color ui.theme.background, Font.color ui.theme.foreground]

invert: UIStyle -> Element Msg -> Element Msg
invert ui = el [Background.color ui.theme.foreground, Font.color ui.theme.background]

multiLineQuotedText: UIStyle -> String -> Element Msg
multiLineQuotedText ui txt = let lines = String.split ("\n") txt in
    column [spacing 1] (lines |> List.map (quotedText ui))

quotedText: UIStyle -> String -> Element Msg
quotedText ui str =
    let quoted = TextUtils.parseQuotedText str
        quoteToString = \s -> case s of
            Str x -> x |> text
            UserQuote x -> userStyle ui (String.dropLeft 1 x) Nothing
            HashtagQuote x -> x |> String.dropLeft 1 |> Hashtag |> hashtagStyle ui
    in quoted |> List.map (quoteToString) |> List.intersperse (" " |> text) |> paragraph []

followButtonStyle: UIStyle -> UserId -> Element Msg
followButtonStyle ui id =
    Input.button [relFontSize ui 0
                  , paddingXY 2 2
                  , Border.width 1
                  , Border.rounded 4]
        { onPress = Just (FollowUser id), label = Element.text "Follow" }

unfollowButtonStyle: UIStyle -> UserId -> Element Msg
unfollowButtonStyle ui id =
    Input.button [relFontSize ui 0
                  , paddingXY 2 2
                  , Border.width 1
                  , Border.rounded 4]
        { onPress = Just (UnfollowUser id), label = Element.text "Unfollow" }

likeButtonStyle: UIStyle -> PostId -> Element Msg
likeButtonStyle ui id =
    Input.button [relFontSize ui 0
                  , paddingXY 1 1]
        { onPress = Just (LikePost id), label = Icons.unlike <| ui.tiny }

unlikeButtonStyle: UIStyle -> PostId -> Element Msg
unlikeButtonStyle ui id =
    Input.button [relFontSize ui 0
                  , paddingXY 1 1]
        { onPress = Just (UnlikePost id), label = Icons.like <| ui.tiny }

pinButtonStyle: UIStyle -> PostId -> Element Msg
pinButtonStyle ui id =
    Input.button [relFontSize ui 0
                  , paddingXY 1 1]
        { onPress = Just (PinPost id), label = Icons.unpinned <| ui.tiny }

unpinButtonStyle: UIStyle -> PostId -> Element Msg
unpinButtonStyle ui id =
    Input.button [relFontSize ui 0
                  , paddingXY 1 1]
        { onPress = Just (UnpinPost id), label = Icons.pinned <| ui.tiny }

openConversationButtonStyle: UIStyle -> PostId -> Element Msg
openConversationButtonStyle ui id =
    Input.button [relFontSize ui 0
                  , paddingXY 1 1]
            { onPress = Just (OpenPostConversation id), label = Icons.openConversation <| ui.tiny }

closeConversationButtonStyle: UIStyle -> PostId -> Element Msg
closeConversationButtonStyle ui id =
    Input.button [relFontSize ui 0
                  , paddingXY 1 1]
            { onPress = Just (ClosePostConversation id), label = Icons.closeConversation <| ui.tiny }

repostButtonStyle: UIStyle -> PostId -> Element Msg
repostButtonStyle ui id =
    Input.button [relFontSize ui 0
                  , paddingXY 1 1]
            { onPress = Just (Repost id), label = Icons.repost <| ui.tiny }

viewChallengeButtonStyle: UIStyle -> ChallengeId -> Element Msg
viewChallengeButtonStyle ui id =
    Input.button [relFontSize ui 1
                  , paddingXY 2 2
                  , Border.width 1
                  , Border.rounded 5]
        { onPress = Just (DisplayPage (ChallengeDetailsPage id)), label = Element.text "View challenge" }

viewEventButtonStyle: UIStyle -> EventId -> Element Msg
viewEventButtonStyle ui id =
    Input.button [relFontSize ui 1
                  , paddingXY 2 2
                  , Border.width 1
                  , Border.rounded 5]
        { onPress = Just (DisplayPage (EventDetailsPage id)), label = Element.text "View event" }

buttonBarStyle: List (Attribute Msg) -> List (String, Msg) -> Element Msg
buttonBarStyle attrs buttons = buttons
    |> List.map (\(txt, action) -> Input.button attrs { onPress = Just action, label = txt |> text })
    |> List.intersperse (el attrs ("|" |> text))
    |> row attrs

postButtonBarStyle: UIStyle -> (String, Msg) -> Element Msg
postButtonBarStyle ui (txt, action) =
    Input.button [relFontSize ui -1, Font.semiBold] { onPress = Just action, label = txt |> text }


tabButton: UIStyle -> String -> Msg -> Bool -> Element Msg
tabButton ui label msg selected = Input.button [
    relFontSize ui 4
    , Font.color (if selected then ui.theme.tabForeground else ui.theme.disabledTabForeground)
    , if selected then Font.italic else Font.regular
    , Border.color ui.theme.tabForeground
    , Border.widthEach { bottom = (if selected then 3 else 0), top = 0, left = 0, right = 0 }
    , padding 5
 ] { onPress = Just msg, label = label |> text}

tabIconButton: UIStyle -> Element msg -> msg -> Element msg
tabIconButton ui icon msg =
    Input.button [ Background.color ui.theme.background
                  , Font.color ui.theme.foreground
                  , Border.rounded 5
                  , padding 5]
        { onPress = Just msg, label = icon }

screenTabButton: AppState -> Display -> String -> msg -> Element msg
screenTabButton state display txt msg =
    Input.button [ Background.color (if state.display==display then state.uiStyle.theme.foreground else state.uiStyle.theme.background)
                  , Font.color (if state.display==display then state.uiStyle.theme.background else state.uiStyle.theme.foreground)
                  , Border.rounded 5
                  , padding 5]
        { onPress = Just msg, label = Element.text txt }

screenTabIcon: AppState -> Display -> Element msg -> msg -> Element msg
screenTabIcon state display icon msg =
    Input.button [ Background.color (if state.display==display then state.uiStyle.theme.foreground else state.uiStyle.theme.background)
                  , Font.color (if state.display==display then state.uiStyle.theme.background else state.uiStyle.theme.foreground)
                  , Border.rounded 5
                  , padding 5]
        { onPress = Just msg, label = icon }

-- Ads a distinctive sign if there is something new (notifications, new posts, ...)
screenTabIconWithRefresh: AppState -> Display -> Bool -> Element msg -> msg -> Element msg
screenTabIconWithRefresh state display needsRefresh icon msg =
    let refreshColor = if needsRefresh then state.uiStyle.theme.alertColor else state.uiStyle.theme.background
    in Input.button [ Background.color (if state.display==display then state.uiStyle.theme.foreground else refreshColor)
                  , Font.color (if state.display==display then refreshColor else state.uiStyle.theme.foreground)
                  , Border.rounded 5
                  , padding 5]
        { onPress = Just msg, label = icon }

searchBar: AppState -> Element Msg
searchBar state = row [
        width fill
        , Background.color state.uiStyle.theme.foreground
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
    in Input.search [
        relFontSize state.uiStyle 1
        , Border.color state.uiStyle.theme.background
        , Background.color state.uiStyle.theme.textFieldBackground
        , Font.color state.uiStyle.theme.textFieldForeground
        , Border.rounded 5
        , onEnter PerformSearchFromField]
     { onChange = (\txt -> EnteringSearch txt)
       , text = state.search.field
       , placeholder = placeholderStyle state.uiStyle "Search..."
       , label = labelHidden "" }

hashtagStyle: UIStyle -> Hashtag -> Element Msg
hashtagStyle ui (Hashtag tag as ht) =
    Input.button [Font.italic, Font.semiBold, Font.color ui.theme.hashtagForeground]
        { onPress = Just <| PerformSearchFromHashtag ht, label = Element.text ("#" ++ tag) }

hashtagCloudStyle: Hashtag -> Element Msg
hashtagCloudStyle (Hashtag tag as ht) =
    Input.button [Font.italic, Font.semiBold]
        { onPress = Just <| PerformSearchFromHashtag ht, label = Element.text ("#" ++ tag) }

followHashtagStyle: UIStyle -> Hashtag -> Element Msg
followHashtagStyle ui ht = row [spacing 5] [
    hashtagStyle ui ht
    , Input.button [relFontSize ui 1
        , paddingXY 2 2
        , Border.width 1
        , Border.rounded 5]
      { onPress = Just <| FollowHashtag ht, label = "Follow" |> text }
 ]

unfollowHashtagStyle: UIStyle -> Hashtag -> Element Msg
unfollowHashtagStyle ui ht = row [spacing 5] [
    hashtagStyle ui ht
    , Input.button [relFontSize ui 1
        , paddingXY 2 2
        , Border.width 1
        , Border.rounded 5]
      { onPress = Just <| UnfollowHashtag ht, label = "Unfollow" |> text }
 ]

userStyle: UIStyle -> String -> Maybe UserId -> Element Msg
userStyle ui pseudo userId =
    let page = userId |> Maybe.map (UserPage) |> Maybe.withDefault (PseudoPage pseudo) in
    Input.button [Font.italic, Font.semiBold, Font.color ui.theme.userLinkForeground]
        { onPress = DisplayPage page |> Just, label = Element.text ("@" ++ pseudo) }

userPseudoStyle: String -> Maybe UserId -> Element Msg
userPseudoStyle pseudo userId =
    let page = userId |> Maybe.map (UserPage) |> Maybe.withDefault (PseudoPage pseudo) in
    Input.button [Font.italic, Font.semiBold]
        { onPress = DisplayPage page |> Just, label = Element.text ("@" ++ pseudo) }

linkStyle: UIStyle -> Url -> String -> Element msg
linkStyle ui (Url url) txt =
    link [Background.color ui.theme.background
          , Font.color ui.theme.linkForeground
          , Font.italic]
        { url = url, label = Element.text txt }

internalPageLinkStyle: Display -> String -> Element Msg
internalPageLinkStyle page txt =
    Input.button [Font.italic, Font.semiBold]
    { onPress = DisplayPage page |> Just , label = Element.text txt }

italicTextStyle: UIStyle -> String -> Element msg
italicTextStyle ui txt =
    el [Background.color ui.theme.background
        , Font.color ui.theme.foreground
        , Font.italic]
    (text txt)

errorTextStyle: UIStyle -> String -> Element msg
errorTextStyle ui txt =
    el [Font.color ui.theme.errorForeground, Font.italic]
    (text txt)

titledTextStyle: UIStyle -> String -> String -> Element Msg
titledTextStyle ui title content = column [width fill,height fill, spacing 5] [
    el [Font.semiBold, relFontSize ui 2] (quotedText ui title)
    , el [relFontSize ui 0] (multiLineQuotedText ui content)
 ]

titledParagraphStyle: UIStyle -> String -> List String -> Element Msg
titledParagraphStyle ui title content = column [width fill,height fill, spacing 10] [
    el [Font.semiBold, relFontSize ui 2] (quotedText ui title)
    , column [width fill,height fill, spacing 10, relFontSize ui 0] (content |> List.map (multiLineQuotedText ui))
 ]

titledElementStyle: UIStyle -> String -> Element Msg  -> Element Msg
titledElementStyle ui title content = column [width fill, height fill, spacing 5] [
    el [Font.semiBold, relFontSize ui 2] (text title)
    , row [relFontSize ui 0] [content]
 ]

checkListStyle: UIStyle -> List (Attribute Msg) -> List (Bool, String) -> Element Msg
checkListStyle ui attrs items = items
    |> List.map (\(checked, item) -> row [alignLeft] [
        el [centerX, centerY, padding 1, Font.color (if checked then ui.theme.background else ui.theme.disabledButton)] (Icons.square ui.tiny),
        item |> text
      ])
    |> column attrs

placeholderStyle: UIStyle -> String -> Maybe (Placeholder msg)
placeholderStyle ui txt = Just (placeholder [Font.italic, Font.color ui.theme.textFieldPlaceHolder] (text txt))

headerDateStyle: UIStyle -> Element msg -> Element msg
headerDateStyle ui = el [relFontSize ui -2]

postBodyStyle: UIStyle -> Element msg -> Element msg
postBodyStyle ui e = paragraph [width fill, relFontSize ui 0] [e]

postFooterStyle: UIStyle -> Element msg -> Element msg
postFooterStyle ui e = el
    [ width fill
     , relFontSize ui 0
     , Border.rounded 5
     , padding 5
    ] e

notifHeaderStyle: UIStyle -> Element msg -> Element msg
notifHeaderStyle ui e = el
    [Font.color ui.theme.background, Border.rounded 3] e


-- Trying to implement a simplistic and decent date spinner ...
dateSpinner: UIStyle -> UTCTimestamp -> (LocalDate -> Maybe Msg) -> Element Msg
dateSpinner ui timestamp onChange =
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
    in row [Border.width 3, Border.rounded 3, Border.color ui.theme.background, spacing 5] [
        -- Day
        Input.button [Background.color ui.theme.foreground
            , padding 4
            , relFontSize ui 4
            , Font.color ui.theme.foreground
            , Background.color ui.theme.background
            , width <| px 40
            , height fill
            , Border.color ui.theme.background
            , Border.widthEach { right = 1, top = 0, bottom = 0, left = 0 } ]
        { onPress = onChange (spinDay { day = day , month = month, year = year })
          , label = day |> format2Digits |> Element.text |> el [centerX] }
        -- Month
        , Input.button [Background.color ui.theme.foreground
            , padding 4
            , relFontSize ui 4
            , Font.color ui.theme.foreground
            , Background.color ui.theme.background
            , width <| px 40
            , height fill
            , Border.color ui.theme.background
            , Border.widthEach { right = 1, top = 0, bottom = 0, left = 0 }]
         { onPress = onChange (spinMonth { day = day, month = month, year = year })
           , label = month |> format2Digits |> Element.text |> el [centerX] }
        -- Year
        , Input.button [Background.color ui.theme.foreground
            , Font.color ui.theme.foreground
            , Background.color ui.theme.background
            , padding 4
            , relFontSize ui 4
            , height fill
            , width <| px 60]
         { onPress = onChange (spinYear { day = day, month = month, year = year })
          , label = year |> String.fromInt |> Element.text |> el [centerX] }
    ]

timeSpinner: UIStyle -> UTCTimestamp -> (LocalTime -> Maybe Msg) -> Element Msg
timeSpinner ui timestamp onChange =
    let {hour, minute} = toLocalTime timestamp
        spinHour time   = { hour = (modBy 24 time.hour), minute = time.minute }
        spinMinute time = { hour = time.hour, minute = (modBy 60 time.minute) }
    in row [Border.width 3, Border.rounded 3, Border.color ui.theme.background, spacing 5] [
        Input.button [Background.color ui.theme.foreground
            , padding 4
            , relFontSize ui 4
            , Font.color ui.theme.foreground
            , Background.color ui.theme.background
            , width <| px 40
            , height fill
            , Border.color ui.theme.background
            , Border.widthEach { right = 1, top = 0, bottom = 0, left = 0 } ]
        { onPress = onChange (spinHour { hour = hour + 1 , minute = minute })
          , label = hour |> format2Digits |> Element.text |> el [centerX] }
       , Input.button [Background.color ui.theme.foreground
                     , padding 4
                     , relFontSize ui 4
                     , Font.color ui.theme.foreground
                     , Background.color ui.theme.background
                     , width <| px 40
                     , height fill
                     , Border.color ui.theme.background
                     , Border.widthEach { right = 1, top = 0, bottom = 0, left = 0 } ]
        { onPress = onChange (spinMinute { hour = hour , minute = minute + 1 })
          , label = minute |> format2Digits |> Element.text |> el [centerX] }
    ]

intSpinner: UIStyle -> Int -> Int -> Int -> Int -> (Int -> Maybe Msg) -> Element Msg
intSpinner ui mn mx step value onChange =
    let checked v = min mx v |> max mn
        checkedValue = checked value
    in
    row [spacing 5] [
        String.fromInt checkedValue |> text
        , column [centerX, centerY] [
            Input.button [centerX, centerY, relFontSize ui 0]
                     { onPress = onChange ((checkedValue + step) |> checked), label = "▲" |> text}
            , Input.button [centerX, centerY, relFontSize ui 0]
                     { onPress = onChange ((checkedValue - step) |> checked), label = "▼" |> text}
        ]
      ]


-- Option selection

type ButtonPosition
    = First
    | Mid
    | Last

options: UIStyle -> List (String, a) -> a -> (a -> Msg) -> Element Msg
options ui opts selected onChange =
    Input.radioRow
        [ Border.rounded 6
          --, Border.shadow { offset = ( 0, 0 ), size = 3, blur = 10, color = Theme.lightGrey }
        ]
        { onChange = onChange
          , selected = Just selected
          , label = Input.labelHidden ""
          , options = opts
            |> List.map (\(label, opt) -> Input.optionWith opt <| button ui Mid label )
        }

button : UIStyle -> ButtonPosition -> String -> Input.OptionState -> Element msg
button ui position label state =
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
            , Border.color ui.theme.background
            , Font.color <|
                if state == Input.Selected then  ui.theme.foreground
                else ui.theme.background
            , Background.color <|
                if state == Input.Selected then ui.theme.background
                else ui.theme.foreground
            ] <| el [ centerX, centerY, relFontSize ui 4 ] <| text label
