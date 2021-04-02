module View.Style exposing (..)

import Data.Challenge exposing (ChallengeId)
import Data.Hashtag exposing (Hashtag(..))
import Data.Page as Page exposing (Page)
import Data.Post exposing (PostId)
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
import View.Icons as Icons
import View.Theme as Theme exposing (background, black, blue, darkRed, errorForeground, foreground, grey)

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
    Input.button [Background.color background
                  , Font.color foreground
                  , Font.size 10
                  , paddingXY 2 2
                  , Border.width 1
                  , Border.rounded 4]
        { onPress = Just (FollowUser id), label = Element.text "Follow" }

unfollowButtonStyle: UserId -> Element Msg
unfollowButtonStyle id =
    Input.button [Background.color background
                  , Font.color foreground
                  , Font.size 10
                  , paddingXY 2 2
                  , Border.width 1
                  , Border.rounded 4]
        { onPress = Just (UnfollowUser id), label = Element.text "Unfollow" }

likeButtonStyle: PostId -> Element Msg
likeButtonStyle id =
    Input.button [Background.color background
                  , Font.color foreground
                  , Font.size 10
                  , paddingXY 1 1]
        { onPress = Just (LikePost id), label = Icons.unlike <| Icons.tiny }

unlikeButtonStyle: PostId -> Element Msg
unlikeButtonStyle id =
    Input.button [Background.color background
                  , Font.color foreground
                  , Font.size 10
                  , paddingXY 1 1]
        { onPress = Just (UnlikePost id), label = Icons.like <| Icons.tiny }

pinButtonStyle: PostId -> Element Msg
pinButtonStyle id =
    Input.button [Background.color background
                  , Font.color foreground
                  , Font.size 10
                  , paddingXY 1 1]
        { onPress = Just (PinPost id), label = Icons.unpinned <| Icons.tiny }

unpinButtonStyle: PostId -> Element Msg
unpinButtonStyle id =
    Input.button [Background.color background
                  , Font.color foreground
                  , Font.size 10
                  , paddingXY 1 1]
        { onPress = Just (UnpinPost id), label = Icons.pinned <| Icons.tiny }

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
    Input.button [Font.italic, Font.semiBold, Font.color (if userId==Nothing then black else blue)]
        { onPress = Maybe.map (DisplayPage << UserPage) userId, label = Element.text ("@" ++ pseudo) }

userPseudoStyle: String -> Maybe UserId -> Element Msg
userPseudoStyle pseudo userId =
    Input.button [Font.italic, Font.semiBold, Font.color foreground]
        { onPress = Maybe.map (DisplayPage << UserPage) userId, label = Element.text ("@" ++ pseudo) }

linkStyle: Url -> String -> Element msg
linkStyle (Url url) txt =
    link [Background.color background
          , Font.color foreground
          , Font.italic]
        { url = url, label = Element.text txt }

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
    el [Font.semiBold, Font.size (fontSize + 2)] (text title)
    , paragraph [Font.size fontSize] [text content]
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

verticalSeparator: Int -> Color -> Element Msg
verticalSeparator width col =
    el [Border.color col, height fill, Border.widthEach {left = width, top = 0, bottom = 0, right = 0} ] empty


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
headerDateStyle e = el
    [ Background.color background
      , Font.color foreground
      , Font.size 10
    ] e

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
