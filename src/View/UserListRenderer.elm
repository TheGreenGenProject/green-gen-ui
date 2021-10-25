module View.UserListRenderer exposing (renderUserId, renderUser, renderLoadingUserPage)

import Data.User exposing (User, UserId)
import Element exposing (Element, alignLeft, centerX, centerY, column, el, fill, height, padding, paddingEach, px, row, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import State.Cache as Cache exposing (Cache)
import State.UserState exposing (UserInfo)
import Update.Msg exposing (Msg)
import Utils.DateUtils as DateUtils
import View.Icons as Icons
import View.Style exposing (headerDateStyle, horizontalSeparator, italic, loadingFixedTextLine, loadingTextBlock, multiLineQuotedText, relFontSize, relSize, standard, userStyle)
import View.Theme
import View.UIStyle exposing (UIStyle)


renderUserId: UIStyle -> Cache -> UserId -> Element Msg
renderUserId ui cache userId = userId
    |> Cache.getUser cache
    |> Maybe.map (renderUser ui cache)
    |> Maybe.withDefault ("<Unable to render user info>" |> text)

renderUser: UIStyle -> Cache -> UserInfo -> Element Msg
renderUser ui _ user =
    column [width fill, alignLeft, spacing 5, padding 10 ]
        [renderHeader ui user, horizontalSeparator 1 ui.theme.background, renderUserDesc ui user]

renderHeader: UIStyle -> UserInfo -> Element Msg
renderHeader ui user = row [alignLeft, spacing 5] [
    userLogo ui |> standard ui,
    userStyle ui user.pseudo (Just user.id) |> relSize ui 0,
    ("since " ++ (user.since |> DateUtils.formatDate)) |> italic |> headerDateStyle ui
 ]

renderUserDesc: UIStyle -> UserInfo -> Element Msg
renderUserDesc ui user = user.introduction
    |> multiLineQuotedText ui
    |> el [Font.italic, relFontSize ui 0, paddingEach {left = 40, top = 0, right = 0, bottom = 0}]

userLogo: UIStyle -> Element Msg
userLogo ui = let render = el [width <| px 20, height <| px 20, Border.width 2, centerX, centerY]
                  center = el [centerX, centerY] >> render
           in ui.tiny |> Icons.user |> center

-- Render a loading post
renderLoadingSingleUser: UIStyle -> Element Msg
renderLoadingSingleUser ui =
    column [width fill, alignLeft, spacing 5, padding 10 ]
        [row [alignLeft] [loadingFixedTextLine ui 12 16, loadingFixedTextLine ui 12 100]
         |> el [width fill, spacing 5, paddingEach {left=0,top=0,bottom=2,right=0}, Border.widthEach {left=0,top=0,bottom=2,right=0}, Border.color ui.theme.background]
         , loadingTextBlock ui 12 3]

renderLoadingUserPage: UIStyle -> Int -> Element Msg
renderLoadingUserPage ui count = List.range 1 count
    |> List.map (\_ -> renderLoadingSingleUser ui)
    |> column [width fill , height fill, centerX, spacing 5, padding 10]
