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
import View.Style exposing (headerDateStyle, horizontalSeparator, italic, loadingFixedTextLine, loadingTextBlock, multiLineQuotedText, size, standard, userStyle)
import View.Theme exposing (background)


renderUserId: Cache -> UserId -> Element Msg
renderUserId cache userId = userId
    |> Cache.getUser cache
    |> Maybe.map (renderUser cache)
    |> Maybe.withDefault ("<Unable to render user info>" |> text)

renderUser: Cache -> UserInfo -> Element Msg
renderUser _ user =
    column [width fill, alignLeft, spacing 5, padding 10 ]
        [renderHeader user, horizontalSeparator 1 background, renderUserDesc user]

renderHeader: UserInfo -> Element Msg
renderHeader user = row [alignLeft, spacing 5] [
    userLogo |> standard,
    userStyle user.pseudo (Just user.id) |> size 10,
    ("since " ++ (user.since |> DateUtils.formatDate)) |> italic |> headerDateStyle
 ]

renderUserDesc: UserInfo -> Element Msg
renderUserDesc user = user.introduction
    |> multiLineQuotedText
    |> el [Font.italic, Font.size 10, paddingEach {left = 40, top = 0, right = 0, bottom = 0}]

userLogo: Element Msg
userLogo = let render = el [width <| px 20, height <| px 20, Border.width 2, centerX, centerY]
               center = el [centerX, centerY] >> render
           in Icons.tiny |> Icons.user |> center

-- Render a loading post
renderLoadingSingleUser:  Element Msg
renderLoadingSingleUser =
    column [width fill, alignLeft, spacing 5, padding 10 ]
        [row [alignLeft] [loadingFixedTextLine 12 16, loadingFixedTextLine 12 100]
         |> el [width fill, spacing 5, paddingEach {left=0,top=0,bottom=2,right=0}, Border.widthEach {left=0,top=0,bottom=2,right=0}, Border.color background]
         , loadingTextBlock 12 3]

renderLoadingUserPage: Int -> Element Msg
renderLoadingUserPage count = List.range 1 count
    |> List.map (\_ -> renderLoadingSingleUser)
    |> column [width fill , height fill, centerX, spacing 5, padding 10]
