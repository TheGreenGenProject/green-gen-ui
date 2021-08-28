module View.PartnershipStyle exposing (..)


import Data.Post exposing (PostId)
import Data.User exposing (UserId)
import Element exposing (Element, el, fill, paddingEach, width)
import Element.Background as Background
import Element.Border as Border
import State.Cache as Cache exposing (Cache)
import Update.Msg exposing (Msg)
import View.Theme exposing (lightOrange, lighterYellow)



postDecoration: Cache -> PostId -> Element Msg -> Element Msg
postDecoration cache postId elmt =
    if Cache.hasPartnership cache postId
    then elmt |> el [width fill, Background.color lighterYellow, Border.width 2, Border.color lightOrange, Border.rounded 6]
    else elmt

userWallDecoration: Cache -> UserId -> Element Msg -> Element Msg
userWallDecoration cache userId elmt =
    if Cache.isPartner cache userId
    then elmt |> el [width fill
        , Background.color lighterYellow
        , Border.width 2
        , Border.color lightOrange
        , Border.rounded 6
        , paddingEach {top=5, bottom=5,left=2, right=2}]
    else elmt