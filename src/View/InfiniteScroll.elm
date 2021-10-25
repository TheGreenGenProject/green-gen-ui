module View.InfiniteScroll exposing (
    infiniteScroll
    , infiniteScrollWithMoreButton
    , biDirectionalInfiniteScroll
    , loadMoreIfNeeded
    , loadLessIfNeeded
    , loadMoreOrLessIfNeeded)

import Browser.Dom exposing (Viewport)
import Element exposing (Element, centerX, column, el, fill, height, padding, scrollbarY, spacing, width)
import Element.Border as Border
import Element.Input as Input
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Task
import Update.Msg exposing (Msg(..))
import View.Style as Style exposing (noSelectionEffect, relSize)
import View.UIStyle exposing (UIStyle)


infinitePrefix = "infinite-scroll-"

-- Loads only in one direction
infiniteScroll: String -> Msg -> Element Msg -> Element Msg
infiniteScroll id scrollMsg elt =
    let onScroll : msg -> Element.Attribute msg
        onScroll msg = Element.htmlAttribute (Html.Events.on "scroll" (Decode.succeed msg))
        htmlId: String-> Element.Attribute msg
        htmlId ref = Element.htmlAttribute (Html.Attributes.id (infinitePrefix ++ ref))
        onScrollMsg = LoadMore (infinitePrefix ++ id) scrollMsg
    in el [width fill, height fill, scrollbarY, onScroll onScrollMsg, htmlId id] elt

-- Loads only in one direction
infiniteScrollWithMoreButton: UIStyle -> Bool -> String -> Msg -> Element Msg -> Element Msg
infiniteScrollWithMoreButton ui showButton id scrollMsg elt =
    let onScroll : msg -> Element.Attribute msg
        onScroll msg = Element.htmlAttribute (Html.Events.on "scroll" (Decode.succeed msg))
        htmlId: String-> Element.Attribute msg
        htmlId ref = Element.htmlAttribute (Html.Attributes.id (infinitePrefix ++ ref))
        onScrollMsg = LoadMore (infinitePrefix ++ id) scrollMsg
        loadMoreButton = Input.button [
            centerX
            , padding 1
            , noSelectionEffect
            , Border.width 0] {
                onPress = LoadMore (infinitePrefix ++ id) scrollMsg |> Just
                , label = (". . ." |> Style.bold |> relSize ui 3)
            }
    in el [width fill, height fill, scrollbarY, onScroll onScrollMsg, htmlId id]
        (column [width fill, height fill, spacing 2] [
            elt
            , if showButton then loadMoreButton else Element.none
        ])


-- Loads in both direction ("more" nd "less")
biDirectionalInfiniteScroll: String -> Msg -> Msg -> Element Msg -> Element Msg
biDirectionalInfiniteScroll id scrollLess scrollMore elt =
    let onScroll : msg -> Element.Attribute msg
        onScroll msg = Element.htmlAttribute (Html.Events.on "scroll" (Decode.succeed msg))
        htmlId: String-> Element.Attribute msg
        htmlId ref = Element.htmlAttribute (Html.Attributes.id (infinitePrefix ++ ref))
        onScrollMsg = LoadMoreOrLess (infinitePrefix ++ id) scrollLess scrollMore
    in el [width fill, height fill, scrollbarY, onScroll onScrollMsg, htmlId id] elt


-- type alias Viewport =
--    { scene :
--        { width : Float
--        , height : Float
--        }
--    , viewport :
--        { x : Float
--        , y : Float
--        , width : Float
--        , height : Float
--        }
--    }
loadMoreIfNeeded: String -> Msg -> Cmd Msg
loadMoreIfNeeded ref loadMoreMsg =
    let viewport = Debug.log ("Browser.Dom.getViewportOf " ++ Debug.toString ref) Browser.Dom.getViewportOf ref in
    viewport
        |> Task.map (\vp -> if endOfViewport vp |> Debug.log "Reached end of viewport: " then loadMoreMsg else NoOp)
        |> Task.attempt (\syncMsg -> case syncMsg of
            Ok msg -> msg
            Err b -> NoOp
        )

endOfViewport: Viewport -> Bool
endOfViewport vp = vp.viewport.y + vp.viewport.height >= vp.scene.height - 5
    |> Debug.log ("EOV: Viewport" ++ (Debug.toString vp) ++ ":")

loadLessIfNeeded: String -> Msg -> Cmd Msg
loadLessIfNeeded ref loadPrevMsg =
    let viewport = Debug.log ("Browser.Dom.getViewportOf " ++ Debug.toString ref) Browser.Dom.getViewportOf ref in
    viewport
        |> Task.map (\vp -> if startOfViewport vp |> Debug.log "Reached start of viewport: " then loadPrevMsg else NoOp)
        |> Task.attempt (\syncMsg -> case syncMsg of
            Ok msg -> msg
            Err b -> NoOp
        )

startOfViewport: Viewport -> Bool
startOfViewport vp = vp.viewport.y== 0
    |> Debug.log ("SOV: Viewport" ++ (Debug.toString vp) ++ ":")

loadMoreOrLessIfNeeded: String -> Msg -> Msg -> Cmd Msg
loadMoreOrLessIfNeeded ref loadLess loadMore =
    let viewport = Debug.log ("Browser.Dom.getViewportOf " ++ Debug.toString ref) Browser.Dom.getViewportOf ref in
    viewport
        |> Task.map (\vp -> if startOfViewport vp |> Debug.log "Reached start of viewport: " then loadLess
                            else if endOfViewport vp |> Debug.log "Reached end of viewport: " then loadMore
                            else NoOp)
        |> Task.attempt (\syncMsg -> case syncMsg of
            Ok msg -> msg
            Err b -> NoOp
        )