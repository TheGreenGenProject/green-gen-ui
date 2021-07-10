module View.WallScreen exposing (wallScreen)

import Basics as Int
import Data.Post exposing (Post, PostId)
import Data.Rank as Rank exposing (Score(..), ScoreBreakdown)
import Data.Schedule exposing (UTCTimestamp(..))
import Data.User exposing (UserId)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import State.AppState exposing (AppState)
import State.Cache as Cache exposing (Cache)
import State.PostPage exposing (PostPage)
import State.UserState exposing (UserInfo, UserState)
import State.WallState as WallState exposing (WallState)
import Update.Msg exposing (Msg(..))
import Utils.DateUtils as DateUtils
import View.Chart.ChartUtils as ChartUtils
import View.Chart.Donut as Donut
import View.Icons as Icons
import View.PostRenderer exposing (renderPostId)
import View.ScreenUtils
import View.Style exposing (multiLineQuotedText, paged, verticalSeparator)
import View.Theme exposing (background, foreground, lightPurple)


wallScreen: AppState -> Element Msg
wallScreen state = column [
        width fill
        , height fill
        , scrollbarY
        , centerX
        , spacing 5
        , padding 5 ]
    [ renderUserHeader state.cache state.wall, renderWallState state.timestamp state.cache state.wall]
    |> paged state.wall.currentPage (\p -> ChangeWallPage p) (WallState.hasMorePost state.wall)

renderUserHeader: Cache -> WallState -> Element Msg
renderUserHeader cache state =
    let maybeUser        = state.user |> Maybe.andThen (Cache.getUser cache)
        pseudo           = maybeUser  |> Maybe.map (.pseudo) |> Maybe.withDefault "<Unknown>"
        since            = maybeUser  |> Maybe.map (\x -> x.since |> DateUtils.formatDate) |> Maybe.withDefault "Forever !"
        introduction     = maybeUser  |> Maybe.map (\x -> x.introduction) |> Maybe.withDefault ""
        scoreBreakdown   = maybeUser  |> Maybe.map (.id)
                                      |> Maybe.andThen (Cache.getScoreBreakdown cache)
                                      |> Maybe.withDefault Rank.emptyBreakdown
        score            = Rank.score scoreBreakdown
    in row [width fill, spacing 10] [
        el [Font.color foreground, Background.color background, Border.rounded 10] (Icons.user Icons.extraLarge)
        , column [spacing 10, height fill, Border.color background] [
            row [spacing 5, centerY] [text pseudo |> el [Font.size 15, Font.semiBold]
                                      , renderFollowingButton cache state.user]
            , score |> Rank.fromScore |> Rank.toString |> text |> el [Font.size 10, Font.italic, centerY]
            , "Since " ++ since  |> text |> el [Font.size 12, Font.italic, centerY]
          ]
        , verticalSeparator 1 background
        , (renderChart scoreBreakdown)
        , verticalSeparator 1 background
        , paragraph [height fill
                    , width fill
                    , Font.size 12
                    , Font.italic]
            [introduction |> multiLineQuotedText]
 ]

renderFollowingButton: Cache -> Maybe UserId -> Element Msg
renderFollowingButton cache maybeUserId = maybeUserId
    |> Maybe.map (\userId -> if Cache.containsFollowingUser cache userId then unfollowButtonStyle userId else followButtonStyle userId)
    |> Maybe.withDefault Element.none

renderWallState: UTCTimestamp -> Cache -> WallState -> Element Msg
renderWallState tmstp cache state = case WallState.currentPage state of
    Just page -> renderPostPage tmstp cache page
    Nothing   -> renderNoPostPage

renderPostPage: UTCTimestamp -> Cache -> PostPage -> Element Msg
renderPostPage tmstp cache page = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 10 ]
    <| List.map (renderSinglePost tmstp cache) page.posts

renderSinglePost: UTCTimestamp -> Cache -> PostId -> Element Msg
renderSinglePost = renderPostId

renderNoPostPage: Element Msg
renderNoPostPage = View.ScreenUtils.emptyScreen "No posts"

renderChart: ScoreBreakdown -> Element Msg
renderChart breakdown = let score = Rank.score breakdown
                            rank  = Rank.fromScore score
                            nextRankScore = rank |> Rank.next   |> Rank.maxScore |> Int.toFloat
                            fromLikes   = breakdown.fromLikes   |> Int.toFloat
                            fromFollows = breakdown.fromFollows |> Int.toFloat
                            fromPosts   = breakdown.fromPosts   |> Int.toFloat
                            fromEvents  = breakdown.fromEvents  |> Int.toFloat
                            remainder   = nextRankScore - (fromLikes + fromFollows + fromPosts + fromEvents) |> max 0.0
    in row [spacing 10] [
        Donut.donut [(fromLikes + fromFollows + fromPosts + fromEvents, background), (remainder, lightPurple)]
        , ChartUtils.legend [Font.size 10] [
            ("Likes: " ++ String.fromFloat fromLikes, background)
            , ("Follow: " ++ String.fromFloat fromFollows, background)
            , ("Posts: " ++ String.fromFloat fromPosts, background)
            , ("Event: " ++ String.fromFloat fromEvents, background)
            , ("Remaining: " ++ String.fromFloat remainder, lightPurple)
         ]
      ]


-- Helpers

followButtonStyle: UserId -> Element Msg
followButtonStyle id =
    Input.button [Font.size 11
                  , paddingXY 2 2
                  , Border.width 1
                  , Border.rounded 5]
        { onPress = Just (FollowUser id), label = Element.text "Follow" }

unfollowButtonStyle: UserId -> Element Msg
unfollowButtonStyle id =
    Input.button [Font.size 11
                  , paddingXY 2 2
                  , Border.width 1
                  , Border.rounded 5]
        { onPress = Just (UnfollowUser id), label = Element.text "Unfollow" }