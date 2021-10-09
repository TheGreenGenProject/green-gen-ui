module View.WallScreen exposing (wallScreen)

import Basics as Int
import Data.Page as Page
import Data.Post exposing (Post, PostId)
import Data.Rank as Rank exposing (Score(..), ScoreBreakdown)
import Data.Schedule exposing (UTCTimestamp(..))
import Data.User exposing (UserId)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import State.AppState exposing (AppState, Display(..))
import State.Cache as Cache exposing (Cache)
import State.GenericPage as GenericPage
import State.PostPageCache exposing (PostPage)
import State.UserState exposing (UserInfo, UserState)
import State.WallState as WallState exposing (WallState)
import Update.Msg exposing (Msg(..))
import Utils.DateUtils as DateUtils
import View.Chart.ChartUtils as ChartUtils
import View.Chart.Donut as Donut
import View.Icons as Icons
import View.InfiniteScroll exposing (infiniteScroll)
import View.PartnershipStyle as PartnershipStyle
import View.PostRenderer exposing (renderLoadingPostPage, renderPostId)
import View.Style exposing (loadingFixedTextLine, loadingTextBlock, multiLineQuotedText, verticalSeparator)
import View.Theme exposing (background, darkOrange, foreground, progressColor, remainingProgressColor)
import View.WelcomeWallScreen exposing (welcomeWallScreen)


wallScreen: AppState -> Element Msg
wallScreen state = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 5 ]
    [ renderUserHeader state.cache state.wall
      , renderWallState state ]
    |> infiniteScroll "wall" (ChangeWallPage (Page.next state.wall.currentPage))

renderUserHeader: Cache -> WallState -> Element Msg
renderUserHeader cache state =
    let maybeUser        = state.user |> Maybe.andThen (Cache.getUser cache)
        pseudo           = maybeUser  |> Maybe.map (.pseudo >> text >> el [Font.size 12, Font.semiBold])
                                      |> Maybe.withDefault (loadingFixedTextLine 12 100)
        since            = maybeUser  |> Maybe.map (\x -> x.since |> DateUtils.formatDate)
                                      |> Maybe.map (\txt -> "Since " ++ txt  |> text |> el [Font.size 12, Font.italic, centerY])
                                      |> Maybe.withDefault (loadingFixedTextLine 12 100)
        introduction     = maybeUser  |> Maybe.map (.introduction >> multiLineQuotedText >> List.singleton)
                                      |> Maybe.map (paragraph [height fill, width fill, Font.size 12, Font.italic])
                                      |> Maybe.withDefault (loadingTextBlock 12 4)
        scoreBreakdown   = maybeUser  |> Maybe.map (.id)
                                      |> Maybe.andThen (Cache.getScoreBreakdown cache)
                                      |> Maybe.withDefault Rank.emptyBreakdown
        score            = Rank.score scoreBreakdown
        rank             = maybeUser  |> Maybe.map (\_ -> score |> Rank.fromScore |> Rank.toString |> text |> el [Font.size 10, Font.italic, centerY])
                                      |> Maybe.withDefault (loadingFixedTextLine 12 50)
        isPartner        = maybeUser  |> Maybe.map (.id) |> Maybe.map(Cache.isPartner cache) |> Maybe.withDefault False
    in row [width fill, spacing 10] [
        el [Font.color foreground, Background.color background, Border.rounded 10] (Icons.user Icons.extraLarge)
        , column [spacing 10, height fill, Border.color background] [
            row [spacing 5, centerY] [pseudo, renderFollowingButton cache state.user]
            , rank
            , since
            , if isPartner
              then text "Partnership" |> el [Font.semiBold, Font.italic, Font.color darkOrange, Font.size 11]
              else Element.none
          ]
        , verticalSeparator 1 background
        , (renderChart scoreBreakdown)
        , verticalSeparator 1 background
        , introduction
 ]

renderPartnership: AppState -> Element Msg -> Element Msg
renderPartnership state elmnt = state.wall.user
    |> Maybe.map (\user -> PartnershipStyle.userWallDecoration state.cache user elmnt)
    |> Maybe.withDefault elmnt

renderFollowingButton: Cache -> Maybe UserId -> Element Msg
renderFollowingButton cache maybeUserId = maybeUserId
    |> Maybe.map (\userId -> if Cache.containsFollowingUser cache userId then unfollowButtonStyle userId else followButtonStyle userId)
    |> Maybe.withDefault Element.none

renderWallState: AppState -> Element Msg
renderWallState state = case WallState.allUpToCurrentPage state.wall of
    Just page -> if GenericPage.isEmpty page then renderNoPostPage state else renderPostPage state.timestamp state.cache page
    Nothing   -> renderLoadingPosts

renderPostPage: UTCTimestamp -> Cache -> PostPage -> Element Msg
renderPostPage tmstp cache page = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 10 ]
    <| List.map (renderSinglePost tmstp cache) page.items

renderSinglePost: UTCTimestamp -> Cache -> PostId -> Element Msg
renderSinglePost = renderPostId

renderNoPostPage: AppState -> Element Msg
renderNoPostPage = welcomeWallScreen

renderLoadingPosts: Element Msg
renderLoadingPosts = renderLoadingPostPage 2

renderChart: ScoreBreakdown -> Element Msg
renderChart breakdown = let score = Rank.score breakdown
                            rank  = Rank.fromScore score
                            nextRankScore = rank |> Rank.next   |> Rank.maxScore |> Int.toFloat
                            fromLikes   = breakdown.fromLikes   |> Int.toFloat
                            fromFollows = breakdown.fromFollows |> Int.toFloat
                            fromPosts   = breakdown.fromPosts   |> Int.toFloat
                            fromEvents  = breakdown.fromEvents  |> Int.toFloat
                            remainder   = nextRankScore - (fromLikes + fromFollows + fromPosts + fromEvents) |> max 0.0
                            progress    = progressColor
                            remaining   = remainingProgressColor
    in row [spacing 10] [
        Donut.donut [(fromLikes + fromFollows + fromPosts + fromEvents, progress), (remainder, remaining)]
        , ChartUtils.legend [Font.size 10] [
            ("Likes: " ++ String.fromFloat fromLikes, progress)
            , ("Follow: " ++ String.fromFloat fromFollows, progress)
            , ("Posts: " ++ String.fromFloat fromPosts, progress)
            , ("Event: " ++ String.fromFloat fromEvents, progress)
            , ("Remaining: " ++ String.fromFloat remainder, remaining)
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