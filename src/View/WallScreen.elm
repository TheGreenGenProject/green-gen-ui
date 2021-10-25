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
import View.InfiniteScroll exposing (infiniteScroll, infiniteScrollWithMoreButton)
import View.PartnershipStyle as PartnershipStyle
import View.PostRenderer exposing (renderLoadingPostPage, renderPostId)
import View.Style exposing (horizontalSeparator, loadingFixedTextLine, loadingTextBlock, multiLineQuotedText, relFontSize, verticalSeparator)
import View.Theme exposing (darkOrange)
import View.UIStyle as UIStyle exposing (UIStyle)
import View.WelcomeWallScreen exposing (welcomeWallScreen)


wallScreen: AppState -> Element Msg
wallScreen state = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 5 ]
    [ (if UIStyle.isMobile state.device then renderMobileUserHeader else renderUserHeader) state
      , renderWallState state ]
    |> infiniteScrollWithMoreButton state.uiStyle (UIStyle.isMobile state.device) "wall" (ChangeWallPage (Page.next state.wall.currentPage))

renderUserHeader: AppState -> Element Msg
renderUserHeader state =
    let maybeUser        = state.wall.user |> Maybe.andThen (Cache.getUser state.cache)
        pseudo           = maybeUser       |> Maybe.map (.pseudo >> text >> el [relFontSize state.uiStyle 2, Font.semiBold])
                                           |> Maybe.withDefault (loadingFixedTextLine state.uiStyle 12 100)
        since            = maybeUser       |> Maybe.map (\x -> x.since |> DateUtils.formatDate)
                                           |> Maybe.map (\txt -> "Since " ++ txt  |> text |> el [relFontSize state.uiStyle 2, Font.italic, centerY])
                                           |> Maybe.withDefault (loadingFixedTextLine state.uiStyle 12 100)
        introduction     = maybeUser       |> Maybe.map (.introduction >> multiLineQuotedText state.uiStyle >> List.singleton)
                                           |> Maybe.map (paragraph [height fill, width fill, relFontSize state.uiStyle 2, Font.italic])
                                           |> Maybe.withDefault (loadingTextBlock state.uiStyle 12 4)
        scoreBreakdown   = maybeUser       |> Maybe.map (.id)
                                           |> Maybe.andThen (Cache.getScoreBreakdown state.cache)
                                           |> Maybe.withDefault Rank.emptyBreakdown
        score            = Rank.score scoreBreakdown
        rank             = maybeUser       |> Maybe.map (\_ -> score |> Rank.fromScore |> Rank.toString |> text |> el [relFontSize state.uiStyle 0, Font.italic, centerY])
                                           |> Maybe.withDefault (loadingFixedTextLine state.uiStyle 12 50)
        isPartner        = maybeUser       |> Maybe.map (.id) |> Maybe.map(Cache.isPartner state.cache) |> Maybe.withDefault False
    in row [width fill, spacing 10] [
        el [Font.color state.uiStyle.theme.foreground, Background.color state.uiStyle.theme.background, Border.rounded 10] (Icons.user state.uiStyle.extraLarge)
        , column [spacing 10, height fill, Border.color state.uiStyle.theme.background] [
            row [spacing 5, centerY] [pseudo, renderFollowingButton state.uiStyle state.cache state.wall.user]
            , rank
            , since
            , if isPartner
              then text "Partnership" |> el [Font.semiBold, Font.italic, Font.color darkOrange, relFontSize state.uiStyle 1]
              else Element.none
          ]
        , verticalSeparator 1 state.uiStyle.theme.background
        , (renderChart state.uiStyle scoreBreakdown)
        , verticalSeparator 1 state.uiStyle.theme.background
        , introduction
 ]

renderMobileUserHeader: AppState -> Element Msg
renderMobileUserHeader state =
    let maybeUser        = state.wall.user |> Maybe.andThen (Cache.getUser state.cache)
        pseudo           = maybeUser       |> Maybe.map (.pseudo >> text >> el [relFontSize state.uiStyle 2, Font.semiBold])
                                           |> Maybe.withDefault (loadingFixedTextLine state.uiStyle 12 100)
        since            = maybeUser       |> Maybe.map (\x -> x.since |> DateUtils.formatDate)
                                           |> Maybe.map (\txt -> "Since " ++ txt  |> text |> el [relFontSize state.uiStyle 2, Font.italic, centerY])
                                           |> Maybe.withDefault (loadingFixedTextLine state.uiStyle 12 100)
        introduction     = maybeUser       |> Maybe.map (.introduction >> multiLineQuotedText state.uiStyle >> List.singleton)
                                           |> Maybe.map (paragraph [height fill, width fill, relFontSize state.uiStyle 2, Font.italic])
                                           |> Maybe.withDefault (loadingTextBlock state.uiStyle 12 4)
        scoreBreakdown   = maybeUser       |> Maybe.map (.id)
                                           |> Maybe.andThen (Cache.getScoreBreakdown state.cache)
                                           |> Maybe.withDefault Rank.emptyBreakdown
        score            = Rank.score scoreBreakdown
        rank             = maybeUser       |> Maybe.map (\_ -> score |> Rank.fromScore |> Rank.toString |> text |> el [relFontSize state.uiStyle 0, Font.italic, centerY])
                                           |> Maybe.withDefault (loadingFixedTextLine state.uiStyle 12 50)
        isPartner        = maybeUser       |> Maybe.map (.id) |> Maybe.map(Cache.isPartner state.cache) |> Maybe.withDefault False
    in column [width fill, spacing 10] [
        row [width fill, spacing 10] [
        el [Font.color state.uiStyle.theme.foreground, Background.color state.uiStyle.theme.background, Border.rounded 10]
            (Icons.user state.uiStyle.large)
        , column [spacing 10, height fill, Border.color state.uiStyle.theme.background] [
            row [spacing 5, centerY] [pseudo, renderFollowingButton state.uiStyle state.cache state.wall.user]
            , rank
            , since
            , if isPartner
              then text "Partnership" |> el [Font.semiBold, Font.italic, Font.color darkOrange, relFontSize state.uiStyle 1]
              else Element.none
          ]
        , verticalSeparator 1 state.uiStyle.theme.background
        , (renderChart state.uiStyle scoreBreakdown)
       ]
       , horizontalSeparator 1 state.uiStyle.theme.background
       , el [width fill] introduction
       , horizontalSeparator 1 state.uiStyle.theme.background
 ]

renderPartnership: AppState -> Element Msg -> Element Msg
renderPartnership state elmnt = state.wall.user
    |> Maybe.map (\user -> PartnershipStyle.userWallDecoration state.cache user elmnt)
    |> Maybe.withDefault elmnt

renderFollowingButton: UIStyle -> Cache -> Maybe UserId -> Element Msg
renderFollowingButton ui cache maybeUserId = maybeUserId
    |> Maybe.map (\userId -> if Cache.containsFollowingUser cache userId then unfollowButtonStyle ui userId else followButtonStyle ui userId)
    |> Maybe.withDefault Element.none

renderWallState: AppState -> Element Msg
renderWallState state = case WallState.allUpToCurrentPage state.wall of
    Just page -> if GenericPage.isEmpty page then renderNoPostPage state else renderPostPage state page
    Nothing   -> renderLoadingPosts state.uiStyle

renderPostPage: AppState -> PostPage -> Element Msg
renderPostPage state page = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 10 ]
    <| List.map (renderSinglePost state.uiStyle state.timestamp state.cache) page.items

renderSinglePost: UIStyle -> UTCTimestamp -> Cache -> PostId -> Element Msg
renderSinglePost = renderPostId

renderNoPostPage: AppState -> Element Msg
renderNoPostPage = welcomeWallScreen

renderLoadingPosts: UIStyle -> Element Msg
renderLoadingPosts ui = renderLoadingPostPage ui 2

renderChart: UIStyle -> ScoreBreakdown -> Element Msg
renderChart ui breakdown = let score = Rank.score breakdown
                               rank  = Rank.fromScore score
                               nextRankScore = rank |> Rank.next   |> Rank.maxScore |> Int.toFloat
                               fromLikes   = breakdown.fromLikes   |> Int.toFloat
                               fromFollows = breakdown.fromFollows |> Int.toFloat
                               fromPosts   = breakdown.fromPosts   |> Int.toFloat
                               fromEvents  = breakdown.fromEvents  |> Int.toFloat
                               remainder   = nextRankScore - (fromLikes + fromFollows + fromPosts + fromEvents) |> max 0.0
                               progress    = ui.theme.progressColor
                               remaining   = ui.theme.remainingProgressColor
    in row [spacing 10] [
        Donut.smallDonut ui [(fromLikes + fromFollows + fromPosts + fromEvents, progress), (remainder, remaining)]
        , ChartUtils.legend ui [relFontSize ui -1] [
            ("Likes: " ++ String.fromFloat fromLikes, progress)
            , ("Follow: " ++ String.fromFloat fromFollows, progress)
            , ("Posts: " ++ String.fromFloat fromPosts, progress)
            , ("Event: " ++ String.fromFloat fromEvents, progress)
            , ("Remaining: " ++ String.fromFloat remainder, remaining)
         ]
      ]


-- Helpers

followButtonStyle: UIStyle -> UserId -> Element Msg
followButtonStyle ui id =
    Input.button [relFontSize ui 1
                  , paddingXY 2 2
                  , Border.width 1
                  , Border.rounded 5]
        { onPress = Just (FollowUser id), label = Element.text "Follow" }

unfollowButtonStyle: UIStyle -> UserId -> Element Msg
unfollowButtonStyle ui id =
    Input.button [relFontSize ui 1
                  , paddingXY 2 2
                  , Border.width 1
                  , Border.rounded 5]
        { onPress = Just (UnfollowUser id), label = Element.text "Unfollow" }