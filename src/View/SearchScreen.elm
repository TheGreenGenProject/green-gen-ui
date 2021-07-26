module View.SearchScreen exposing (searchScreen)

import Data.Page as Page
import Data.Post exposing (Post, PostId)
import Data.Schedule exposing (UTCTimestamp)
import Data.User as User
import Element exposing (Element, centerX, centerY, column, el, fill, height, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import State.AppState exposing (AppState)
import State.Cache as Cache exposing (Cache)
import State.PostPage as PostPage exposing (PostPage)
import State.SearchState as SearchState exposing (SearchFilter(..), SearchState)
import Update.Msg exposing (Msg(..))
import View.Chart.ColorScheme
import View.Chart.WordCloud exposing (hashtagCloud)
import View.InfiniteScroll exposing (infiniteScroll)
import View.PostRenderer exposing (renderPostId)
import View.ScreenUtils
import View.Style exposing (empty, followHashtagStyle, unfollowHashtagStyle)
import View.Theme exposing (background, foreground)


searchScreen: AppState -> Element Msg
searchScreen state = column [
        width fill
        , height fill
        , centerX
        , spacing 5] [
        renderSearchFilter state.cache state.search.filter
        , renderSearchState state.timestamp state.cache state.search ]

renderSearchState: UTCTimestamp -> Cache -> SearchState -> Element Msg
renderSearchState tmstp cache state = case SearchState.allUpToCurrentPage state of
    Just page -> if PostPage.isEmpty page
        then renderHashtagCloud cache
        else (el [width fill, height fill] (renderPostPage tmstp cache page))
         |> infiniteScroll "search" (ChangeSearchPage (Page.next state.currentPage))
    Nothing   -> renderHashtagCloud cache

renderPostPage: UTCTimestamp -> Cache -> PostPage -> Element Msg
renderPostPage tmstp cache page = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 10]
    <| List.map (renderSinglePost tmstp cache) page.posts

renderSinglePost: UTCTimestamp -> Cache -> PostId -> Element Msg
renderSinglePost = renderPostId

renderNoPostPage: Element Msg
renderNoPostPage = View.ScreenUtils.emptyScreen "No search results"

renderSearchFilter: Cache -> SearchFilter -> Element Msg
renderSearchFilter cache filter = case filter of
    EmptySearch        -> empty
    ByHashtag []       -> empty
    ByHashtag hashtags -> hashtags
        |> List.map (\ht -> if Cache.containsFollowingHashtag cache ht then unfollowHashtagStyle ht else followHashtagStyle ht)
        |> row [spacing 10, padding 10]
    ByAuthor userId    -> let pseudo = userId |> Cache.getUserPseudo cache |> Maybe.withDefault (User.toString userId) in
        row [spacing 10] [el [Font.italic, Font.semiBold] (pseudo |> text)]

renderHashtagCloud: Cache -> Element Msg
renderHashtagCloud cache = let whiteScheme = {default = foreground, colors = [foreground] }
    in cache.hashtagTrend
        |> Maybe.map (\trend -> el [Background.color background, centerX, centerY, padding 20, Border.rounded 20] (hashtagCloud whiteScheme 96 trend))
        |> Maybe.withDefault Element.none

