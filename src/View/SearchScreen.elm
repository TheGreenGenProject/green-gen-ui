module View.SearchScreen exposing (searchScreen)

import Data.Hashtag as Hashtag exposing (Hashtag(..))
import Data.Post exposing (Post, PostId)
import Data.Schedule exposing (UTCTimestamp)
import Data.User as User
import Element exposing (Element, centerX, column, el, fill, height, padding, row, scrollbarY, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import State.AppState exposing (AppState)
import State.Cache as Cache exposing (Cache)
import State.PostPage as PostPage exposing (PostPage)
import State.SearchState as SearchState exposing (SearchFilter(..), SearchState)
import Update.Msg exposing (Msg(..))
import View.Chart.ColorScheme as ColorScheme
import View.Chart.WordCloud exposing (hashtagCloud)
import View.PostRenderer exposing (renderPostId)
import View.ScreenUtils
import View.Style exposing (empty, paged, searchBar)
import View.Theme exposing (background, foreground)


searchScreen: AppState -> Element Msg
searchScreen state = column [
        width fill
        , height fill
        , centerX
        , spacing 10 ]
    <| [ row [width fill, spacing 10] [
            renderSmallHashtagCloud state.cache
            , searchField state]
        , renderSearchState state.timestamp state.cache state.search ]


renderSearchState: UTCTimestamp -> Cache -> SearchState -> Element Msg
renderSearchState tmstp cache state = case SearchState.firstPage state of
    Just page -> if PostPage.isEmpty page
        then renderHashtagCloud cache
        else column [width fill, height fill]
            [renderSearchFilter cache state.filter
            , renderPostPage tmstp cache page |> paged state.currentPage (\p -> ChangeSearchPage p) True]
    Nothing   -> renderNoPostPage

renderPostPage: UTCTimestamp -> Cache -> PostPage -> Element Msg
renderPostPage tmstp cache page = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 10
        , scrollbarY]
    <| List.map (renderSinglePost tmstp cache) page.posts

renderSinglePost: UTCTimestamp -> Cache -> PostId -> Element Msg
renderSinglePost = renderPostId

renderNoPostPage: Element Msg
renderNoPostPage = View.ScreenUtils.emptyScreen "No search results"

renderSearchFilter: Cache -> SearchFilter -> Element Msg
renderSearchFilter cache filter = case filter of
    EmptySearch        -> empty
    ByHashtag []       -> empty
    ByHashtag hashtags -> let criterion = hashtags |> List.map Hashtag.format |> String.join " " in
        row [spacing 10] [text "Search filter: ", el [Font.italic, Font.semiBold] (text criterion)]
    ByAuthor userId    -> let pseudo = userId |> Cache.getUserPseudo cache |> Maybe.withDefault (User.toString userId) in row [spacing 10] [
        text "user:", el [Font.italic, Font.semiBold] (pseudo |> text)]

searchField : AppState -> Element Msg
searchField = searchBar

renderHashtagCloud: Cache -> Element Msg
renderHashtagCloud cache = cache.hashtagTrend
    |> Maybe.map (\trend -> hashtagCloud ColorScheme.wordCloudScheme 96 trend)
    |> Maybe.withDefault Element.none

-- Small word cloud
renderSmallHashtagCloud: Cache -> Element Msg
renderSmallHashtagCloud cache = let whiteScheme = {default = foreground, colors = [foreground] }
    in cache.hashtagTrend
        |> Maybe.map (\trend -> hashtagCloud whiteScheme 24 (List.take 10 trend))
        |> Maybe.map (\elt -> el [Background.color background, Border.color background, Border.width 5, Border.rounded 5] elt)
        |> Maybe.withDefault Element.none
