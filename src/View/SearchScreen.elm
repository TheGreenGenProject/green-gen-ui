module View.SearchScreen exposing (searchScreen)

import Basics as Int
import Data.Page as Page
import Data.Post exposing (Post, PostId)
import Data.Schedule exposing (UTCTimestamp)
import Data.User as User exposing (UserId)
import Element exposing (Element, alignLeft, centerX, centerY, column, el, fill, height, padding, row, scrollbarX, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import State.AppState exposing (AppState, WindowSize)
import State.Cache as Cache exposing (Cache)
import State.GenericPage as GenericPage
import State.PostPageCache exposing (PostPage)
import State.SearchState as SearchState exposing (PostType(..), SearchFilter(..), SearchState, allPostTypes, postTypeFilter)
import State.UserPageCache exposing (UserPage)
import Update.Msg exposing (Msg(..))
import View.Chart.ColorScheme
import View.Chart.WordCloud exposing (hashtagCloud)
import View.InfiniteScroll exposing (infiniteScroll, infiniteScrollWithMoreButton)
import View.PostRenderer exposing (renderLoadingPostPage, renderPostId)
import View.ScreenUtils
import View.Style exposing (empty, followHashtagStyle, tabButton, unfollowHashtagStyle)
import View.UIStyle as UIStyle exposing (UIStyle)
import View.UserListRenderer exposing (renderLoadingUserPage, renderUserId)


searchScreen: AppState -> Element Msg
searchScreen state = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 5] [
        renderSearchFilter state.uiStyle state.cache state.search.filter
        , renderSearchState state ]

renderSearchState: AppState -> Element Msg
renderSearchState state =
    if SearchState.isPostSearchFilter state.search.filter
    then renderPostSearchState state
    else renderUserSearchState state

renderPostSearchState: AppState -> Element Msg
renderPostSearchState state = case SearchState.allUpToCurrentPostPage state.search of
    Just page -> if GenericPage.isEmpty page
        then renderHashtagCloud state
        else (el [width fill, height fill] (renderPostPage state page))
         |> infiniteScrollWithMoreButton state.uiStyle (UIStyle.isMobile state.device) "search-posts" (ChangeSearchPostPage (Page.next state.search.currentPage))
    Nothing   -> renderLoadingPosts state.uiStyle

renderUserSearchState: AppState -> Element Msg
renderUserSearchState state = case SearchState.allUpToCurrentUserPage state.search of
    Just page -> if GenericPage.isEmpty page
        then renderNoResultPage state.uiStyle
        else (el [width fill, height fill] (renderUserPage state.uiStyle state.cache page))
         |> infiniteScrollWithMoreButton state.uiStyle (UIStyle.isMobile state.device) "search-users" (ChangeSearchPostPage (Page.next state.search.currentPage))
    Nothing   -> renderLoadingUsers state.uiStyle

renderPostSearchFilterTabs: UIStyle -> SearchFilter -> Element Msg
renderPostSearchFilterTabs ui filter = case postTypeFilter filter of
    Nothing -> Element.none
    Just pt ->  allPostTypes
        |> List.map (\x -> renderPostTypeTab ui x (x==pt))
        |> wrappedRow [alignLeft, spacing 5]

renderPostPage: AppState -> PostPage -> Element Msg
renderPostPage state page = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 10]
    <| List.map (renderSinglePost state.uiStyle state.timestamp state.cache) page.items

renderSinglePost: UIStyle -> UTCTimestamp -> Cache -> PostId -> Element Msg
renderSinglePost = renderPostId

renderLoadingPosts: UIStyle -> Element Msg
renderLoadingPosts ui = renderLoadingPostPage ui 2

renderUserPage: UIStyle -> Cache -> UserPage -> Element Msg
renderUserPage ui cache page = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 10]
    <| List.map (renderSingleUser ui cache) page.items

renderSingleUser: UIStyle -> Cache -> UserId -> Element Msg
renderSingleUser ui = renderUserId ui

renderLoadingUsers: UIStyle -> Element Msg
renderLoadingUsers ui = renderLoadingUserPage ui 2

renderSearchFilter: UIStyle -> Cache -> SearchFilter -> Element Msg
renderSearchFilter ui cache filter = case filter of
    EmptySearch          -> empty
    ByHashtag [] _       -> empty
    ByUserPrefix _       -> empty
    ByHashtag hashtags _ -> hashtags
        |> List.map (\ht -> if Cache.containsFollowingHashtag cache ht then unfollowHashtagStyle ui ht else followHashtagStyle ui ht)
        |> row [spacing 10, padding 10]
        |> above (renderPostSearchFilterTabs ui filter)
    ByAuthor userId  _   -> let pseudo = userId |> Cache.getUserPseudo cache |> Maybe.withDefault (User.toString userId) in
        row [spacing 10] [el [Font.italic, Font.semiBold] (pseudo |> text)]

renderHashtagCloud: AppState -> Element Msg
renderHashtagCloud state =
    let whiteScheme = {default = state.uiStyle.theme.foreground, colors = [state.uiStyle.theme.foreground] }
        fontSize = computeFontFromWindowSize state.windowSize
    in state.cache.hashtagTrend
        |> Maybe.map (\trend -> el [centerX, centerY] (hashtagCloud whiteScheme fontSize trend))
        |> Maybe.map (el [width fill, height fill, centerX, centerY, Background.color state.uiStyle.theme.background, Border.rounded 20])
        |> Maybe.map (el [width fill, height fill, centerX, centerY, padding 20])
        |> Maybe.withDefault Element.none

computeFontFromWindowSize: WindowSize -> Int
computeFontFromWindowSize {width, height} =
    let refWindowWidth = 200
        scaled = Element.modular (Int.toFloat 48) 1.25
    in scaled (width // refWindowWidth) |> round

renderNoResultPage: UIStyle -> Element Msg
renderNoResultPage ui = View.ScreenUtils.emptyScreen ui "No results"

renderPostTypeTab: UIStyle -> PostType -> Bool -> Element Msg
renderPostTypeTab ui pt selected =
    let renderTabButton: String -> Element Msg
        renderTabButton label = tabButton ui label (ChangeSearchPostTypeFilter pt) selected
    in case pt of
        AllPostTypes   -> renderTabButton "All"
        TipPosts       -> renderTabButton "Tips"
        ChallengePosts -> renderTabButton "Challenges"
        EventPosts     -> renderTabButton "Events"
        PollPosts      -> renderTabButton "Polls"
        FreeTextPosts  -> renderTabButton "Free posts"

above: Element Msg -> Element Msg -> Element Msg
above el1 el2 = column [width fill] [el1, el2]