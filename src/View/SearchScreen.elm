module View.SearchScreen exposing (searchScreen)

import Basics as Int
import Data.Page as Page
import Data.Post exposing (Post, PostId)
import Data.Schedule exposing (UTCTimestamp)
import Data.User as User exposing (UserId)
import Element exposing (Element, alignLeft, centerX, centerY, column, el, fill, height, padding, row, spacing, text, width)
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
import View.InfiniteScroll exposing (infiniteScroll)
import View.PostRenderer exposing (renderLoadingPostPage, renderPostId)
import View.ScreenUtils
import View.Style exposing (empty, followHashtagStyle, tabButton, unfollowHashtagStyle)
import View.Theme exposing (background, foreground)
import View.UserListRenderer exposing (renderLoadingUserPage, renderUserId)


searchScreen: AppState -> Element Msg
searchScreen state = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 5] [
        renderSearchFilter state.cache state.search.filter
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
        else (el [width fill, height fill] (renderPostPage state.timestamp state.cache page))
         |> infiniteScroll "search-posts" (ChangeSearchPostPage (Page.next state.search.currentPage))
    Nothing   -> renderLoadingPosts

renderUserSearchState: AppState -> Element Msg
renderUserSearchState state = case SearchState.allUpToCurrentUserPage state.search of
    Just page -> if GenericPage.isEmpty page
        then renderNoResultPage
        else (el [width fill, height fill] (renderUserPage state.cache page))
         |> infiniteScroll "search-users" (ChangeSearchPostPage (Page.next state.search.currentPage))
    Nothing   -> renderLoadingUsers

renderPostSearchFilterTabs: SearchFilter -> Element Msg
renderPostSearchFilterTabs filter = case postTypeFilter filter of
    Nothing -> Element.none
    Just pt ->  allPostTypes
        |> List.map (\x -> renderPostTypeTab x (x==pt))
        |> row [alignLeft, spacing 5]

renderPostPage: UTCTimestamp -> Cache -> PostPage -> Element Msg
renderPostPage tmstp cache page = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 10]
    <| List.map (renderSinglePost tmstp cache) page.items

renderSinglePost: UTCTimestamp -> Cache -> PostId -> Element Msg
renderSinglePost = renderPostId

renderLoadingPosts: Element Msg
renderLoadingPosts = renderLoadingPostPage 2

renderUserPage: Cache -> UserPage -> Element Msg
renderUserPage cache page = column [
        width fill
        , height fill
        , centerX
        , spacing 5
        , padding 10]
    <| List.map (renderSingleUser cache) page.items

renderSingleUser: Cache -> UserId -> Element Msg
renderSingleUser = renderUserId

renderLoadingUsers: Element Msg
renderLoadingUsers = renderLoadingUserPage 2

renderSearchFilter: Cache -> SearchFilter -> Element Msg
renderSearchFilter cache filter = case filter of
    EmptySearch          -> empty
    ByHashtag [] _       -> empty
    ByUserPrefix _       -> empty
    ByHashtag hashtags _ -> hashtags
        |> List.map (\ht -> if Cache.containsFollowingHashtag cache ht then unfollowHashtagStyle ht else followHashtagStyle ht)
        |> row [spacing 10, padding 10]
        |> above (renderPostSearchFilterTabs filter)
    ByAuthor userId  _   -> let pseudo = userId |> Cache.getUserPseudo cache |> Maybe.withDefault (User.toString userId) in
        row [spacing 10] [el [Font.italic, Font.semiBold] (pseudo |> text)]

renderHashtagCloud: AppState -> Element Msg
renderHashtagCloud state =
    let whiteScheme = {default = foreground, colors = [foreground] }
        fontSize = computeFontFromWindowSize state.windowSize
    in state.cache.hashtagTrend
        |> Maybe.map (\trend -> el [centerX, centerY] (hashtagCloud whiteScheme fontSize trend))
        |> Maybe.map (el [width fill, height fill, centerX, centerY, Background.color background, Border.rounded 20])
        |> Maybe.map (el [width fill, height fill, centerX, centerY, padding 20])
        |> Maybe.withDefault Element.none

computeFontFromWindowSize: WindowSize -> Int
computeFontFromWindowSize {width, height} =
    let refWindowWidth = 200
        scaled = Element.modular (Int.toFloat 48) 1.25
    in scaled (width // refWindowWidth) |> round

renderNoResultPage: Element Msg
renderNoResultPage = View.ScreenUtils.emptyScreen "No results"

renderPostTypeTab: PostType -> Bool -> Element Msg
renderPostTypeTab pt selected =
    let renderTabButton: String -> Element Msg
        renderTabButton label = tabButton label (ChangeSearchPostTypeFilter pt) selected
    in case pt of
        AllPostTypes   -> renderTabButton "All"
        TipPosts       -> renderTabButton "Tips"
        ChallengePosts -> renderTabButton "Challenges"
        EventPosts     -> renderTabButton "Events"
        PollPosts      -> renderTabButton "Polls"
        FreeTextPosts  -> renderTabButton "Free posts"

above: Element Msg -> Element Msg -> Element Msg
above el1 el2 = column [width fill] [el1, el2]