module Query.Search exposing (performSearch)

import Data.Hashtag as Hashtag
import Data.Page as Page exposing (Page)
import Data.Post exposing (Post, PostId)
import Data.User as User exposing (UserId)
import Http
import Json.Decode as Decoder
import Query.CacheQueryUtils exposing (fetchAndCacheAllUsers, fetchFromIdAndCacheAll)
import Query.Json.DecoderUtils exposing (decodeUserId, jsonResolver)
import Query.Json.PostDecoder exposing (decodePostId)
import Query.QueryUtils exposing (authHeader, baseUrl)
import Query.TaskUtils exposing (thread)
import State.Cache exposing (Cache)
import State.SearchState as SearchState exposing (PostSearchResult, PostType(..), SearchFilter(..), SearchResult(..), UserSearchResult, postTypeFilter)
import State.UserState exposing (UserInfo)
import Task exposing (Task)
import Update.Msg exposing (Msg(..))
import Url.Builder exposing (absolute)


performSearch: Cache -> UserInfo -> SearchFilter -> Page -> Cmd Msg
performSearch cache user filter page =
    if SearchState.isPostSearchFilter filter
    then searchPostFromFilter user filter page
        |> Task.andThen (\searchResult -> fetchFromIdAndCacheAll cache user (postIds searchResult) |> thread searchResult)
        |> Task.attempt HttpPostSearchResultFetched
    else searchUserFromFilter user filter page
        |> Task.andThen (\searchResult -> fetchAndCacheAllUsers cache user (userIds searchResult) |> thread searchResult)
        |> Task.attempt HttpUserSearchResultFetched

searchPostFromFilter: UserInfo -> SearchFilter -> Page -> Task Http.Error PostSearchResult
searchPostFromFilter user filter page =
    case filter of
        EmptySearch    -> Task.succeed (SearchResult Page.first [])
        ByHashtag _ _  -> searchByHashtag user filter page
        ByAuthor _ _   -> searchByAuthor user filter page
        ByUserPrefix _ -> Task.succeed (SearchResult Page.first [])

searchUserFromFilter: UserInfo -> SearchFilter -> Page -> Task Http.Error UserSearchResult
searchUserFromFilter user filter page =
    case filter of
        ByUserPrefix prefix -> searchByUserPrefix user prefix page
        _                   -> Task.succeed (SearchResult Page.first [])

searchByHashtag: UserInfo -> SearchFilter -> Page -> Task Http.Error PostSearchResult
searchByHashtag user filter page = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute [
        "post",
        filter |> postTypeFromFilter,
        "by-hashtag",
        filter |> hashtagUrlString,
        page |> Page.number |> String.fromInt] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| Decoder.list decodePostId
    , timeout = Nothing
  } |> Task.map (\all -> SearchResult page all)

searchByAuthor: UserInfo -> SearchFilter -> Page -> Task Http.Error PostSearchResult
searchByAuthor user filter page = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute [
        "post",
        filter |> postTypeFromFilter,
        "by-author",
        filter |> userIdUrlString,
        page |> Page.number |> String.fromInt] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| Decoder.list decodePostId
    , timeout = Nothing
  } |> Task.map (\all -> SearchResult page all)

searchByUserPrefix: UserInfo -> String -> Page -> Task Http.Error UserSearchResult
searchByUserPrefix user filter page = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["user", "by-prefix", filter, page |> Page.number |> String.fromInt] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| Decoder.list decodeUserId
    , timeout = Nothing
  } |> Task.map (\all -> SearchResult page all)

-- Helpers
postIds: PostSearchResult -> List PostId
postIds (SearchResult _ ps) = ps

userIds: UserSearchResult -> List UserId
userIds (SearchResult _ users) = users

hashtagUrlString: SearchFilter -> String
hashtagUrlString filter = case filter of
    ByHashtag tags _ -> tags
        |> List.map Hashtag.toString
        |> List.intersperse "+"
        |> String.concat
    _ -> ""

userIdUrlString: SearchFilter -> String
userIdUrlString filter = case filter of
    ByAuthor userId _ -> userId
        |> User.toString
    _ -> ""

postTypeFromFilter: SearchFilter -> String
postTypeFromFilter filter = filter
    |> postTypeFilter
    |> Maybe.map (postTypeToString)
    |> Maybe.withDefault "error"

postTypeToString: PostType -> String
postTypeToString pt = case pt of
    AllPostTypes   -> "all"
    TipPosts       -> "tips"
    ChallengePosts -> "challenges"
    EventPosts     -> "events"
    PollPosts      -> "polls"
    FreeTextPosts  -> "free-texts"