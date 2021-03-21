module Query.Search exposing (performSearch)

import Data.Hashtag as Hashtag
import Data.Page as Page exposing (Page)
import Data.Post exposing (Post, PostId)
import Data.User as User
import Http
import Json.Decode as Decoder
import Query.CacheQueryUtils exposing (fetchFromIdAndCacheAll)
import Query.Json.DecoderUtils exposing (jsonResolver)
import Query.Json.PostDecoder exposing (decodePostId)
import Query.QueryUtils exposing (authHeader, baseUrl)
import Query.TaskUtils exposing (thread)
import State.Cache exposing (Cache)
import State.SearchState exposing (SearchFilter(..), SearchResult(..))
import State.UserState exposing (UserInfo)
import Task exposing (Task)
import Update.Msg exposing (Msg(..))
import Url.Builder exposing (absolute)


performSearch: Cache -> UserInfo -> SearchFilter -> Page -> Cmd Msg
performSearch cache user filter page =  searchFromFilter user filter page
    |> Task.andThen (\searchResult -> fetchFromIdAndCacheAll cache user (postIds searchResult) |> thread searchResult)
    |> Task.attempt HttpSearchResultFetched

searchFromFilter: UserInfo -> SearchFilter -> Page -> Task Http.Error SearchResult
searchFromFilter user filter page =
    case filter of
        EmptySearch -> Task.succeed (SearchResult Page.first [])
        ByHashtag _ -> searchByHashtag user filter page
        ByAuthor _  -> searchByAuthor user filter page

searchByHashtag: UserInfo -> SearchFilter -> Page -> Task Http.Error SearchResult
searchByHashtag user filter page = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["post", "by-hashtag", filter |> hashtagUrlString, page |> Page.number |> String.fromInt] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| Decoder.list decodePostId
    , timeout = Nothing
  } |> Task.map (\all -> SearchResult page all)

searchByAuthor: UserInfo -> SearchFilter -> Page -> Task Http.Error SearchResult
searchByAuthor user filter page = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["post", "by-author", filter |> userIdUrlString, page |> Page.number |> String.fromInt] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| Decoder.list decodePostId
    , timeout = Nothing
  } |> Task.map (\all -> SearchResult page all)

-- Helpers
postIds: SearchResult -> List PostId
postIds (SearchResult _ ps) = ps

hashtagUrlString: SearchFilter -> String
hashtagUrlString filter = case filter of
    ByHashtag tags -> tags
        |> List.map Hashtag.toString
        |> List.intersperse "+"
        |> String.concat
    _ -> ""

userIdUrlString: SearchFilter -> String
userIdUrlString filter = case filter of
    ByAuthor userId -> userId
        |> User.toString
    _ -> ""
