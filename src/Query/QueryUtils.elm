module Query.QueryUtils exposing (..)

import Data.Post as Post exposing (Post, PostId)
import Data.Token as Token
import Http exposing (Error(..), Header, header)
import Query.Json.DecoderUtils exposing (jsonResolver)
import Query.Json.PostDecoder exposing (decodePost)
import State.UserState exposing (UserInfo)
import Task exposing (Task)
import Url.Builder exposing (absolute)


baseUrl = "http://localhost:8080"
--baseUrl = "http://ancient-journey-03206.herokuapp.com"

authHeader: UserInfo -> Header
authHeader userInfo = header "Authorization" (Token.toString userInfo.token)

fetchAllPosts: UserInfo -> List PostId -> Task Http.Error (List Post)
fetchAllPosts user ids = ids
    |> List.map (fetchPost user)
    |> Task.sequence

fetchPost: UserInfo -> PostId -> Task Http.Error Post
fetchPost user id = Http.task {
        method = "GET"
        , headers = [authHeader user]
        , url = baseUrl ++ absolute ["post", "by-id", id |> Post.toString] []
        , body = Http.emptyBody
        , resolver = jsonResolver <| decodePost
        , timeout = Nothing
 }

errorToString : Http.Error -> String
errorToString error = case error of
    BadUrl url    -> "URL '" ++ url ++ "' was invalid"
    Timeout       -> "Timeout"
    NetworkError  -> "Unable to reach the server"
    BadStatus 500 -> "Internal error"
    BadStatus _   -> "Unknown error"
    BadBody err   -> err