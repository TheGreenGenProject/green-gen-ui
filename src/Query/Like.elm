module Query.Like exposing (like, unlike)

import Data.Post as Post exposing (PostId)
import Http
import Query.Json.DecoderUtils exposing (jsonResolver, unitDecoder)
import Query.QueryUtils exposing (authHeader, baseUrl)
import State.Cache exposing (Cache)
import State.UserState exposing (UserInfo)
import Task
import Update.Msg exposing (Msg(..))
import Url.Builder exposing (absolute, string)


like: Cache -> UserInfo -> PostId -> Cmd Msg
like cache user postId = Debug.log ("Liking post " ++ (Post.toString postId))
    Http.task {
        method     = "POST"
        , headers  = [authHeader user]
        , url      = baseUrl ++ absolute ["like", "add"] [string "post-id" (Post.toString postId)]
        , body     = Http.emptyBody
        , resolver = jsonResolver <| unitDecoder
        , timeout  = Nothing
     } |> Task.attempt HttpPostLiked

unlike: Cache -> UserInfo -> PostId -> Cmd Msg
unlike cache user postId = Debug.log ("Unliking post " ++ (Post.toString postId))
    Http.task {
        method     = "POST"
        , headers  = [authHeader user]
        , url      = baseUrl ++ absolute ["like", "remove"] [string "post-id" (Post.toString postId)]
        , body     = Http.emptyBody
        , resolver = jsonResolver <| unitDecoder
        , timeout  = Nothing
     } |> Task.attempt HttpPostUnliked