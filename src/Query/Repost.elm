module Query.Repost exposing (repost)

import Data.Post as PostId exposing (PostId)
import Http exposing (Error(..))
import Query.Json.DecoderUtils exposing (jsonResolver, unitDecoder)
import Query.QueryUtils exposing (authHeader, baseUrl)
import State.FormState exposing (NewRepostWizardState)
import State.UserState exposing (UserInfo)
import Task exposing (Task)
import Update.Msg exposing (Msg(..))
import Url.Builder exposing (absolute, string)


repost: UserInfo -> NewRepostWizardState -> Cmd Msg
repost user repostState =
    repostState.repost
    |> Maybe.map (\x -> createNewRepost user x)
    |> Maybe.withDefault (Task.fail <| BadUrl "No post-id provided for a repost")
    |> Task.attempt HttpNewRepostPosted

createNewRepost: UserInfo -> PostId -> Task Http.Error ()
createNewRepost user postId = Http.task {
        method     = "POST"
        , headers  = [authHeader user]
        , url      = baseUrl ++ absolute ["post", "repost"] [
                string "post-id" (postId |> PostId.toString)
            ]
        , body     = Http.emptyBody
        , resolver = jsonResolver <| unitDecoder
        , timeout  = Nothing
    }
