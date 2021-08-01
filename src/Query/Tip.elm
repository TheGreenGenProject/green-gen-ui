module Query.Tip exposing (postTip)


import Data.Hashtag exposing (Hashtag(..))
import Data.Tip as Tip exposing (TipId)
import Http
import Query.Json.DecoderUtils exposing (jsonResolver, unitDecoder)
import Query.Json.TipDecoder exposing (decodeTipId)
import Query.QueryUtils exposing (authHeader, baseUrl)
import State.FormState exposing (NewTipWizardState)
import State.UserState exposing (UserInfo)
import Task exposing (Task)
import Update.Msg exposing (Msg(..))
import Url.Builder exposing (absolute, string)
import Utils.TextUtils as TextUtils


postTip: UserInfo -> NewTipWizardState -> Cmd Msg
postTip user newTip =
    createNewTip user newTip
    |> Task.andThen (\tipId -> createNewTipPost user newTip tipId)
    |> Task.attempt HttpNewTipPosted

createNewTip: UserInfo -> NewTipWizardState -> Task Http.Error TipId
createNewTip user newTip = Http.task {
        method     = "POST"
        , headers  = [authHeader user]
        , url      = baseUrl ++ absolute ["tip", "new"] [
                string "content" (newTip.content |> Maybe.withDefault "")
                , string "sources" "myself"
            ]
        , body     = Http.emptyBody
        , resolver = jsonResolver <| decodeTipId
        , timeout  = Nothing
     }

createNewTipPost: UserInfo -> NewTipWizardState -> TipId -> Task Http.Error ()
createNewTipPost user newTip tipId = Http.task {
        method     = "POST"
        , headers  = [authHeader user]
        , url      = baseUrl ++ absolute ["post", "new", "tip"] [
                string "tip-id" (tipId |> Tip.toString)
                , string "hashtags" (newTip |> hashtagsAsParameter)
            ]
        , body     = Http.emptyBody
        , resolver = jsonResolver <| unitDecoder
        , timeout  = Nothing
    }

hashtagsAsParameter: NewTipWizardState -> String
hashtagsAsParameter state =
    let content = state.content |> Maybe.withDefault ""
        hashtags = TextUtils.hashtagsFrom content
    in hashtags
        |> List.map (\(Hashtag x) -> x)
        |> String.join "+"

