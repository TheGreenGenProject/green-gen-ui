module Query.FreeText exposing (postFreeText)

import Data.Hashtag exposing (Hashtag(..))
import Http
import Query.Json.DecoderUtils exposing (jsonResolver, unitDecoder)
import Query.QueryUtils exposing (authHeader, baseUrl)
import State.FormState exposing (NewFreeTextWizardState, NewTipWizardState)
import State.UserState exposing (UserInfo)
import Task exposing (Task)
import Update.Msg exposing (Msg(..))
import Url.Builder exposing (absolute, string)
import Utils.TextUtils as TextUtils


postFreeText: UserInfo -> NewFreeTextWizardState -> Cmd Msg
postFreeText user newFreeText =
    createNewFreeTextPost user newFreeText
    |> Task.attempt HttpNewFreeTextPosted

createNewFreeTextPost: UserInfo -> NewFreeTextWizardState -> Task Http.Error ()
createNewFreeTextPost user newFreeText = Http.task {
        method     = "POST"
        , headers  = [authHeader user]
        , url      = baseUrl ++ absolute ["post", "new", "free-text"] [
            string "hashtags" (newFreeText |> hashtagsAsParameter)
            , string "content" (newFreeText.content |> Maybe.withDefault "")
            , string "sources" "myself"]
        , body     = Http.emptyBody
        , resolver = jsonResolver <| unitDecoder
        , timeout  = Nothing
    }

hashtagsAsParameter: NewFreeTextWizardState -> String
hashtagsAsParameter state =
    let content = state.content |> Maybe.withDefault ""
        hashtags = TextUtils.hashtagsFrom content
    in hashtags
        |> List.map (\(Hashtag x) -> x)
        |> String.join "+"