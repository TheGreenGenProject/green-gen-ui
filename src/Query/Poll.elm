module Query.Poll exposing (
    postPoll
    , answerPollOption
    , isClosed
    , answerPoll
 )

import Data.Hashtag exposing (Hashtag(..))
import Data.Poll as Poll exposing (PollId, PollOption(..), optionIndex)
import Http
import Json.Decode exposing (bool)
import Query.Json.DecoderUtils exposing (jsonResolver, unitDecoder)
import Query.Json.PollDecoder exposing (decodePollId)
import Query.QueryUtils exposing (authHeader, baseUrl)
import State.Cache as Cache exposing (Cache)
import State.FormState exposing (NewPollWizardState)
import State.UserState exposing (UserInfo)
import Task exposing (Task)
import Update.Msg exposing (Msg(..))
import Url.Builder exposing (absolute, string)
import Utils.TextUtils as TextUtils


postPoll: UserInfo -> NewPollWizardState -> Cmd Msg
postPoll user newPoll =
    createNewPoll user newPoll
    |> Task.andThen (createNewPollPost user newPoll)
    |> Task.attempt HttpNewPollPosted

createNewPoll: UserInfo -> NewPollWizardState -> Task Http.Error PollId
createNewPoll user newPoll = Http.task {
        method     = "POST"
        , headers  = [authHeader user]
        , url      = baseUrl ++ absolute ["poll", "new"] [
            string "question" (newPoll.question |> Maybe.withDefault "")
            , string "options" (newPoll |> pollOptionAsParameter)
        ]
        , body     = Http.emptyBody
        , resolver = jsonResolver <| decodePollId
        , timeout  = Nothing
  }

createNewPollPost: UserInfo -> NewPollWizardState -> PollId -> Task Http.Error ()
createNewPollPost user state challengeId = Http.task {
    method     = "POST"
    , headers  = [authHeader user]
    , url      = baseUrl ++ absolute ["post", "new", "poll"] [
        string "poll-id" (challengeId |> Poll.toString)
        , string "hashtags" (state |> hashtagsAsParameter)]
    , body     = Http.emptyBody
    , resolver = jsonResolver <| unitDecoder
    , timeout  = Nothing
  }

answerPollOption: Cache -> UserInfo -> PollId -> PollOption -> Cmd Msg
answerPollOption cache user pollId option =
    let maybeIndex = Cache.getPoll cache pollId |> Maybe.andThen (\poll -> Poll.optionIndex poll option)
    in case maybeIndex of
        Just index -> answerPoll user pollId index |> Task.attempt HttpPollAnswered
        Nothing    -> Cmd.none
            |> Debug.log ("Couldn't retrieve Option index for Poll with id" ++ (Poll.toString pollId)) -- FIXME failure here

isClosed: UserInfo -> PollId -> Task Http.Error Bool
isClosed user pollId = Http.task {
    method     = "GET"
    , headers  = [authHeader user]
    , url      = baseUrl ++ absolute ["poll", "is-closed", (Poll.toString pollId)] []
    , body     = Http.emptyBody
    , resolver = jsonResolver <| bool
    , timeout  = Nothing
  }

answerPoll: UserInfo -> PollId -> Int -> Task Http.Error ()
answerPoll user pollId optionIndex = Http.task {
    method     = "POST"
    , headers  = [authHeader user]
    , url      = baseUrl ++ absolute ["poll", "answer", (Poll.toString pollId), String.fromInt optionIndex] []
    , body     = Http.emptyBody
    , resolver = jsonResolver <| unitDecoder
    , timeout  = Nothing
  }

hashtagsAsParameter: NewPollWizardState -> String
hashtagsAsParameter state =
    let content = state.question |> Maybe.withDefault ""
        hashtags = TextUtils.hashtagsFrom content
    in hashtags
        |> List.map (\(Hashtag x) -> x)
        |> String.join "+"

pollOptionAsParameter: NewPollWizardState -> String
pollOptionAsParameter state =
    (state.options |> Maybe.withDefault [])
    |> List.map (\(PollOption opt) -> opt)
    |> String.join "+"
