module Query.Poll exposing (..)

import Data.Poll as Poll exposing (PollId, PollOption, optionIndex)
import Http
import Json.Decode exposing (bool)
import Query.Json.DecoderUtils exposing (jsonResolver, unitDecoder)
import Query.QueryUtils exposing (authHeader, baseUrl)
import State.Cache as Cache exposing (Cache)
import State.UserState exposing (UserInfo)
import Task exposing (Task)
import Update.Msg exposing (Msg(..))
import Url.Builder exposing (absolute)


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