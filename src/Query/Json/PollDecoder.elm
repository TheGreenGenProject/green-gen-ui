module Query.Json.PollDecoder exposing (..)


import Data.Poll exposing (Poll, PollId(..), PollOption(..), PollStats(..), PollStatsEntry)
import Json.Decode exposing (Decoder, int, list, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Query.Json.DecoderUtils exposing (decodeTimestamp, decodeUserId, decodeUuid)


decodePoll: Decoder Poll
decodePoll = succeed Poll
    |> required "id" decodePollId
    |> required "author" decodeUserId
    |> required "question" string
    |> required "options" decodePollOptionList
    |> required "timestamp" decodeTimestamp

decodePollOptionList: Decoder (List PollOption)
decodePollOptionList = list decodePollOption

decodePollOption: Decoder PollOption
decodePollOption = succeed PollOption
    |> required "value" string

decodePollStats: Decoder PollStats
decodePollStats = succeed PollStats
    |> required "stats" (list decodePollStatsEntry)

decodePollStatsEntry: Decoder PollStatsEntry
decodePollStatsEntry = succeed PollStatsEntry
    |> required "option" decodePollOption
    |> required "count" int

decodePollId: Decoder PollId
decodePollId = succeed PollId
    |> required "value" decodeUuid