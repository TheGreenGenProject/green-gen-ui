module Data.Poll exposing (..)

import Uuid exposing (Uuid)
import Data.User exposing (UserId)
import Data.Schedule exposing (UTCTimestamp)

type PollId = PollId Uuid
type PollOption = PollOption String
type alias Poll = {
    id: PollId,
    author: UserId,
    title: String,
    options: List PollOption,
    created: UTCTimestamp
  }

type alias PollStatsEntry = {
    option: PollOption,
    count: Int
 }
type PollStats = PollStats (List PollStatsEntry)

emptyPollStats: Poll -> PollStats
emptyPollStats poll = poll.options
    |> List.map(\x -> {option=x, count=0})
    |> PollStats

normalizePollStats: Poll -> PollStats -> PollStats
normalizePollStats poll (PollStats stats) =
    let (PollStats empty) = emptyPollStats poll
        containOption = (\xs opt -> xs |> List.filter (\x -> x.option==opt) |> not << List.isEmpty)
        notProvided = empty |> List.filter (\entry -> containOption stats entry.option |> not) |> Debug.log "Not provided: "
    in stats ++ notProvided |> PollStats

optionIndex: Poll -> PollOption -> Maybe Int
optionIndex poll option = poll.options
    |> List.indexedMap (\ index opt -> (index, opt))
    |> List.filter (\(_, opt) -> opt==option)
    |> List.head |> Maybe.map (\(index, _) -> index + 1)

updatePollStats: PollOption -> PollStats -> PollStats
updatePollStats opt (PollStats entries) =
    let (entry, others) = entries |> List.partition (\x -> x.option == opt)
        {option, count} = entry |> List.head |> Maybe.withDefault { option=opt, count=0}
    in PollStats ({option=option, count = count + 1 } :: others)

respondents: PollStats -> Int
respondents (PollStats stats) = stats |> List.foldl (\ x y -> x.count + y) 0

fromUuid: Uuid -> PollId
fromUuid uuid = PollId uuid

toString: PollId -> String
toString (PollId uuid) = uuid |> Uuid.toString

fromString: String -> Maybe PollId
fromString = Maybe.map PollId << Uuid.fromString