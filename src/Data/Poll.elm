module Data.Poll exposing (..)

import Uuid exposing (Uuid)
import Data.User exposing (UserId)
import Data.Schedule exposing (UTCTimestamp)

type PollId = PollId Uuid
type PollOption = PollOption String
type alias Poll = {
    id: PollId,
    author: UserId,
    options: List PollOption,
    created: UTCTimestamp
  }

fromUuid: Uuid -> PollId
fromUuid uuid = PollId uuid