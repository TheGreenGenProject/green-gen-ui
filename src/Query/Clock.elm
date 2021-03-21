module Query.Clock exposing (..)

import Data.Schedule exposing (UTCTimestamp(..))
import Query.TaskUtils exposing (delay)
import Task exposing (Task)
import Time
import Update.Msg exposing (Msg(..))


currentTime: Cmd Msg
currentTime = Time.now
    |> Task.map Time.posixToMillis
    |> Task.map UTC
    |> Task.attempt (\ x -> case x of
        Ok tmstp -> SetCurrentTime tmstp
        Err _    -> NoOp
    )

-- A clock tic every second
scheduleClockTick: Cmd Msg
scheduleClockTick = delay 1000.0 ClockTick