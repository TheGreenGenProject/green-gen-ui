module Query.TaskUtils exposing (..)

import Process
import Task exposing (Task)

-- Thread a result from a task
thread: a -> Task err b -> Task err (b, a)
thread a task = Task.map (\b -> (b, a)) task

-- Delay the sending of the given message
delay : Float -> msg -> Cmd msg
delay time msg =
  Process.sleep time
  |> Task.andThen (always <| Task.succeed msg)
  |> Task.perform identity