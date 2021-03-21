module Utils.DateUtils exposing (..)

import Data.Schedule exposing (UTCTimestamp(..))
import DateTime
import Time

formatDate: UTCTimestamp -> String
formatDate (UTC tmstp) = let date = DateTime.fromPosix (Time.millisToPosix tmstp)
                             year  = date |> DateTime.getYear  |> String.fromInt
                             month = date |> DateTime.getMonth |> Debug.toString
                             day   = date |> DateTime.getDay   |> String.fromInt
                         in "" ++ day ++ " " ++ month ++ " " ++ year

formatRelativeTo: UTCTimestamp -> UTCTimestamp -> String
formatRelativeTo (UTC now) (UTC tmstp) =
    if Basics.abs(now - tmstp) <= 60000
    then "now"
    else if Basics.abs (now - tmstp) <= 60 * 60000
    then ((now - tmstp) // 60000 |> Basics.abs |> String.fromInt) ++ " min(s)"
    else if Basics.abs (now - tmstp) <= 24 * 60 * 60000
    then ((now - tmstp) // 3600000 |> Basics.abs |> String.fromInt) ++ " hour(s)"
    else if Basics.abs (now - tmstp) <= 7 * 24 * 60 * 60000
    then ((now - tmstp) // (24 * 3600000) |> Basics.abs |> String.fromInt) ++ " day(s)"
    else formatDate (UTC tmstp)