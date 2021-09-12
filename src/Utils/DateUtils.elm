module Utils.DateUtils exposing (..)

import Data.Schedule exposing (UTCTimestamp(..))
import DateTime
import Time exposing (Month(..))


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

max: UTCTimestamp -> UTCTimestamp -> UTCTimestamp
max (UTC t1) (UTC t2) = if t1 > t2 then UTC t1 else UTC t2

min: UTCTimestamp -> UTCTimestamp -> UTCTimestamp
min (UTC t1) (UTC t2) = if t1 <= t2 then UTC t1 else UTC t2


plusDays: Int -> UTCTimestamp -> UTCTimestamp
plusDays days (UTC millis) = UTC (millis + days * 24 * 60 * 60 * 1000)

plusHours: Int -> UTCTimestamp -> UTCTimestamp
plusHours hours (UTC millis) = UTC (millis + hours * 60 * 60 * 1000)

plusMinutes: Int -> UTCTimestamp -> UTCTimestamp
plusMinutes mins (UTC millis) = UTC (millis + mins * 60 * 1000)

-- UTC timestamp / Local Date conversion
type alias LocalDate = {
    day: Int,
    month: Int,
    year: Int
 }

-- Local date - for UTC time zone ...
toLocalDate: UTCTimestamp -> LocalDate
toLocalDate (UTC millis) =
    let posix = Time.millisToPosix millis
        day = Time.toDay Time.utc posix
        month = Time.toMonth Time.utc posix
        year = Time.toYear Time.utc posix
    in {day = day, month = monthToInt month, year = year }

monthToInt: Month -> Int
monthToInt m = case m of
    Jan -> 1
    Feb -> 2
    Mar -> 3
    Apr -> 4
    May -> 5
    Jun -> 6
    Jul -> 7
    Aug -> 8
    Sep -> 9
    Oct -> 10
    Nov -> 11
    Dec -> 12

intToMonth: Int -> Maybe Month
intToMonth month = case month of
    1 -> Jan |> Just
    2 -> Feb |> Just
    3 -> Mar |> Just
    4 -> Apr |> Just
    5 -> May |> Just
    6 -> Jun |> Just
    7 -> Jul |> Just
    8 -> Aug |> Just
    9 -> Sep |> Just
    10 -> Oct |> Just
    11 -> Nov |> Just
    12 -> Dec |> Just
    _  -> Nothing

toUTCTimestamp: LocalDate -> Maybe UTCTimestamp
toUTCTimestamp {day, month, year} = intToMonth month
    |> Maybe.andThen (\m -> DateTime.fromRawParts
        {day = day, month = m, year = year}
        {hours = 0, minutes = 0, seconds = 0, milliseconds = 0})
    |> Maybe.map DateTime.toPosix
    |> Maybe.map Time.posixToMillis
    |> Maybe.map UTC

-- UTC timestamp / Local Time conversion
type alias LocalTime = {
    hour: Int,
    minute: Int
 }

toLocalTime: UTCTimestamp -> LocalTime
toLocalTime (UTC millis) =
    let posix = Time.millisToPosix millis
        hours = Time.toHour Time.utc posix
        minutes = Time.toMinute Time.utc posix
    in { hour = hours, minute = minutes }

toUTCTimestampFromTime: LocalDate -> LocalTime -> Maybe UTCTimestamp
toUTCTimestampFromTime {day, month, year} {hour, minute} = intToMonth month
    |> Maybe.andThen (\m -> DateTime.fromRawParts
        { day = day, month = m, year = year}
        { hours = hour, minutes = minute, seconds = 0, milliseconds = 0})
    |> Maybe.map DateTime.toPosix
    |> Maybe.map Time.posixToMillis
    |> Maybe.map UTC
