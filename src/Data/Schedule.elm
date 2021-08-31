module Data.Schedule exposing (..)

import Time exposing(Weekday(..), Month(..))
import Utils.ListUtils as ListUtils
type UTCTimestamp = UTC Int
type Duration = Duration Int

type Schedule = OneOff UTCTimestamp UTCTimestamp
    | Recurring UTCTimestamp Duration Duration UTCTimestamp

start: Schedule -> UTCTimestamp
start schedule = case schedule of
    (OneOff x _) -> x
    (Recurring x _ _ _) -> x

duration: Schedule -> Duration
duration schedule = case schedule of
        (OneOff (UTC x) (UTC y)) -> Duration (y - x)
        (Recurring _ d _ _) -> d

end: Schedule -> UTCTimestamp
end schedule = case schedule of
    (OneOff _ x) -> x
    (Recurring _ _ _ x) -> x

formatDuration: Duration -> String
formatDuration (Duration millis) =
    if millis == 24 * 60 * 60 * 1000 then "Daily"
    else if millis == 7 * 24 * 60 * 60 * 1000 then "Weekly"
    else "<Unknown>"

-- All the dates within the Schedule
-- Includes the start date, but might not include the end date in case of a recurring schedule
dates: Schedule -> List UTCTimestamp
dates schedule = case schedule of
    OneOff s e             -> [s, e]
    Recurring s _ period e -> ListUtils.fix s (\x -> plus x period) (\x -> after x e)
        |> List.reverse

beforeSchedule: UTCTimestamp -> Schedule -> Bool
beforeSchedule (UTC a) schedule = let (UTC b) = end schedule in a < b

afterSchedule: UTCTimestamp -> Schedule -> Bool
afterSchedule (UTC a) schedule = let (UTC b) = end schedule in a > b

before: UTCTimestamp -> UTCTimestamp -> Bool
before (UTC a) (UTC b) = a < b

after: UTCTimestamp -> UTCTimestamp -> Bool
after (UTC a) (UTC b) = a > b

plus: UTCTimestamp -> Duration -> UTCTimestamp
plus (UTC time) (Duration millis) = UTC (time + millis)

minus: UTCTimestamp -> Duration -> UTCTimestamp
minus (UTC time) (Duration millis) = UTC (time - millis)

milliseconds: UTCTimestamp -> Int
milliseconds (UTC ms) = ms


{-- Helper to display a date --}
weekDay: UTCTimestamp -> String
weekDay (UTC ms) = case (Time.toWeekday Time.utc (Time.millisToPosix ms)) of
    Mon -> "Monday"
    Tue -> "Tuesday"
    Wed -> "Wednesday"
    Thu -> "Thursday"
    Fri -> "Friday"
    Sat -> "Saturday"
    Sun -> "Sunday"

day: UTCTimestamp -> Int
day (UTC ms) = Time.toDay Time.utc (Time.millisToPosix ms)

month: UTCTimestamp -> String
month (UTC ms) = case (Time.toMonth Time.utc (Time.millisToPosix ms)) of
    Jan -> "January"
    Feb -> "February"
    Mar -> "March"
    Apr -> "April"
    May -> "May"
    Jun -> "June"
    Jul -> "July"
    Aug -> "August"
    Sep -> "September"
    Oct -> "October"
    Nov -> "November"
    Dec -> "December"

year: UTCTimestamp -> Int
year (UTC ms) = Time.toYear Time.utc (Time.millisToPosix ms)
