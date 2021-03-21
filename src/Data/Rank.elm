module Data.Rank exposing (..)

type Score = Score Int

type alias ScoreBreakdown = {
    fromLikes: Int,
    fromFollows: Int,
    fromPosts: Int,
    fromEvents: Int
 }

type Rank = GreenWood
    | GaiaFriend
    | Converted
    | Influencer
    | Evangelist
    | Expert
    | BlackBelt
    | Sensei
    | Shihan
    | Hanshi
    | OSensei
    | Guru

emptyBreakdown: ScoreBreakdown
emptyBreakdown = {
    fromLikes   = 0,
    fromFollows = 0,
    fromPosts   = 0,
    fromEvents  = 0
 }

toString: Rank -> String
toString = Debug.toString

score: ScoreBreakdown -> Score
score breakdown = breakdown.fromLikes + breakdown.fromPosts + breakdown.fromFollows + breakdown.fromEvents |> Score


next: Rank -> Rank
next rank = case rank of
    GreenWood  -> GaiaFriend
    GaiaFriend -> Converted
    Converted  -> Influencer
    Influencer -> Evangelist
    Evangelist -> Expert
    Expert     -> BlackBelt
    BlackBelt  -> Sensei
    Sensei     -> Shihan
    Shihan     -> Hanshi
    Hanshi     -> OSensei
    OSensei    -> Guru
    Guru       -> Guru


-- This logic could just sit on the server
maxScore: Rank -> Int
maxScore rank = case rank of
    GreenWood  -> 10
    GaiaFriend -> 100
    Converted  -> 250
    Influencer -> 1000
    Evangelist -> 5000
    Expert     -> 10000
    BlackBelt  -> 25000
    Sensei     -> 50000
    Shihan     -> 100000
    Hanshi     -> 250000
    OSensei    -> 500000
    Guru       -> 1000000

fromScore: Score -> Rank
fromScore (Score amount) =
    if amount < maxScore GreenWood then GreenWood
    else if amount < maxScore GaiaFriend then GaiaFriend
    else if amount < maxScore Converted then Converted
    else if amount < maxScore Influencer then Influencer
    else if amount < maxScore Evangelist then Evangelist
    else if amount < maxScore Expert then Expert
    else if amount < maxScore BlackBelt then BlackBelt
    else if amount < maxScore Sensei then Sensei
    else if amount < maxScore Shihan then Shihan
    else if amount < maxScore Hanshi then Hanshi
    else if amount < maxScore OSensei then OSensei
    else Guru