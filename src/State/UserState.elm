module State.UserState exposing (..)

import Data.Schedule exposing (UTCTimestamp)
import Data.Token exposing (Token)
import Data.User exposing (UserId)

type UserState = NotLogged
    | Blocked UTCTimestamp
    | LoggingIn Credentials
    | LoggedIn UserInfo

type alias Credentials = {
    email: String,
    password: String
 }

type alias UserInfo = {
    id: UserId,
    pseudo: String,
    introduction: String,
    enabled: Bool,
    since: UTCTimestamp,
    token: Token
 }

isUserLoggedIn: UserState -> Bool
isUserLoggedIn state = case state of
    LoggedIn _ -> True
    _          -> False

isUserLoggedOff: UserState -> Bool
isUserLoggedOff = not << isUserLoggedIn

pseudo: UserState -> String
pseudo state = case state of
    LoggedIn user -> user.pseudo
    _             -> "Not logged in !"

