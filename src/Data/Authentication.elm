module Data.Authentication exposing (..)

import Data.Schedule exposing (UTCTimestamp)
import Data.Token exposing (Token)
import Data.User exposing (UserId)

type Authentication = NotAuthenticated | Authenticated Token UserId UTCTimestamp