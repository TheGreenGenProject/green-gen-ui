module Test.TestState exposing (..)

import Data.Schedule exposing (UTCTimestamp(..))
import Data.Token exposing (Token(..))
import Data.User exposing (UserId(..))
import Uuid


uuid = "63B9AAA2-6AAF-473E-B37E-22EB66E66B76"
maybeToken = Uuid.fromString uuid |> Maybe.map Token
maybeUserId = Uuid.fromString uuid |> Maybe.map UserId

maybeUserInfo = case (maybeUserId, maybeToken) of
    (Just userId, Just tokenId) -> Just {
        id = userId,
        pseudo = "chrisgg",
        enabled = True,
        lastLogin = UTC 0,
        token = tokenId }
    _ -> Nothing