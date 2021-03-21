module Data.Token exposing (..)

import Uuid exposing (Uuid)


type Token = Token Uuid

fromString: String -> Maybe Token
fromString = Maybe.map Token << Uuid.fromString

toString: Token -> String
toString (Token uuid) = Uuid.toString uuid