module Data.Url exposing (..)

type Url = Url String

toString: Url -> String
toString (Url url) = url