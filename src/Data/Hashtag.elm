module Data.Hashtag exposing (..)

type Hashtag = Hashtag String

toString: Hashtag -> String
toString (Hashtag ht) = ht

format: Hashtag -> String
format (Hashtag ht) = "#" ++ ht

