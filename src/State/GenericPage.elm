module State.GenericPage exposing (..)

import Data.Page exposing (Page)


type alias GenericPage a = {
    number: Page,
    items: List a
 }

next: GenericPage a -> Int
next { number } = let (Data.Page.Page n) = number in n + 1

isLast: GenericPage a -> Bool
isLast { items } = items == []

isEmpty: GenericPage a -> Bool
isEmpty { items } = items == []
