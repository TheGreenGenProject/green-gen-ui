module Data.Page exposing (..)

type Page = Page Int

number: Page -> Int
number (Page n) = n

next: Page -> Page
next (Page n) = Page (n+1)

previous: Page -> Maybe Page
previous (Page n) = if n > 1
    then Page (n - 1) |> Just
    else Nothing

first: Page
first = Page 1

isFirst: Page -> Bool
isFirst (Page n) = n==1