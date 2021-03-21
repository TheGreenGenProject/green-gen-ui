module State.PostPage exposing (..)

import Data.Page exposing (Page)
import Data.Post exposing (Post, PostId)

type alias PostPage = {
    number: Page,
    posts: List PostId
 }

next: PostPage -> Int
next { number } = let (Data.Page.Page n) = number in n + 1

isLast: PostPage -> Bool
isLast { posts } = posts == []

isEmpty: PostPage -> Bool
isEmpty { posts } = posts == []


