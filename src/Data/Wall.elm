module Data.Wall exposing (..)

import Data.Page as Page exposing (Page(..))
import Data.Post exposing (Post, PostId)
import Data.User exposing (UserId)

type Wall = Wall UserId Page (List PostId)

currentPage: Wall -> Page
currentPage (Wall _ page _) = page

nextPage: Wall -> Page
nextPage (Wall _ page _) = Page.next page

isEmpty: Wall -> Bool
isEmpty (Wall _ _ ps) = ps == []

postIds: Wall -> List PostId
postIds (Wall _ _ ps) = ps