module Data.Feed exposing (..)

import Data.Page as Page exposing (Page)
import Data.Post exposing (Post, PostId)

type Feed = Feed Page (List PostId)

currentPage: Feed -> Page
currentPage (Feed page _) = page

nextPage: Feed -> Page
nextPage (Feed page _) = Page.next page

isEmpty: Feed -> Bool
isEmpty (Feed _ ps) = ps == []

postIds: Feed -> List PostId
postIds (Feed _ ps) = ps
