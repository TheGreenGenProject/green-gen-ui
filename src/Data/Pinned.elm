module Data.Pinned exposing (..)

import Data.Page as Page exposing (Page)
import Data.Post exposing (PinnedPost(..), Post, PostId)

type Pinned = Pinned Page (List PinnedPost)

currentPage: Pinned -> Page
currentPage (Pinned page _) = page

nextPage: Pinned -> Page
nextPage (Pinned page _) = Page.next page

isEmpty: Pinned -> Bool
isEmpty (Pinned _ ps) = ps == []

postIds: Pinned -> List PostId
postIds (Pinned _ ps) = ps |> List.map (\(PinnedPost id _) -> id)