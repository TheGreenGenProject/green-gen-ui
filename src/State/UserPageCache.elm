module State.UserPageCache exposing (..)

import Data.Page as Page exposing (Page(..))
import Data.User exposing (UserId)
import Dict exposing (Dict)
import State.GenericPage exposing (GenericPage)
import State.PageCache as PageCache exposing (PageCache)
import Utils.ListUtils as ListUtils


type alias UserPage = GenericPage UserId
type alias UserPageCache = PageCache UserPage

add: UserPage -> UserPageCache -> UserPageCache
add page cache = PageCache.add page page.number cache

getAllUpTo: Page -> UserPageCache -> Maybe UserPage
getAllUpTo page cache =
    let sortedKeys = cache.cache
            |> Dict.keys |> List.sort
            |> ListUtils.takeWhile (\key -> key <= (Page.number page))
        all = sortedKeys
            |> List.concatMap (\p -> (PageCache.get (Page p) cache)
            |> Maybe.map (.items)
            |> Maybe.withDefault [])
    in if List.isEmpty sortedKeys
        then Nothing
        else { number = page, items = all } |> Just