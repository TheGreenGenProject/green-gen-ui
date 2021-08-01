module State.PageCache exposing (..)

import Data.Page as Page exposing (Page(..))
import Dict exposing (Dict)
import Utils.MaybeUtils as MaybeUtils


type alias PageCache a = {
    cache: Dict Int a,
    loading: Maybe Page,
    noMoreData: Bool
 }

empty: PageCache a
empty = {
    cache = Dict.empty,
    loading = Nothing,
    noMoreData = False
 }

add: a -> Page -> PageCache a -> PageCache a
add content page cache = {cache |
    cache = cache.cache |> Dict.insert (Page.number page) content
 }

get: Page -> PageCache a -> Maybe a
get page cache = Dict.get (Page.number page) cache.cache

contains: Page -> PageCache a -> Bool
contains page cache = get page cache
    |> MaybeUtils.nonEmpty

-- Flag loading if the page is not already in the cache
loading: Page -> PageCache a -> PageCache a
loading page cache =
    let pageCached = contains page cache in
    {cache | loading = if pageCached then Nothing else page |> Just }

noMoreData: Page -> PageCache a -> PageCache a
noMoreData _ cache = {cache | loading = Nothing, noMoreData = True }

isEmpty: PageCache a -> Bool
isEmpty cache = Dict.isEmpty cache.cache

nonEmpty: PageCache a -> Bool
nonEmpty  = isEmpty >> not


-- Higher-order functions

map: (a -> b) -> PageCache a -> PageCache b
map f cache = {
    cache = Dict.map (\k v -> f v) cache.cache,
    loading = cache.loading,
    noMoreData = cache.noMoreData
 }

filter: (a -> Bool) -> PageCache a -> PageCache a
filter p cache = {
    cache = Dict.filter (\k v -> p v) cache.cache,
    loading = cache.loading,
    noMoreData = cache.noMoreData
 }

pageContains: (a -> Bool) -> PageCache a -> Bool
pageContains p cache = Dict.values cache.cache
    |> List.any p
