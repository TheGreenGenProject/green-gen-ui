module State.PostPageCache exposing (..)

import Data.Page as Page exposing (Page(..))
import Dict exposing (Dict)
import State.PostPage exposing (PostPage)
import Utils.ListUtils as ListUtils
import Utils.MaybeUtils as MaybeUtils


type alias PostPageCache = {
    cache: Dict Int PostPage,
    loading: Maybe Page,
    noMoreData: Bool
 }

empty: PostPageCache
empty = {
    cache = Dict.empty,
    loading = Nothing,
    noMoreData = False
 }

add: PostPage -> PostPageCache -> PostPageCache
add page cache = {cache | cache = cache.cache |> Dict.insert (Page.number page.number) page  }

get: Page -> PostPageCache -> Maybe PostPage
get page cache = Dict.get (Page.number page) cache.cache

getAllUpTo: Page -> PostPageCache -> Maybe PostPage
getAllUpTo page cache =
    let sortedKeys = cache.cache
            |> Dict.keys |> List.sort
            |> ListUtils.takeWhile (\key -> key <= (Page.number page))
        allPosts = sortedKeys
            |> List.concatMap (\p -> (get (Page p) cache)
            |> Maybe.map (.posts)
            |> Maybe.withDefault [])
    in if List.isEmpty allPosts
        then Nothing
        else { number = page, posts = allPosts } |> Just

contains: Page -> PostPageCache -> Bool
contains page cache = get page cache
    |> MaybeUtils.nonEmpty

-- Flag loading if the page is not already in the cache
loading: Page -> PostPageCache -> PostPageCache
loading page cache =
    let pageCached = contains page cache in
    {cache | loading = if pageCached then Nothing else page |> Just }

noMoreData: Page -> PostPageCache -> PostPageCache
noMoreData _ cache = {cache | loading = Nothing, noMoreData = True }

isEmpty: PostPageCache -> Bool
isEmpty cache = Dict.isEmpty cache.cache

nonEmpty: PostPageCache -> Bool
nonEmpty  = isEmpty >> not



