module State.PostPageCache exposing (..)

import Data.Page as Page exposing (Page(..))
import Data.Post exposing (PostId)
import Dict exposing (Dict)
import State.GenericPage exposing (GenericPage)
import State.PageCache as PageCache exposing (PageCache)
import Utils.ListUtils as ListUtils


type alias PostPage = GenericPage PostId
type alias PostPageCache = PageCache PostPage

add: PostPage -> PostPageCache -> PostPageCache
add page cache = PageCache.add page page.number cache

getAllUpTo: Page -> PostPageCache -> Maybe PostPage
getAllUpTo page cache =
    let sortedKeys = cache.cache
            |> Dict.keys |> List.sort
            |> ListUtils.takeWhile (\key -> key <= (Page.number page))
        allPosts = sortedKeys
            |> List.concatMap (\p -> (PageCache.get (Page p) cache)
            |> Maybe.map (.items)
            |> Maybe.withDefault [])
    in if List.isEmpty sortedKeys
        then Nothing
        else { number = page, items = allPosts } |> Just
