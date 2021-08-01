module State.PostPageCache exposing (..)

import Data.Page as Page exposing (Page(..))
import Dict exposing (Dict)
import State.PageCache as PageCache exposing (PageCache)
import State.PostPage exposing (PostPage)
import Utils.ListUtils as ListUtils


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
            |> Maybe.map (.posts)
            |> Maybe.withDefault [])
    in if List.isEmpty allPosts
        then Nothing
        else { number = page, posts = allPosts } |> Just
