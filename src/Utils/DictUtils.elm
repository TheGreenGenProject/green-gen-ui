module Utils.DictUtils exposing (..)

import Dict exposing (Dict)
import Utils.ListUtils as ListUtils


-- Merging two dictionaries with control on merged the merge entry in case of collision
-- Note: this function is rather inefficient and also use the inefficient ListUtils.unique
merge: (b -> b -> b) -> Dict comparable b -> Dict comparable b -> Dict comparable b
merge mergefn dict1 dict2 =
    let k1   = Dict.keys dict1
        k2   = Dict.keys dict2
        keys = ListUtils.unique (k1 ++ k2)
        f    = \key acc -> case (Dict.get key dict1, Dict.get key dict2) of
                (Nothing, Nothing) -> acc
                (Just v, Nothing)  -> Dict.insert key v acc
                (Nothing, Just v)  -> Dict.insert key v acc
                (Just v1, Just v2) -> Dict.insert key (mergefn v1 v2) acc
    in List.foldl f Dict.empty keys




