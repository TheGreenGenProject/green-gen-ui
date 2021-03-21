module Utils.MaybeUtils exposing (..)

orElse: Maybe a -> Maybe a -> Maybe a
orElse fallback src = case src of
    Nothing -> fallback
    Just x  -> src
