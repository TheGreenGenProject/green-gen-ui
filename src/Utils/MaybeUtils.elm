module Utils.MaybeUtils exposing (..)

orElse: Maybe a -> Maybe a -> Maybe a
orElse fallback src = case src of
    Nothing -> fallback
    Just x  -> src

isEmpty: Maybe a -> Bool
isEmpty maybe = case maybe of
    Nothing -> True
    Just _  -> False

nonEmpty: Maybe a -> Bool
nonEmpty = not << isEmpty

maybeString: String -> Maybe String
maybeString str =
    if String.isEmpty str then Nothing else Just str

