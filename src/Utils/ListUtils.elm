module Utils.ListUtils exposing (
    fromMaybe
    , nth
    , flatten
    , unique
    , split2
    , rotate
    , rotaten
    , fix
    , find
    , replace
    , delete
    , nonEmptyToMaybe)


fromMaybe: Maybe a -> List a
fromMaybe mx = case mx of
    Just x  -> [x]
    Nothing -> [ ]

flatten: List (Maybe a) -> List a
flatten = List.concatMap fromMaybe

nth: Int -> List a -> Maybe a
nth n =  List.head << List.drop n

-- Inefficient algorithm, non tail-rec
-- Keeps the ordering and doesn't require a comparable
unique: List a -> List a
unique xs = case xs of
    []      -> []
    y :: ys -> y :: (ys |> List.filter (\x -> not (x == y)) |> unique)

-- Split in 2 lists by putting first element in first bucket, second one in teh second bucket, third one in the first etc.
split2: List a -> (List a, List a)
split2 xs = case xs of
    []      -> ([], [])
    [x]     -> ([x],[])
    x :: y :: ys -> let (left, right) = split2 ys in (x :: left, y :: right)

rotate: List a -> List a
rotate xs = case xs of
    [] -> []
    x :: rest -> rest ++ [x]

rotaten: Int -> List a -> List a
rotaten n xs = if n > 0 then rotaten (n - 1) (rotate xs) else xs

-- A kind of fix point style list generator
fix: a -> (a -> a) -> (a -> Bool) -> List a
fix start func stopCondition = fixLoop start func stopCondition []

fixLoop x f stopCondition acc = let res = f x in
    if stopCondition res then acc else fixLoop res f stopCondition (res :: acc)

find: (a -> Bool) -> List a -> Maybe a
find predicate lst = case lst of
    []      -> Nothing
    x :: xs -> if predicate x then Just x else find predicate xs

nonEmptyToMaybe: List a -> Maybe (List a)
nonEmptyToMaybe xs = if List.isEmpty xs then Nothing else Just xs

replace: a -> Int -> List a -> List a
replace elt index xs =
    (xs |> List.take index) ++ [elt] ++ (xs |> List.drop (index+1))

delete: Int -> List a -> List a
delete index xs =
    (xs |> List.take index) ++ (xs |> List.drop (index+1))



