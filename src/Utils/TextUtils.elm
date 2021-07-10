module Utils.TextUtils exposing (..)

import Data.Hashtag exposing (Hashtag(..))
import Data.Post exposing (Source(..))
import Regex exposing (Regex)
import Utils.ListUtils as ListUtils


userRegex = unsafeRegex "@(\\w|\\d)+"
academicRefSourceRegex = unsafeRegex "\\ref{[^{}#@]+\\}"
urlRefSourceRegex = unsafeRegex "\\link{(http://|https://)([^{}])+\\}"

unsafeRegex: String -> Regex
unsafeRegex = Maybe.withDefault Regex.never << Regex.fromString

isEmpty: String -> Bool
isEmpty = String.trim >> String.isEmpty

nonEmpty: String -> Bool
nonEmpty = not << isEmpty

take: Int -> String -> String
take n str = str
    |> String.toList
    |> List.take n
    |> List.map (String.fromChar)
    |> String.join ""


{-- Hashtags --}

hashtagsFrom: String -> List Hashtag
hashtagsFrom txt = txt
    |> Regex.split (unsafeRegex " |\t|,|;|-|\\.|\\[|\\]|\\(|\\)|\\{|\\}")
    |> List.filter isHashtag
    |> List.map (String.dropLeft 1 >> String.toLower >> Hashtag)
    |> ListUtils.unique

isHashtag: String -> Bool
isHashtag tag =
    (tag |> String.length) > 2 &&
    (tag |> String.startsWith"#")  &&
    (tag |> String.dropLeft 1 |> isAlphaNumerical)


{-- Users --}

isUser: String -> Bool
isUser = Regex.contains userRegex

userPseudosFrom: String -> List String
userPseudosFrom txt = txt
    |> Regex.split (unsafeRegex " |\t|,|;|-|\\.|\\[|\\]|\\(|\\)|\\{|\\}")
    |> List.filter isUser
    |> List.map (String.dropLeft 1)

{-- Sources --}

sourcesFrom: String -> List Source
sourcesFrom txt = Regex.find academicRefSourceRegex txt
    |> List.map (.match >> AcademicReference)

isSource: String -> Bool
isSource txt = isWebSource txt || isAcademicReference txt

isWebSource: String -> Bool
isWebSource txt = Regex.contains urlRefSourceRegex txt

isAcademicReference: String -> Bool
isAcademicReference = Regex.contains academicRefSourceRegex

isMyself: String -> Bool
isMyself str = String.toLower str == "myself"


{-- Quoted text (text containing user references, sources or hashtags --}

type QuotedString =
    Str String
    | HashtagQuote String
    | UserQuote String

parseQuotedText: String -> List QuotedString
parseQuotedText txt =
    let splitted = txt |> String.split " "
        parse xs = case xs of
            []  -> []
            [x] ->
                if isUser x then [UserQuote x]
                else if isHashtag x then [HashtagQuote x]
                else [Str x]
            x :: y :: rest ->
                if isUser x then (UserQuote x) :: parse (y :: rest)
                else if isHashtag x then (HashtagQuote x) :: parse (y :: rest)
                else (Str x) :: parse (y :: rest)
    in parse splitted

isAlphaNumerical: String -> Bool
isAlphaNumerical = String.toList >> List.all (Char.isAlphaNum)

format2Digits: Int -> String
format2Digits n =
    (if n >= 0 && n <= 9 then "0"  else "") ++ (String.fromInt n)

format4Digits: Int -> String
format4Digits n =
    (if n >= 0 && n < 10 then "000"
     else if n >= 10 && n < 100 then "00"
     else if n >= 100 && n < 1000 then "0"
     else "") ++
    (String.fromInt n)