module Data.Location exposing (..)

import Data.Url exposing (Url(..))

type Latitude = Latitude Float
type Longitude = Longitude Float
type ZipCode = ZipCode String
type Country = Country String

type Location = Online Url
    | GeoLocation Latitude Longitude
    | MapUrl Url
    | Address (Maybe String) (Maybe ZipCode) Country

toMapUrl: Int -> Location -> Url
toMapUrl zoom loc = case loc of
    MapUrl url -> url
    GeoLocation (Latitude lat) (Longitude long)  ->
        Url ("https://www.openstreetmap.org/?mlat=" ++
            (String.fromFloat lat) ++ "&mlon=" ++ (String.fromFloat long) ++
            "#map=" ++ (zoom |> String.fromInt)++ "/" ++ (String.fromFloat lat) ++ "/" ++ (String.fromFloat long))
    _ -> Url "https://www.openstreetmap.org"

formatAddress: Location -> String
formatAddress address = case address of
    Address street zipCode (Country name) ->
        [street |> Maybe.withDefault "", zipCode |> Maybe.map (\(ZipCode zc) -> zc) |> Maybe.withDefault "", name]
        |> List.filter (String.isEmpty >> not)
        |> String.join ", "
    _                              -> "<Invalid address>"