module Data.Location exposing (..)

import Data.Url exposing (Url)

type Latitude = Latitude Float
type Longitude = Longitude Float
type ZipCode = ZipCode String
type Country = World | Country String

type Location = Online Url
    | Physical Latitude Longitude
    | Address (Maybe String) (Maybe ZipCode) Country