module Query.Json.LocationDecoder exposing (decodeLocation)

import Data.Location exposing (Country(..), Latitude(..), Location(..), Longitude(..), ZipCode(..))
import Json.Decode as Decoder exposing (Decoder, float, maybe, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Query.Json.SourceDecoder exposing (decodeUrl)


decodeLocation: Decoder Location
decodeLocation = oneOf [
    decodeOnline
    , decodeMapUrl
    , decodeGeoLocation
    , decodePhysicalAddress
 ]

decodeOnline: Decoder Location
decodeOnline = (succeed Online |> required "url" decodeUrl)
    |> Decoder.field "Online"

decodeMapUrl: Decoder Location
decodeMapUrl = (succeed MapUrl |> required "url" decodeUrl)
    |> Decoder.field "MapUrl"

decodeGeoLocation: Decoder Location
decodeGeoLocation = (succeed GeoLocation
        |> required "latitude" decodeLatitude
        |> required "longitude" decodeLongitude)
    |> Decoder.field "coordinates"
    |> Decoder.field "GeoLocation"

decodePhysicalAddress: Decoder Location
decodePhysicalAddress = (succeed Address
        |> required "address" (maybe string)
        |> required "zipCode" (maybe decodeZipCode)
        |> required "country" decodeCountry)
    |> Decoder.field "Address"

decodeLatitude: Decoder Latitude
decodeLatitude = succeed Latitude
    |> required "value" float

decodeLongitude: Decoder Longitude
decodeLongitude = succeed Longitude
    |> required "value" float

decodeZipCode: Decoder ZipCode
decodeZipCode = string |> Decoder.map ZipCode

decodeCountry: Decoder Country
decodeCountry = succeed Country
    |> required "name" string