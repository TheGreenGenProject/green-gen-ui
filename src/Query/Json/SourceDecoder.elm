module Query.Json.SourceDecoder exposing (decodeSources, decodeSource, decodeUrl)

import Data.Post exposing (Source(..))
import Data.Url exposing (Url(..))
import Json.Decode as Decoder exposing (Decoder, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Query.Json.DecoderUtils exposing (unitDecoder)

-- Source

--type Source = Myself
--    | Web Url
--    | PostReference PostId
--    | AcademicReference String

decodeSources: Decoder (List Source)
decodeSources = Decoder.list decodeSource

decodeSource: Decoder Source
decodeSource = Decoder.oneOf [
        decodeSourceMySelf
    ]

decodeSourceMySelf: Decoder Source
decodeSourceMySelf = Decoder.field "MySelf" unitDecoder
    |> Decoder.map (\_ -> MySelf)

decodeUrl: Decoder Url
decodeUrl = succeed Url
    |> required "url" string
