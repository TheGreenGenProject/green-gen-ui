module Query.Json.SourceDecoder exposing (decodeSources, decodeSource)

import Data.Post exposing (Source(..))
import Json.Decode as Decoder exposing (Decoder)
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