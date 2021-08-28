module Query.Json.PartnerDecoder exposing (..)


import Data.Partner exposing (Partner, PartnerId(..))
import Json.Decode exposing (Decoder, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Query.Json.DecoderUtils exposing (decodeUserId, decodeUuid)
import Query.Json.SourceDecoder exposing (decodeUrl)

decodePartner: Decoder Partner
decodePartner = succeed Partner
    |> required "id" decodePartnerId
    |> required "userId" decodeUserId
    |> required "name" string
    |> required "description" string
    |> required "url" decodeUrl

decodePartnerId: Decoder PartnerId
decodePartnerId = succeed PartnerId
    |> required "value" decodeUuid
