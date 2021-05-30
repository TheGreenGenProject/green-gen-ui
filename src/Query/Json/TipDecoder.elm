module Query.Json.TipDecoder exposing (..)

import Data.Tip exposing (Tip, TipId(..))
import Json.Decode exposing (Decoder, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Query.Json.DecoderUtils exposing(..)


decodeTip: Decoder Tip
decodeTip = succeed Tip
    |> required "id" decodeTipId
    |> required "author" decodeUserId
    |> required "content" string
    |> required "created" decodeTimestamp

decodeTipId: Decoder TipId
decodeTipId = succeed TipId
    |> required "value" decodeUuid
