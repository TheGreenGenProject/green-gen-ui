module Query.Json.DecoderUtils exposing (..)

import Data.Schedule exposing (UTCTimestamp(..))
import Data.Token exposing (Token(..))
import Data.User exposing (UserId(..))
import Http
import Json.Decode as Decoder exposing (Decoder, nullable, succeed)
import Json.Decode.Pipeline exposing (required)
import Uuid exposing (Uuid(..))
import Json.Decode exposing (Decoder, int, succeed)


unitDecoder: Decoder ()
unitDecoder = succeed ()

decodeUuid: Decoder Uuid
decodeUuid = Decoder.field "uuid" Uuid.decoder

decodeToken: Decoder Token
decodeToken = succeed Token
    |> required "value" decodeUuid

decodeUserId: Decoder UserId
decodeUserId = succeed UserId
    |> required "value" decodeUuid

decodeTimestamp: Decoder UTCTimestamp
decodeTimestamp = succeed UTC
    |> required "value" int

decodePair: Decoder a -> Decoder b -> Decoder (a,b)
decodePair decode1 decode2 = Decoder.map2 Tuple.pair
    (Decoder.index 0 decode1)
    (Decoder.index 1 decode2)

-- Decoder returning a default value if null is decoded
decodeWithDefault: Decoder a -> a -> Decoder a
decodeWithDefault decoder default = Decoder.map
    (\v -> case v of
        Nothing -> default
        Just x  -> x
    ) (nullable decoder)

decodeIntWithDefault: Int -> Decoder Int
decodeIntWithDefault n = decodeWithDefault (Decoder.field "value" int) 0

handleJsonResponse : Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)
        Http.Timeout_ ->
            Err Http.Timeout
        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)
        Http.NetworkError_ ->
            Err Http.NetworkError
        Http.GoodStatus_ _ body ->
            case Json.Decode.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)
                Ok result ->
                    Ok result

jsonResolver : Decoder a -> Http.Resolver Http.Error a
jsonResolver = Http.stringResolver << handleJsonResponse
