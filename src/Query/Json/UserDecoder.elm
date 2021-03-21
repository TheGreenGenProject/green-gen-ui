module Query.Json.UserDecoder exposing (decodeAuthentication, decodeUserProfile, decodeUserList)

import Data.Authentication exposing (Authentication(..))
import Data.Token exposing (Token)
import Data.User exposing (UserId)
import Json.Decode as Decoder exposing (Decoder, bool, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)
import State.UserState exposing (UserInfo)
import Query.Json.DecoderUtils exposing(..)


decodeAuthentication: Decoder Authentication
decodeAuthentication = Decoder.oneOf [decodeAuthenticated, decodeNotAuthenticated]

decodeAuthenticated: Decoder Authentication
decodeAuthenticated = Decoder.field "Authenticated"
    (succeed Authenticated
        |> required "token" decodeToken
        |> required "user" decodeUserId
        |> required "validUntil" decodeTimestamp)

decodeNotAuthenticated: Decoder Authentication
decodeNotAuthenticated = Decoder.field "NotAuthenticated" unitDecoder
    |> Decoder.map (\_ -> NotAuthenticated)


-- UserInfo populated from server profile query
decodeUserProfile: Token -> Decoder UserInfo
decodeUserProfile token = succeed UserInfo
    |> required "id" decodeUserId
    |> required "pseudo" decodePseudo
    |> required "intro" string
    |> hardcoded True
    |> required "since" decodeTimestamp
    |> hardcoded token

decodePseudo: Decoder String
decodePseudo = Decoder.field "value" string

decodeUserList: Decoder (List UserId)
decodeUserList = Decoder.list decodeUserId
