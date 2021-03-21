module Query.Authentication exposing (logon, logoff)

import Data.Authentication exposing (Authentication(..))
import Data.User as User
import Http exposing(Header(..))
import Data.Hash exposing (Hash(..))
import Data.Token as Token exposing (Token)
import Query.Json.DecoderUtils exposing (jsonResolver, unitDecoder)
import Query.Json.UserDecoder exposing (decodeAuthentication, decodeUserProfile)
import Query.QueryUtils exposing (baseUrl)
import State.AppState exposing (AuthError(..))
import State.UserState exposing (UserInfo)
import Task exposing (Task)
import Update.Msg exposing (Msg(..))
import Url.Builder exposing (..)


logon: Hash -> Hash -> Cmd Msg
logon email pw = authenticatedUser email pw
    |> Task.attempt HttpAuthenticated

logoff: Token -> Cmd Msg
logoff tk = Http.post {
    url = baseUrl ++ absolute ["logoff"] [token tk]
    , body = Http.emptyBody
    , expect = Http.expectJson HttpLoggedOff unitDecoder
 }


-- Http helpers

authenticatedUser: Hash -> Hash -> Task AuthError UserInfo
authenticatedUser email pw = authenticate email pw
    |> Task.andThen userInfo

authenticate: Hash -> Hash -> Task AuthError Authentication
authenticate (Hash email) (Hash pw) = Http.task {
        method     = "POST"
        , headers  = []
        , url      = baseUrl ++ absolute ["auth", "authenticate"] [string "email-hash" email, string "password-hash" pw]
        , body     = Http.emptyBody
        , resolver = jsonResolver <| decodeAuthentication
        , timeout  = Nothing
    } |> Task.mapError HttpError

userInfo: Authentication -> Task AuthError UserInfo
userInfo auth = case auth of
    NotAuthenticated -> Debug.log "Authentication failed" <| Task.fail AuthenticationFailed
    Authenticated tok userId _ -> Debug.log "Authenticated. Asking for profile"
        Http.task {
            method     = "GET"
            , headers  = [authHeader tok]
            , url      = baseUrl ++ absolute ["user", "profile", userId |> User.toString] []
            , body     = Http.emptyBody
            , resolver = jsonResolver <| (decodeUserProfile tok)
            , timeout  = Nothing
        } |> Task.mapError HttpError

-- Helpers

token: Token -> QueryParameter
token tk = tk |> Token.toString |> string "token"

authHeader: Token -> Header
authHeader tok = Http.header "Authorization" (Token.toString tok)



