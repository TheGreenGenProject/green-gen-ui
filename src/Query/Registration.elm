module Query.Registration exposing (checkPseudoAvailability, register, verifyAccount)

import Data.Hash as Hash exposing (Hash)
import Data.VerificationCode as VerificationCode
import Http
import Http
import Json.Decode exposing (bool)
import Query.Json.DecoderUtils exposing (jsonResolver, unitDecoder)
import Query.QueryUtils exposing (baseUrl)
import State.FormState exposing (RegistrationFormState)
import Task
import Update.Msg exposing (Msg(..))
import Url.Builder exposing (absolute, string)


checkPseudoAvailability: String -> Cmd Msg
checkPseudoAvailability pseudo =  Http.task {
    method     = "GET"
    , headers  = [] -- typically user will not be authenticated when checking for pseudo
    , url      = baseUrl ++ absolute ["registration", "check-availability", "pseudo", pseudo] []
    , body     = Http.emptyBody
    , resolver = jsonResolver <| bool
    , timeout  = Nothing
  } |> Task.map (\checked -> (pseudo, checked))
    |> Task.attempt HttpPseudoAvailabilityChecked

register: RegistrationFormState -> Cmd Msg
register state =
    let email    = state.email |> Maybe.map md5String |> Maybe.withDefault ""
        password = state.password |> Maybe.map md5String |> Maybe.withDefault ""
        pseudo   = state.pseudo |> Maybe.withDefault ""
        intro    = state.introduction |> Maybe.withDefault ""
    in
    Http.task {
        method     = "POST"
        , headers  = [] -- typically user will not be authenticated when checking for pseudo
        , url      = baseUrl ++ absolute ["registration", "new"] [
            string "email-hash" email
            , string "password-hash" password
            , string "pseudo" pseudo
            , string "profile-introduction" intro
         ]
        , body     = Http.emptyBody
        , resolver = jsonResolver <| unitDecoder
        , timeout  = Nothing
      } |> Task.attempt HttpNewAccountRegistered

verifyAccount: RegistrationFormState -> Cmd Msg
verifyAccount state =
    let email    = state.email |> Maybe.map md5String |> Maybe.withDefault ""
        code     = state.verification |> Maybe.map VerificationCode.format |> Maybe.withDefault ""
    in
    Http.task {
        method     = "POST"
        , headers  = [] -- typically user will not be authenticated when checking for pseudo
        , url      = baseUrl ++ absolute ["validation", "validate"] [
            string "email-hash" email
            , string "validation-code" code
         ]
        , body     = Http.emptyBody
        , resolver = jsonResolver <| unitDecoder
        , timeout  = Nothing
      } |> Task.attempt HttpNewAccountVerified

md5String: String -> String
md5String = Hash.md5 >> Hash.toString