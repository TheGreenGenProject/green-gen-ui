module Query.Event exposing (..)


import Data.Event as Event exposing (EventId)
import Data.User as User exposing (UserId)
import Http
import Query.Json.DecoderUtils exposing (jsonResolver, unitDecoder)
import Query.QueryUtils exposing (authHeader, baseUrl)
import State.Cache exposing (Cache)
import State.UserState exposing (UserInfo)
import Task
import Update.Msg exposing (Msg(..))
import Url.Builder exposing (absolute, string)


requestParticipation: Cache -> UserInfo -> EventId -> Cmd Msg
requestParticipation cache user eventId = Http.task {
    method = "POST"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["event", "participation", "request"] [string "event-id" (eventId |> Event.toString)]
    , body = Http.emptyBody
    , resolver = jsonResolver <| unitDecoder
    , timeout = Nothing
 } |> Task.map (\_ -> eventId)
   |> Task.attempt HttpEventParticipationRequested

cancelParticipation: Cache -> UserInfo -> EventId -> Cmd Msg
cancelParticipation cache user eventId = Http.task {
    method = "POST"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["event", "participation", "cancel"] [string "event-id" (eventId |> Event.toString)]
    , body = Http.emptyBody
    , resolver = jsonResolver <| unitDecoder
    , timeout = Nothing
 } |> Task.map (\_ -> eventId)
   |> Task.attempt HttpEventParticipationRequestCancelled

acceptParticipation: Cache -> UserInfo -> EventId -> UserId -> Cmd Msg
acceptParticipation cache user eventId participantId = Http.task {
    method = "POST"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["event", "participation", "accept"] [
        string "event-id" (eventId |> Event.toString)
        , string "participant-id" (User.toString participantId)]
    , body = Http.emptyBody
    , resolver = jsonResolver <| unitDecoder
    , timeout = Nothing
 } |> Task.attempt HttpEventParticipationAccepted

rejectParticipation: Cache -> UserInfo -> EventId -> UserId -> Cmd Msg
rejectParticipation cache user eventId participantId = Http.task {
    method = "POST"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["event", "participation", "reject"] [
        string "event-id" (eventId |> Event.toString)
        , string "participant-id" (User.toString participantId)]
    , body = Http.emptyBody
    , resolver = jsonResolver <| unitDecoder
    , timeout = Nothing
 } |> Task.attempt HttpEventParticipationRejected

cancelEvent: Cache -> UserInfo -> EventId -> Cmd Msg
cancelEvent cache user eventId = Http.task {
    method = "POST"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["event", "cancel"] [string "event-id" (eventId |> Event.toString)]
    , body = Http.emptyBody
    , resolver = jsonResolver <| unitDecoder
    , timeout = Nothing
 } |> Task.attempt HttpEventParticipationRejected