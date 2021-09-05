module Query.Event exposing (
    requestParticipation
    , cancelParticipation
    , acceptParticipation
    , rejectParticipation
    , cancelEvent
    , fetchUserEventPosts
    , fetchEventDetails
 )


import Data.Event as Event exposing (EventId)
import Data.Page as Page exposing (Page)
import Data.Post exposing (PostId)
import Data.User as User exposing (UserId)
import Http
import Json.Decode exposing (list)
import Query.CacheQueryUtils exposing (fetchFromIdAndCacheAll)
import Query.Json.DecoderUtils exposing (jsonResolver, unitDecoder)
import Query.Json.EventDecoder exposing (decodeEvent, decodeEventId)
import Query.Json.PostDecoder exposing (decodePostId)
import Query.QueryUtils exposing (authHeader, baseUrl)
import Query.TaskUtils exposing (thread)
import State.Cache as Cache exposing (Cache)
import State.EventState exposing (EventPagedTab, EventTab(..))
import State.UserState exposing (UserInfo)
import Task exposing (Task)
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

fetchEventDetails: Cache -> UserInfo -> EventId -> Cmd Msg
fetchEventDetails cache user eventId =
    fetchAndCacheEventDetails cache user eventId
    |> Task.map (\c -> (c, eventId))
    |> Task.attempt HttpEventDetailsFetched

fetchAndCacheEventDetails: Cache -> UserInfo -> EventId -> Task Http.Error Cache
fetchAndCacheEventDetails cache user eventId = cache
    |> Task.succeed
    |> Task.andThen (\cache1 -> fetchAndCacheEvent cache1 user eventId)

fetchAndCacheEvent: Cache -> UserInfo -> EventId -> Task Http.Error Cache
fetchAndCacheEvent cache user id = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["event", "by-id", id |> Event.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| decodeEvent
    , timeout = Nothing
 } |> Task.map (\res -> Cache.addEvent cache id res)

fetchUserEventPosts: Cache -> UserInfo -> EventPagedTab -> Cmd Msg
fetchUserEventPosts cache user pagedTab = case pagedTab.tab of
    IncomingEventTab              -> fetchIncomingEventPosts cache user pagedTab.page
    ParticipationAcceptedEventTab -> fetchParticipationAcceptedEventPosts cache user pagedTab.page
    ParticipatedEventTab          -> fetchFinishedEventPosts cache user pagedTab.page
    RequestedEventTab             -> fetchPendingEventPosts cache user pagedTab.page
    CancelledEventTab             -> fetchCancelledEventPosts cache user pagedTab.page
    OrganizedEventTab             -> fetchOrganizedEventPosts cache user pagedTab.page

fetchOrganizedEventPosts: Cache -> UserInfo -> Page -> Cmd Msg
fetchOrganizedEventPosts cache user page =
    fetchAllOrganizedEvents user page
    |> Task.andThen (fetchEventPosts user)
    |> Task.andThen (\ids -> fetchFromIdAndCacheAll cache user ids |> thread ids)
    |> Task.map (\(cache1, ids) -> (cache1, { tab = OrganizedEventTab, page = page}, ids))
    |> Task.attempt HttpEventPostsFetched

fetchAllOrganizedEvents: UserInfo -> Page -> Task Http.Error (List EventId)
fetchAllOrganizedEvents user page = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["event", "by-owner", User.toString user.id, page |> Page.number |> String.fromInt] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| list decodeEventId
    , timeout = Nothing
 }

fetchFinishedEventPosts: Cache -> UserInfo -> Page -> Cmd Msg
fetchFinishedEventPosts cache user page =
    fetchAllFinishedEvents user page
    |> Task.andThen (fetchEventPosts user)
    |> Task.andThen (\ids -> fetchFromIdAndCacheAll cache user ids |> thread ids)
    |> Task.map (\(cache1, ids) -> (cache1, { tab = ParticipatedEventTab, page = page}, ids))
    |> Task.attempt HttpEventPostsFetched

fetchAllFinishedEvents: UserInfo -> Page -> Task Http.Error (List EventId)
fetchAllFinishedEvents user page = fetchAllEvents user "finished" page

fetchIncomingEventPosts: Cache -> UserInfo -> Page -> Cmd Msg
fetchIncomingEventPosts cache user page =
    fetchAllIncomingEvents user page
    |> Task.andThen (fetchEventPosts user)
    |> Task.andThen (\ids -> fetchFromIdAndCacheAll cache user ids |> thread ids)
    |> Task.map (\(cache1, ids) -> (cache1, { tab = IncomingEventTab, page = page}, ids))
    |> Task.attempt HttpEventPostsFetched

fetchAllIncomingEvents: UserInfo -> Page -> Task Http.Error (List EventId)
fetchAllIncomingEvents user page = fetchAllEvents user "incoming" page

fetchParticipationAcceptedEventPosts: Cache -> UserInfo -> Page -> Cmd Msg
fetchParticipationAcceptedEventPosts cache user page =
    fetchAllParticipationAcceptedEvents user page
    |> Task.andThen (fetchEventPosts user)
    |> Task.andThen (\ids -> fetchFromIdAndCacheAll cache user ids |> thread ids)
    |> Task.map (\(cache1, ids) -> (cache1, { tab = IncomingEventTab, page = page}, ids))
    |> Task.attempt HttpEventPostsFetched

fetchAllParticipationAcceptedEvents: UserInfo -> Page -> Task Http.Error (List EventId)
fetchAllParticipationAcceptedEvents user page = fetchAllEvents user "accepted" page

fetchCancelledEventPosts: Cache -> UserInfo -> Page -> Cmd Msg
fetchCancelledEventPosts cache user page =
    fetchAllCancelledEvents user page
    |> Task.andThen (fetchEventPosts user)
    |> Task.andThen (\ids -> fetchFromIdAndCacheAll cache user ids |> thread ids)
    |> Task.map (\(cache1, ids) -> (cache1, { tab = CancelledEventTab, page = page}, ids))
    |> Task.attempt HttpEventPostsFetched

fetchAllCancelledEvents: UserInfo -> Page -> Task Http.Error (List EventId)
fetchAllCancelledEvents user page = fetchAllEvents user "cancelled" page

fetchPendingEventPosts: Cache -> UserInfo -> Page -> Cmd Msg
fetchPendingEventPosts cache user page =
    fetchAllPendingEvents user page
    |> Task.andThen (fetchEventPosts user)
    |> Task.andThen (\ids -> fetchFromIdAndCacheAll cache user ids |> thread ids)
    |> Task.map (\(cache1, ids) -> (cache1, { tab = RequestedEventTab, page = page}, ids))
    |> Task.attempt HttpEventPostsFetched

fetchAllPendingEvents: UserInfo -> Page -> Task Http.Error (List EventId)
fetchAllPendingEvents user page = fetchAllEvents user "pending-request" page


{-- Helpers --}

fetchAllEvents: UserInfo -> String -> Page -> Task Http.Error (List EventId)
fetchAllEvents user searchFilter page = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["event", "by-user", searchFilter, User.toString user.id, page |> Page.number |> String.fromInt] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| list decodeEventId
    , timeout = Nothing
 }

fetchEventPosts: UserInfo -> List EventId -> Task Http.Error (List PostId)
fetchEventPosts user ids = ids
    |> List.map (fetchChallengePost user)
    |> Task.sequence

fetchChallengePost: UserInfo -> EventId -> Task Http.Error PostId
fetchChallengePost user id = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["post", "by-content", "event", id |> Event.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| decodePostId
    , timeout = Nothing
 }
