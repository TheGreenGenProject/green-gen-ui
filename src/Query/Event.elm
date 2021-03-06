module Query.Event exposing (
    postEvent
    , requestParticipation
    , cancelParticipation
    , acceptParticipation
    , rejectParticipation
    , cancelEvent
    , fetchUserEventPosts
    , fetchEventDetails
    , fetchEventDetailsContentForTab
    , fetchEventConversation
 )


import Data.Event as Event exposing (Event, EventId)
import Data.Hashtag exposing (Hashtag(..))
import Data.Location exposing (Country(..), Latitude(..), Location(..), Longitude(..), ZipCode(..))
import Data.Page as Page exposing (Page)
import Data.Post exposing (PostId)
import Data.Schedule as Schedule exposing (Duration(..), Schedule(..), UTCTimestamp(..))
import Data.Url exposing (Url(..))
import Data.User as User exposing (UserId)
import Http
import Json.Decode exposing (list)
import Query.AggregatedCacheQueryUtils exposing (fetchAggregatedAndCacheAll)
import Query.CacheQueryUtils exposing (fetchAndCacheAllUsers, fetchAndCacheEvent, fetchAndCacheEventCancelledStatus, fetchAndCacheEventParticipantCount, fetchAndCacheEventParticipationRequestStatus, fetchAndCacheEventParticipationStatus)
import Query.Conversation as Conversation
import Query.Json.DecoderUtils exposing (decodeUserId, jsonResolver, unitDecoder)
import Query.Json.EventDecoder exposing (decodeEvent, decodeEventId)
import Query.Json.PostDecoder exposing (decodePostId)
import Query.QueryUtils exposing (authHeader, baseUrl)
import Query.TaskUtils exposing (thread)
import State.Cache exposing (Cache)
import State.EventDetailsState exposing (EventDetailsPagedTab, EventDetailsTab(..))
import State.EventState exposing (EventPagedTab, EventTab(..))
import State.FormState exposing (NewEventWizardState)
import State.UserState exposing (UserInfo)
import Task exposing (Task)
import Update.Msg exposing (Msg(..))
import Url.Builder exposing (absolute, int, string)
import Utils.TextUtils as TextUtils


postEvent: UserInfo -> NewEventWizardState -> Cmd Msg
postEvent user newEvent =
    createNewEvent user newEvent
    |> Task.andThen (\event -> createNewEventPost user newEvent event.id)
    |> Task.attempt HttpNewEventPosted

createNewEvent: UserInfo -> NewEventWizardState -> Task Http.Error Event
createNewEvent user newEvent = Http.task {
        method     = "POST"
        , headers  = [authHeader user]
        , url      = baseUrl ++ absolute ["event", "new"] [
            int "max-participant" newEvent.maxParticipants
            , string "description" (newEvent.description |> Maybe.withDefault "")
            , string "location" (newEvent.location |> toLocationParam)
            , string "schedule"  (newEvent |> toSchedule |> toScheduleParam)]
        , body     = Http.emptyBody
        , resolver = jsonResolver <| decodeEvent
        , timeout  = Nothing
    }

createNewEventPost: UserInfo -> NewEventWizardState -> EventId -> Task Http.Error ()
createNewEventPost user state eventId = Http.task {
    method     = "POST"
    , headers  = [authHeader user]
    , url      = baseUrl ++ absolute ["post", "new", "event"] [
        string "event-id" (eventId |> Event.toString)
        , string "hashtags" (state |> hashtagsAsParameter)]
    , body     = Http.emptyBody
    , resolver = jsonResolver <| unitDecoder
    , timeout  = Nothing
  }


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
 } |> Task.map (\_ -> eventId)
   |> Task.attempt HttpEventParticipationAccepted

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
 } |> Task.map (\_ -> eventId)
   |> Task.attempt HttpEventParticipationRejected

cancelEvent: Cache -> UserInfo -> EventId -> Cmd Msg
cancelEvent cache user eventId = Http.task {
    method = "POST"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["event", "cancel"] [string "event-id" (eventId |> Event.toString)]
    , body = Http.emptyBody
    , resolver = jsonResolver <| unitDecoder
    , timeout = Nothing
 } |> Task.map (\_ -> eventId)
   |> Task.attempt HttpEventCancelled

fetchEventDetails: Cache -> UserInfo -> EventId -> Cmd Msg
fetchEventDetails cache user eventId =
    fetchAndCacheEventDetails cache user eventId
    |> Task.map (\c -> (c, eventId))
    |> Task.attempt HttpEventDetailsFetched

fetchAndCacheEventDetails: Cache -> UserInfo -> EventId -> Task Http.Error Cache
fetchAndCacheEventDetails cache user eventId =
    fetchAndCacheEvent cache user eventId
    |> Task.andThen (\cache1 -> fetchAndCacheEventParticipationStatus cache1 user eventId)
    |> Task.andThen (\cache2 -> fetchAndCacheEventCancelledStatus cache2 user eventId)
    |> Task.andThen (\cache3 -> fetchAndCacheEventParticipationRequestStatus cache3 user eventId)
    |> Task.andThen (\cache4 -> fetchAndCacheEventParticipantCount cache4 user eventId)

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
    |> Task.andThen (\ids -> fetchAggregatedAndCacheAll cache user ids |> thread ids)
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
    |> Task.andThen (\ids -> fetchAggregatedAndCacheAll cache user ids |> thread ids)
    |> Task.map (\(cache1, ids) -> (cache1, { tab = ParticipatedEventTab, page = page}, ids))
    |> Task.attempt HttpEventPostsFetched

fetchAllFinishedEvents: UserInfo -> Page -> Task Http.Error (List EventId)
fetchAllFinishedEvents user page = fetchAllEvents user "finished" page

fetchIncomingEventPosts: Cache -> UserInfo -> Page -> Cmd Msg
fetchIncomingEventPosts cache user page =
    fetchAllIncomingEvents user page
    |> Task.andThen (fetchEventPosts user)
    |> Task.andThen (\ids -> fetchAggregatedAndCacheAll cache user ids |> thread ids)
    |> Task.map (\(cache1, ids) -> (cache1, { tab = IncomingEventTab, page = page}, ids))
    |> Task.attempt HttpEventPostsFetched

fetchAllIncomingEvents: UserInfo -> Page -> Task Http.Error (List EventId)
fetchAllIncomingEvents user page = fetchAllEvents user "incoming" page

fetchParticipationAcceptedEventPosts: Cache -> UserInfo -> Page -> Cmd Msg
fetchParticipationAcceptedEventPosts cache user page =
    fetchAllParticipationAcceptedEvents user page
    |> Task.andThen (fetchEventPosts user)
    |> Task.andThen (\ids -> fetchAggregatedAndCacheAll cache user ids |> thread ids)
    |> Task.map (\(cache1, ids) -> (cache1, { tab = ParticipationAcceptedEventTab, page = page}, ids))
    |> Task.attempt HttpEventPostsFetched

fetchAllParticipationAcceptedEvents: UserInfo -> Page -> Task Http.Error (List EventId)
fetchAllParticipationAcceptedEvents user page = fetchAllEvents user "accepted" page

fetchCancelledEventPosts: Cache -> UserInfo -> Page -> Cmd Msg
fetchCancelledEventPosts cache user page =
    fetchAllCancelledEvents user page
    |> Task.andThen (fetchEventPosts user)
    |> Task.andThen (\ids -> fetchAggregatedAndCacheAll cache user ids |> thread ids)
    |> Task.map (\(cache1, ids) -> (cache1, { tab = CancelledEventTab, page = page}, ids))
    |> Task.attempt HttpEventPostsFetched

fetchAllCancelledEvents: UserInfo -> Page -> Task Http.Error (List EventId)
fetchAllCancelledEvents user page = fetchAllEvents user "cancelled" page

fetchPendingEventPosts: Cache -> UserInfo -> Page -> Cmd Msg
fetchPendingEventPosts cache user page =
    fetchAllPendingEvents user page
    |> Task.andThen (fetchEventPosts user)
    |> Task.andThen (\ids -> fetchAggregatedAndCacheAll cache user ids |> thread ids)
    |> Task.map (\(cache1, ids) -> (cache1, { tab = RequestedEventTab, page = page}, ids))
    |> Task.attempt HttpEventPostsFetched

fetchAllPendingEvents: UserInfo -> Page -> Task Http.Error (List EventId)
fetchAllPendingEvents user page = fetchAllEvents user "pending-request" page


fetchEventDetailsContentForTab: Cache -> UserInfo -> EventId -> EventDetailsPagedTab -> Cmd Msg
fetchEventDetailsContentForTab cache user eventId pagedTab = case pagedTab.tab of
    EventDetailsTab    -> fetchEventDetails cache user eventId
    PendingRequestsTab -> fetchAllPendingRequests cache user eventId pagedTab.page
    ParticipantsTab    -> fetchAllParticipants cache user eventId pagedTab.page
    EventDiscussionTab -> fetchEventConversation cache user eventId pagedTab.page

fetchAllPendingRequests: Cache -> UserInfo -> EventId -> Page -> Cmd Msg
fetchAllPendingRequests cache user eventId page =
    fetchPendingRequests user eventId page
    |> Task.andThen (\ids -> fetchAndCacheAllUsers cache user ids |> thread ids)
    |> Task.map (\(cache1, ids) -> (cache1, { tab = PendingRequestsTab, page = page}, ids))
    |> Task.attempt HttpEventPendingRequestsFetched

fetchPendingRequests: UserInfo -> EventId -> Page -> Task Http.Error (List UserId)
fetchPendingRequests user eventId page =  Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["event", "participation", "requests", eventId |> Event.toString, page |> Page.number |> String.fromInt] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| list decodeUserId
    , timeout = Nothing
 }

fetchAllParticipants: Cache -> UserInfo -> EventId -> Page -> Cmd Msg
fetchAllParticipants cache user eventId page =
    fetchParticipants user eventId page
    |> Task.andThen (\ids -> fetchAndCacheAllUsers cache user ids |> thread ids)
    |> Task.map (\(cache1, ids) -> (cache1, { tab = ParticipantsTab, page = page}, ids))
    |> Task.attempt HttpEventParticipantsFetched

fetchParticipants: UserInfo -> EventId -> Page -> Task Http.Error (List UserId)
fetchParticipants user eventId page =  Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["event", "participation", "participants", eventId |> Event.toString, page |> Page.number |> String.fromInt] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| list decodeUserId
    , timeout = Nothing
 }


fetchEventConversation: Cache -> UserInfo -> EventId -> Page -> Cmd Msg
fetchEventConversation cache user eventId page =
    fetchEventPost user eventId
    |> Task.andThen (\postId -> Conversation.fetchAndCacheConversation cache user postId page)
    |> Task.attempt HttpConversationPageFetched

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
    |> List.map (fetchEventPost user)
    |> Task.sequence

fetchEventPost: UserInfo -> EventId -> Task Http.Error PostId
fetchEventPost user id = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["post", "by-content", "event", id |> Event.toString] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| decodePostId
    , timeout = Nothing
 }


toSchedule: NewEventWizardState -> Schedule
toSchedule state =
    let start = state.start |> Maybe.withDefault (UTC 0)
        end   = state.end |> Maybe.withDefault (UTC 0)
    in OneOff start end

toScheduleParam: Schedule -> String
toScheduleParam schedule = "oneoff(" ++
    (schedule |> Schedule.start |> toMillis |> String.fromInt) ++ "," ++
    (schedule |> Schedule.end |> toMillis |> String.fromInt) ++ ")"

toMillis: UTCTimestamp -> Int
toMillis (UTC millis) = millis

toLocationParam: Maybe Location -> String
toLocationParam loc = case loc of
    Just (Online (Url url))                           -> "url(" ++ url ++ ")"
    Just (GeoLocation (Latitude lat) (Longitude lon)) -> "geoloc(" ++ (String.fromFloat lat) ++ "," ++ (String.fromFloat lon) ++ ")"
    Just (MapUrl (Url url))                           -> "map(" ++ url ++ ")"
    Just (Address street zip (Country country))       -> "address(" ++
        (street |> Maybe.withDefault "") ++ "," ++
        (zip |> Maybe.map (\(ZipCode zp) -> zp) |> Maybe.withDefault "") ++ "," ++
        country ++ ")"
    Nothing                                           -> "invalid!"

hashtagsAsParameter: NewEventWizardState -> String
hashtagsAsParameter state =
    let content = state.description |> Maybe.withDefault ""
        hashtags = TextUtils.hashtagsFrom content
    in hashtags
        |> List.map (\(Hashtag x) -> x)
        |> String.join "+"
