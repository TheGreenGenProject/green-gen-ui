module Query.Conversation exposing (
    fetchConversation,
    flagComment,
    unflagComment,
    postComment)

import Data.Conversation as Conversation exposing (Message, MessageId)
import Data.Page as Page exposing (Page)
import Data.Post as PostId exposing (PostId)
import Http
import Json.Decode exposing (bool)
import Query.CacheQueryUtils exposing (fetchAndCacheUserInfo)
import Query.Json.ConversationDecoder exposing (decodeMessages)
import Query.Json.DecoderUtils exposing (jsonResolver, unitDecoder)
import Query.QueryUtils exposing (authHeader, baseUrl)
import Query.TaskUtils exposing (thread)
import State.Cache as Cache exposing (Cache)
import State.UserState exposing (UserInfo)
import Task exposing (Task)
import Update.Msg exposing (Msg(..))
import Url.Builder exposing (absolute, string)


fetchConversation: Cache -> UserInfo -> PostId -> Page -> Cmd Msg
fetchConversation cache user postId page = fetchConversationPage user postId page
    |> Task.andThen (\messages -> fetchAndCacheAllUsersFromMessages cache user messages |> thread messages )
    |> Task.andThen (\(cache1, messages) -> fetchAndCacheAllMessagesFlagged cache1 user messages |> thread messages )
    |> Task.andThen (\(cache2, messages) -> fetchAndCacheAllMessagesFlaggedByUser cache2 user messages |> thread messages )
    |> Task.map (\(cache3, messages) -> (cache3, {postId = postId, page = page, messages = messages}) )
    |> Task.attempt HttpConversationPageFetched

fetchConversationPage: UserInfo -> PostId -> Page -> Task Http.Error (List Message)
fetchConversationPage user postId page = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute [
        "conversation", "for-post",
        postId |> PostId.toString,
        page |> Page.number |> String.fromInt] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| decodeMessages
    , timeout = Nothing
 }

fetchAndCacheAllUsersFromMessages: Cache -> UserInfo -> List Message -> Task Http.Error Cache
fetchAndCacheAllUsersFromMessages cache user messages = messages
    |> Conversation.usersFromMessages
    |> List.map (\id -> fetchAndCacheUserInfo cache user id)
    |> Task.sequence
    |> Task.andThen (\xs -> List.foldl Cache.merge cache xs |> Task.succeed)

fetchAndCacheAllMessagesFlagged: Cache -> UserInfo -> List Message -> Task Http.Error Cache
fetchAndCacheAllMessagesFlagged cache user messages = messages
    |> List.map (\msg -> fetchMessageAndCacheFlagged cache user msg.id)
    |> Task.sequence
    |> Task.andThen (\xs -> List.foldl Cache.merge cache xs |> Task.succeed)

fetchMessageAndCacheFlagged: Cache -> UserInfo -> MessageId -> Task Http.Error Cache
fetchMessageAndCacheFlagged cache user messageId = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["conversation", "message","is-flagged", (messageId |> Conversation.toString)] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| bool
    , timeout = Nothing
 } |> Task.map (Cache.setFlagged cache messageId)

fetchAndCacheAllMessagesFlaggedByUser: Cache -> UserInfo -> List Message -> Task Http.Error Cache
fetchAndCacheAllMessagesFlaggedByUser cache user messages = messages
    |> List.map (\msg -> fetchMessageAndCacheFlaggedByUser cache user msg.id)
    |> Task.sequence
    |> Task.andThen (\xs -> List.foldl Cache.merge cache xs |> Task.succeed)

fetchMessageAndCacheFlaggedByUser: Cache -> UserInfo -> MessageId -> Task Http.Error Cache
fetchMessageAndCacheFlaggedByUser cache user messageId = Http.task {
    method = "GET"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["conversation", "has","user", "flagged", (messageId |> Conversation.toString)] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| bool
    , timeout = Nothing
 } |> Task.map (Cache.setFlaggedByUser cache messageId)


flagComment: UserInfo -> MessageId -> Cmd Msg
flagComment user messageId = Http.task {
    method = "POST"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["conversation", "flag", (messageId |> Conversation.toString)] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| unitDecoder
    , timeout = Nothing
 } |> Task.attempt HttpCommentFlagged

unflagComment: UserInfo -> MessageId -> Cmd Msg
unflagComment user messageId = Http.task {
    method = "POST"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["conversation", "unflag", (messageId |> Conversation.toString)] []
    , body = Http.emptyBody
    , resolver = jsonResolver <| unitDecoder
    , timeout = Nothing
 } |> Task.attempt HttpCommentUnflagged

postComment: UserInfo -> PostId -> String -> Cmd Msg
postComment user postId comment = Http.task {
    method = "POST"
    , headers = [authHeader user]
    , url = baseUrl ++ absolute ["conversation", "message"] [
        string "post-id" (postId |> PostId.toString),
        string "content" comment
    ]
    , body = Http.emptyBody
    , resolver = jsonResolver <| unitDecoder
    , timeout = Nothing
 } |> Task.map (\_ -> postId)
   |> Task.attempt HttpNewCommentPosted