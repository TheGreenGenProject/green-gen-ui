module Data.Conversation exposing (..)

import Data.Page exposing (Page)
import Data.Post exposing (PostId)
import Data.Schedule exposing (UTCTimestamp)
import Data.User exposing (UserId)
import Utils.ListUtils as ListUtils
import Uuid exposing (Uuid)


type MessageId = MessageId Uuid
type alias Message = {
    id: MessageId,
    author: UserId,
    timestamp: UTCTimestamp,
    content: String
 }

type alias Conversation = {
    postId: PostId,
    messageCount: Int,
    page: Page,
    messages: List Message
 }

type alias ConversationPage = {
    postId: PostId,
    page: Page,
    messages: List Message
 }

usersFromMessages: List Message -> List UserId
usersFromMessages messages = messages
    |> List.map (.author)
    |> ListUtils.unique

-- This needs to match the server configuration
pageSize: Int
pageSize = 10

toString: MessageId -> String
toString (MessageId uuid) = uuid |> Uuid.toString

fromString: String -> Maybe MessageId
fromString = Maybe.map MessageId << Uuid.fromString
