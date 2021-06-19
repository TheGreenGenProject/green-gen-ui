module Query.Json.ConversationDecoder exposing (..)

import Data.Conversation exposing (Message, MessageId(..))
import Json.Decode exposing (Decoder, list, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Query.Json.DecoderUtils exposing (decodeTimestamp, decodeUserId, decodeUuid)


decodeMessageId: Decoder MessageId
decodeMessageId = succeed MessageId
    |> required "value" decodeUuid

decodeMessage: Decoder Message
decodeMessage = succeed Message
    |> required "id" decodeMessageId
    |> required "user" decodeUserId
    |> required "timestamp" decodeTimestamp
    |> required "content" string

decodeMessages: Decoder (List Message)
decodeMessages = list decodeMessage