module Internal.Api.SendMessageEvent.Api exposing (Phantom, sendMessageEvent)

{-|


# Send message event

This module helps send message events to rooms on the Matrix API.

@docs Phantom, sendMessageEvent

-}

import Internal.Api.Api as A
import Internal.Api.Request as R
import Internal.Config.Log exposing (log)
import Internal.Config.Text as Text
import Internal.Tools.Json as Json
import Internal.Values.Envelope as E


{-| Send a message event to the Matrix room.
-}
sendMessageEvent : SendMessageEventInput -> A.TaskChain (Phantom a) (Phantom a)
sendMessageEvent =
    A.startWithVersion "r0.0.0" sendMessageEventV1
        |> A.sameForVersion "r0.0.1"
        |> A.sameForVersion "r0.1.0"
        |> A.sameForVersion "r0.2.0"
        |> A.sameForVersion "r0.3.0"
        |> A.sameForVersion "r0.4.0"
        |> A.sameForVersion "r0.5.0"
        |> A.sameForVersion "r0.6.0"
        |> A.forVersion "r0.6.1" sendMessageEventV2
        |> A.forVersion "v1.1" sendMessageEventV3
        |> A.sameForVersion "v1.2"
        |> A.sameForVersion "v1.3"
        |> A.sameForVersion "v1.4"
        |> A.sameForVersion "v1.5"
        |> A.sameForVersion "v1.6"
        |> A.sameForVersion "v1.7"
        |> A.sameForVersion "v1.8"
        |> A.sameForVersion "v1.9"
        |> A.sameForVersion "v1.10"
        |> A.sameForVersion "v1.11"
        |> A.versionChain


{-| Context needed for sending a message event
-}
type alias Phantom a =
    { a | accessToken : (), baseUrl : (), versions : () }


type alias PhantomV1 a =
    { a | accessToken : (), baseUrl : () }


type alias SendMessageEventInput =
    { content : Json.Value
    , eventType : String
    , roomId : String
    , transactionId : String
    }


type alias SendMessageEventInputV1 a =
    { a
        | content : Json.Value
        , eventType : String
        , roomId : String
        , transactionId : String
    }


type alias SendMessageEventOutputV1 =
    { eventId : Maybe String }


type alias SendMessageEventOutputV2 =
    { eventId : String }


sendMessageEventV1 : SendMessageEventInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
sendMessageEventV1 { content, eventType, roomId, transactionId } =
    A.request
        { attributes = [ R.accessToken, R.fullBody content ]
        , coder = coderV1
        , contextChange = always identity
        , method = "PUT"
        , path = [ "_matrix", "client", "r0", "rooms", roomId, "send", eventType, transactionId ]
        , toUpdate =
            \out ->
                ( E.More []
                , out.eventId
                    |> Text.logs.sendEvent
                    |> log.debug
                    |> List.singleton
                )
        }


sendMessageEventV2 : SendMessageEventInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
sendMessageEventV2 { content, eventType, roomId, transactionId } =
    A.request
        { attributes = [ R.accessToken, R.fullBody content ]
        , coder = coderV2
        , contextChange = always identity
        , method = "PUT"
        , path = [ "_matrix", "client", "r0", "rooms", roomId, "send", eventType, transactionId ]
        , toUpdate =
            \out ->
                ( E.More []
                , out.eventId
                    |> Maybe.Just
                    |> Text.logs.sendEvent
                    |> log.debug
                    |> List.singleton
                )
        }


sendMessageEventV3 : SendMessageEventInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
sendMessageEventV3 { content, eventType, roomId, transactionId } =
    A.request
        { attributes = [ R.accessToken, R.fullBody content ]
        , coder = coderV2
        , contextChange = always identity
        , method = "PUT"
        , path = [ "_matrix", "client", "v3", "rooms", roomId, "send", eventType, transactionId ]
        , toUpdate =
            \out ->
                ( E.More []
                , out.eventId
                    |> Maybe.Just
                    |> Text.logs.sendEvent
                    |> log.debug
                    |> List.singleton
                )
        }


coderV1 : Json.Coder SendMessageEventOutputV1
coderV1 =
    Json.object1
        { name = "EventResponse"
        , description =
            [ "This endpoint is used to send a message event to a room. Message events allow access to historical events and pagination, making them suited for \"once-off\" activity in a room."
            , "The body of the request should be the content object of the event; the fields in this object will vary depending on the type of event."
            , "https://spec.matrix.org/legacy/r0.0.0/client_server.html#put-matrix-client-r0-rooms-roomid-send-eventtype-txnid"
            ]
        , init = SendMessageEventOutputV1
        }
        (Json.field.optional.value
            { fieldName = "event_id"
            , toField = .eventId
            , description = [ "A unique identifier for the event." ]
            , coder = Json.string
            }
        )


coderV2 : Json.Coder SendMessageEventOutputV2
coderV2 =
    Json.object1
        { name = "EventResponse"
        , description =
            [ "This endpoint is used to send a message event to a room. Message events allow access to historical events and pagination, making them suited for \"once-off\" activity in a room."
            , "The body of the request should be the content object of the event; the fields in this object will vary depending on the type of event."
            , "https://spec.matrix.org/legacy/client_server/r0.6.1.html#put-matrix-client-r0-rooms-roomid-send-eventtype-txnid"
            ]
        , init = SendMessageEventOutputV2
        }
        (Json.field.required
            { fieldName = "event_id"
            , toField = .eventId
            , description = [ "A unique identifier for the event." ]
            , coder = Json.string
            }
        )
