module Internal.Api.SendMessageEvent.Api exposing (..)

{-|


# Send message event

This module helps send message events to rooms on the Matrix API.

@docs Phantom

-}

import Internal.Api.Api as A
import Internal.Api.Request as R
import Internal.Config.Leaks as L
import Internal.Config.Log exposing (log)
import Internal.Tools.Json as Json
import Internal.Values.Envelope as E


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


type alias Phantom a =
    a


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
        { attributes = [ R.fullBody content ]
        , coder = coderV1
        , contextChange = always identity
        , method = "PUT"
        , path = [ "_matrix", "client", "r0", "rooms", roomId, "send", eventType, transactionId ]
        , toUpdate =
            \out ->
                ( E.More []
                , out.eventId
                    |> Maybe.map ((++) ", received event id ")
                    |> Maybe.withDefault ""
                    |> (++) "Sent event"
                    |> log.debug
                    |> List.singleton
                )
        }


sendMessageEventV2 : SendMessageEventInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
sendMessageEventV2 { content, eventType, roomId, transactionId } =
    A.request
        { attributes = [ R.fullBody content ]
        , coder = coderV2
        , contextChange = always identity
        , method = "PUT"
        , path = [ "_matrix", "client", "r0", "rooms", roomId, "send", eventType, transactionId ]
        , toUpdate =
            \out ->
                ( E.More []
                , out.eventId
                    |> (++) "Sent event, received event id "
                    |> log.debug
                    |> List.singleton
                )
        }


sendMessageEventV3 : SendMessageEventInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
sendMessageEventV3 { content, eventType, roomId, transactionId } =
    A.request
        { attributes = [ R.fullBody content ]
        , coder = coderV2
        , contextChange = always identity
        , method = "PUT"
        , path = [ "_matrix", "client", "v3", "rooms", roomId, "send", eventType, transactionId ]
        , toUpdate =
            \out ->
                ( E.More []
                , out.eventId
                    |> (++) "Sent event, received event id "
                    |> log.debug
                    |> List.singleton
                )
        }


coderV1 : Json.Coder SendMessageEventOutputV1
coderV1 =
    Json.object2
        { name = "EventResponse"
        , description =
            [ "This endpoint is used to send a message event to a room. Message events allow access to historical events and pagination, making them suited for \"once-off\" activity in a room."
            , "The body of the request should be the content object of the event; the fields in this object will vary depending on the type of event."
            , "https://spec.matrix.org/legacy/r0.0.0/client_server.html#put-matrix-client-r0-rooms-roomid-send-eventtype-txnid"
            ]
        , init = always SendMessageEventOutputV1
        }
        (Json.field.optional.value
            { fieldName = L.field
            , toField = always Nothing
            , description =
                [ "The Elm SDK always expects objects to have at least two fields."
                , "Otherwise, what's the point of hiding the value in an object?"
                , "For this reason, this empty placeholder key will always be ignored."
                ]
            , coder = Json.value
            }
        )
        (Json.field.optional.value
            { fieldName = "event_id"
            , toField = .eventId
            , description = Debug.todo "Needs docs"
            , coder = Json.string
            }
        )


coderV2 : Json.Coder SendMessageEventOutputV2
coderV2 =
    Json.object2
        { name = "EventResponse"
        , description =
            [ "This endpoint is used to send a message event to a room. Message events allow access to historical events and pagination, making them suited for \"once-off\" activity in a room."
            , "The body of the request should be the content object of the event; the fields in this object will vary depending on the type of event."
            , "https://spec.matrix.org/legacy/r0.0.0/client_server.html#put-matrix-client-r0-rooms-roomid-send-eventtype-txnid"
            ]
        , init = always SendMessageEventOutputV2
        }
        (Json.field.optional.value
            { fieldName = L.field
            , toField = always Nothing
            , description =
                [ "The Elm SDK always expects objects to have at least two fields."
                , "Otherwise, what's the point of hiding the value in an object?"
                , "For this reason, this empty placeholder key will always be ignored."
                ]
            , coder = Json.value
            }
        )
        (Json.field.required
            { fieldName = "event_id"
            , toField = .eventId
            , description = Debug.todo "Needs docs"
            , coder = Json.string
            }
        )
