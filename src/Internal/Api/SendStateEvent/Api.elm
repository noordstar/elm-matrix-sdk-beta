module Internal.Api.SendStateEvent.Api exposing (..)

{-|


# Send state event

This module sends state events to Matrix rooms.

@docs Phantom, sendStateEvent

-}

import Internal.Api.Api as A
import Internal.Api.Request as R
import Internal.Config.Log exposing (log)
import Internal.Config.Text as Text
import Internal.Tools.Json as Json
import Internal.Values.Envelope as E


{-| Send a state event to a Matrix room.
-}
sendStateEvent : SendStateEventInput -> A.TaskChain (Phantom a) (Phantom a)
sendStateEvent =
    A.startWithVersion "r0.0.0" sendStateEventV1
        |> A.sameForVersion "r0.0.1"
        |> A.sameForVersion "r0.1.0"
        |> A.sameForVersion "r0.2.0"
        |> A.sameForVersion "r0.3.0"
        |> A.sameForVersion "r0.4.0"
        |> A.sameForVersion "r0.5.0"
        |> A.sameForVersion "r0.6.0"
        |> A.forVersion "r0.6.1" sendStateEventV2
        |> A.forVersion "v1.1" sendStateEventV3
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


{-| Context needed for sending a state event
-}
type alias Phantom a =
    { a | accessToken : (), baseUrl : (), versions : () }


type alias PhantomV1 a =
    { a | accessToken : (), baseUrl : () }


type alias SendStateEventInput =
    { content : Json.Value
    , eventType : String
    , roomId : String
    , stateKey : String
    }


type alias SendStateEventInputV1 a =
    { a
        | content : Json.Value
        , eventType : String
        , roomId : String
        , stateKey : String
    }


type alias SendStateEventOutputV1 =
    { eventId : Maybe String }


type alias SendStateEventOutputV2 =
    { eventId : String }


sendStateEventV1 : SendStateEventInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
sendStateEventV1 { content, eventType, roomId, stateKey } =
    A.request
        { attributes = [ R.accessToken, R.fullBody content ]
        , coder = coderV1
        , contextChange = always identity
        , method = "PUT"
        , path = [ "_matrix", "client", "r0", "rooms", roomId, "state", eventType, stateKey ]
        , toUpdate =
            \out ->
                ( E.More []
                , out.eventId
                    |> Text.logs.sendEvent
                    |> log.debug
                    |> List.singleton
                )
        }


sendStateEventV2 : SendStateEventInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
sendStateEventV2 { content, eventType, roomId, stateKey } =
    A.request
        { attributes = [ R.accessToken, R.fullBody content ]
        , coder = coderV2
        , contextChange = always identity
        , method = "PUT"
        , path = [ "_matrix", "client", "r0", "rooms", roomId, "state", eventType, stateKey ]
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


sendStateEventV3 : SendStateEventInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
sendStateEventV3 { content, eventType, roomId, stateKey } =
    A.request
        { attributes = [ R.accessToken, R.fullBody content ]
        , coder = coderV2
        , contextChange = always identity
        , method = "PUT"
        , path = [ "_matrix", "client", "v3", "rooms", roomId, "state", eventType, stateKey ]
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


coderV1 : Json.Coder SendStateEventOutputV1
coderV1 =
    Json.object1
        { name = "EventResponse"
        , description =
            [ "This object is returned after a state event has been sent."
            ]
        , init = SendStateEventOutputV1
        }
        (Json.field.optional.value
            { fieldName = "event_id"
            , toField = .eventId
            , description = [ "A unique identifier for the event." ]
            , coder = Json.string
            }
        )


coderV2 : Json.Coder SendStateEventOutputV2
coderV2 =
    Json.object1
        { name = "EventResponse"
        , description =
            [ "This object is returned after a state event has been sent."
            ]
        , init = SendStateEventOutputV2
        }
        (Json.field.required
            { fieldName = "event_id"
            , toField = .eventId
            , description = [ "A unique identifier for the event." ]
            , coder = Json.string
            }
        )
