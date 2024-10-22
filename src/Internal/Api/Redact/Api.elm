module Internal.Api.Redact.Api exposing (..)

{-|


# Redact

This module allows the user to redact an event from a room.

-}

import Internal.Api.Api as A
import Internal.Api.Request as R
import Internal.Tools.Json as Json
import Internal.Values.Envelope as E


redact : RedactInput -> A.TaskChain (Phantom a) (Phantom a)
redact =
    A.startWithVersion "r0.0.0" redactV1
        |> A.sameForVersion "r0.0.1"
        |> A.forVersion "r0.1.0" redactV2
        |> A.sameForVersion "r0.2.0"
        |> A.sameForVersion "r0.3.0"
        |> A.sameForVersion "r0.4.0"
        |> A.sameForVersion "r0.5.0"
        |> A.sameForVersion "r0.6.0"
        |> A.sameForVersion "r0.6.1"
        |> A.forVersion "v1.1" redactV3
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
        |> A.sameForVersion "v1.12"
        |> A.versionChain


{-| Context needed for setting global account data.
-}
type alias Phantom a =
    { a | accessToken : (), baseUrl : (), versions : () }


type alias PhantomV1 a =
    { a | accessToken : (), baseUrl : () }


type alias RedactInput =
    { eventId : String
    , reason : Maybe String
    , roomId : String
    , transactionId : String
    }


type alias RedactInputV1 a =
    { a | eventId : String, reason : Maybe String, roomId : String, transactionId : String }


type alias RedactOutputV1 =
    { eventId : Maybe String }


redactV1 : RedactInputV1 i -> A.TaskChain (Phantom a) (Phantom a)
redactV1 { eventId, reason, roomId, transactionId } =
    A.request
        { attributes = [ R.accessToken, R.bodyOpString "reason" reason ]
        , coder = coderV1
        , contextChange = always identity
        , method = "PUT"
        , path = [ "_matrix", "client", "api", "r0", "rooms", roomId, "redact", eventId, transactionId ]
        , toUpdate = always ( E.Optional Nothing, [] )
        }


redactV2 : RedactInputV1 i -> A.TaskChain (Phantom a) (Phantom a)
redactV2 { eventId, reason, roomId, transactionId } =
    A.request
        { attributes = [ R.accessToken, R.bodyOpString "reason" reason ]
        , coder = coderV1
        , contextChange = always identity
        , method = "PUT"
        , path = [ "_matrix", "client", "r0", "rooms", roomId, "redact", eventId, transactionId ]
        , toUpdate = always ( E.Optional Nothing, [] )
        }


redactV3 : RedactInputV1 i -> A.TaskChain (Phantom a) (Phantom a)
redactV3 { eventId, reason, roomId, transactionId } =
    A.request
        { attributes = [ R.accessToken, R.bodyOpString "reason" reason ]
        , coder = coderV1
        , contextChange = always identity
        , method = "PUT"
        , path = [ "_matrix", "client", "v3", "rooms", roomId, "redact", eventId, transactionId ]
        , toUpdate = always ( E.Optional Nothing, [] )
        }


coderV1 : Json.Coder RedactOutputV1
coderV1 =
    Json.object1
        { name = "Redact Output"
        , description =
            [ "Event ID of the redaction event"
            ]
        , init = RedactOutputV1
        }
        (Json.field.optional.value
            { fieldName = "event_id"
            , toField = .eventId
            , description = [ "Event ID of the redaction event" ]
            , coder = Json.string
            }
        )
