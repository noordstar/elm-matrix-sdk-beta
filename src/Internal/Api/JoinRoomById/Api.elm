module Internal.Api.JoinRoomById.Api exposing (..)

{-| This module allows you to join a room by its id.
-}

import Internal.Api.Api as A
import Internal.Api.Request as R
import Internal.Tools.Json as Json
import Internal.Values.Envelope as E


joinRoomById : JoinRoomByIdInput -> A.TaskChain (Phantom a) (Phantom a)
joinRoomById =
    A.startWithVersion "r0.0.0" joinRoomByIdV1
        |> A.sameForVersion "r0.0.1"
        |> A.sameForVersion "r0.1.0"
        |> A.sameForVersion "r0.2.0"
        |> A.sameForVersion "r0.3.0"
        |> A.forVersion "r0.4.0" joinRoomByIdV2
        |> A.sameForVersion "r0.5.0"
        |> A.sameForVersion "r0.6.0"
        |> A.sameForVersion "r0.6.1"
        |> A.forVersion "v1.1" joinRoomByIdV3
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


type alias JoinRoomByIdInput =
    { reason : Maybe String, roomId : String }


type alias JoinRoomByIdInputV1 a =
    { a | roomId : String }


type alias JoinRoomByIdInputV2 a =
    { a | reason : Maybe String, roomId : String }


type alias JoinRoomByIdOutputV1 =
    { roomId : String }


joinRoomByIdV1 : JoinRoomByIdInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
joinRoomByIdV1 { roomId } =
    A.request
        { attributes = [ R.accessToken, R.onStatusCode 403 "M_FORBIDDEN" ]
        , coder = coderV1
        , contextChange = always identity
        , method = "POST"
        , path = [ "_matrix", "client", "r0", "rooms", roomId, "join" ]
        , toUpdate = always ( E.Optional Nothing, [] )
        }


joinRoomByIdV2 : JoinRoomByIdInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
joinRoomByIdV2 { roomId } =
    A.request
        { attributes =
            [ R.accessToken
            , R.onStatusCode 403 "M_FORBIDDEN"
            , R.onStatusCode 429 "M_LIMIT_EXCEEDED"
            ]
        , coder = coderV1
        , contextChange = always identity
        , method = "POST"
        , path = [ "_matrix", "client", "r0", "rooms", roomId, "join" ]
        , toUpdate = always ( E.Optional Nothing, [] )
        }


joinRoomByIdV3 : JoinRoomByIdInputV2 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
joinRoomByIdV3 { reason, roomId } =
    A.request
        { attributes =
            [ R.accessToken
            , R.bodyOpString "reason" reason
            , R.onStatusCode 403 "M_FORBIDDEN"
            , R.onStatusCode 429 "M_LIMIT_EXCEEDED"
            ]
        , coder = coderV1
        , contextChange = always identity
        , method = "POST"
        , path = [ "_matrix", "client", "v3", "rooms", roomId, "join" ]
        , toUpdate = always ( E.Optional Nothing, [] )
        }


coderV1 : Json.Coder JoinRoomByIdOutputV1
coderV1 =
    Json.object1
        { name = "Join Room By Id Output"
        , description =
            [ "Response returned by the homeserver to confirm that one has successfully joined a given room."
            ]
        , init = JoinRoomByIdOutputV1
        }
        (Json.field.required
            { fieldName = "room_id"
            , toField = .roomId
            , description = [ "The joined room ID." ]
            , coder = Json.string
            }
        )
