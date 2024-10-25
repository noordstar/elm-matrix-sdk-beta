module Internal.Api.Leave.Api exposing (..)

{-|


# Leave

This module allows the user to leave a room.

-}

import Internal.Api.Api as A
import Internal.Api.Request as R
import Internal.Tools.Json as Json
import Internal.Values.Envelope as E
import Internal.Values.Vault as V


leave : LeaveInput -> A.TaskChain (Phantom a) (Phantom a)
leave =
    A.startWithVersion "r0.0.0" leaveV1
        |> A.sameForVersion "r0.0.1"
        |> A.sameForVersion "r0.1.0"
        |> A.sameForVersion "r0.2.0"
        |> A.sameForVersion "r0.3.0"
        |> A.forVersion "r0.4.0" leaveV2
        |> A.sameForVersion "r0.5.0"
        |> A.sameForVersion "r0.6.0"
        |> A.sameForVersion "r0.6.1"
        |> A.forVersion "v1.1" leaveV3
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


type alias LeaveInput =
    { reason : Maybe String, roomId : String }


type alias LeaveInputV1 a =
    { a | roomId : String }


type alias LeaveInputV2 a =
    { a | reason : Maybe String, roomId : String }


type alias LeaveOutputV1 =
    ()


leaveV1 : LeaveInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
leaveV1 { roomId } =
    A.request
        { attributes = [ R.accessToken ]
        , coder = coderV1
        , contextChange = always identity
        , method = "POST"
        , path = [ "_matrix", "client", "r0", "rooms", roomId, "leave" ]
        , toUpdate = always ( E.ContentUpdate (V.RemoveInvite roomId), [] )
        }


leaveV2 : LeaveInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
leaveV2 { roomId } =
    A.request
        { attributes = [ R.accessToken, R.onStatusCode 429 "M_LIMIT_EXCEEDED" ]
        , coder = coderV1
        , contextChange = always identity
        , method = "POST"
        , path = [ "_matrix", "client", "r0", "rooms", roomId, "leave" ]
        , toUpdate = always ( E.ContentUpdate (V.RemoveInvite roomId), [] )
        }


leaveV3 : LeaveInputV2 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
leaveV3 { reason, roomId } =
    A.request
        { attributes =
            [ R.accessToken
            , R.bodyOpString "reason" reason
            , R.onStatusCode 429 "M_LIMIT_EXCEEDED"
            ]
        , coder = coderV1
        , contextChange = always identity
        , method = "POST"
        , path = [ "_matrix", "client", "v3", "rooms", roomId, "leave" ]
        , toUpdate = always ( E.ContentUpdate (V.RemoveInvite roomId), [] )
        }


coderV1 : Json.Coder LeaveOutputV1
coderV1 =
    Json.unit
