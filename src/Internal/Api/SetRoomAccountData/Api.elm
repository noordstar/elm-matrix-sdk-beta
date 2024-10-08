module Internal.Api.SetRoomAccountData.Api exposing (..)

{-|


# Set Room Account Data

This module allows the developer to set account data to a Matrix room.

@docs Phantom, setRoomAccountData

-}

import Internal.Api.Api as A
import Internal.Api.Request as R
import Internal.Config.Log exposing (log)
import Internal.Config.Text as Text
import Internal.Tools.Json as Json
import Internal.Values.Envelope as E
import Internal.Values.Room as R
import Internal.Values.Vault as V


{-| Set account data to a Matrix room.
-}
setRoomAccountData : SetRoomAccountDataInput -> A.TaskChain (Phantom a) (Phantom a)
setRoomAccountData =
    A.startWithVersion "r0.0.0" setRoomAccountDataV1
        |> A.sameForVersion "r0.0.1"
        |> A.sameForVersion "r0.1.0"
        |> A.sameForVersion "r0.2.0"
        |> A.sameForVersion "r0.3.0"
        |> A.sameForVersion "r0.4.0"
        |> A.sameForVersion "r0.5.0"
        |> A.sameForVersion "r0.6.0"
        |> A.sameForVersion "r0.6.1"
        |> A.forVersion "v1.1" setRoomAccountDataV2
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


{-| Context needed for setting account data on a room.
-}
type alias Phantom a =
    { a | accessToken : (), baseUrl : (), versions : () }


type alias PhantomV1 a =
    { a | accessToken : (), baseUrl : () }


type alias SetRoomAccountDataInput =
    { content : Json.Value, eventType : String, roomId : String, userId : String }


type alias SetRoomAccountDataInputV1 a =
    { a | content : Json.Value, eventType : String, roomId : String, userId : String }


type alias SetRoomAccountDataOutputV1 =
    ()


setRoomAccountDataV1 : SetRoomAccountDataInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
setRoomAccountDataV1 { content, eventType, roomId, userId } =
    A.request
        { attributes = [ R.accessToken, R.fullBody content ]
        , coder = coderV1
        , contextChange = always identity
        , method = "PUT"
        , path = [ "_matrix", "client", "r0", "user", userId, "rooms", roomId, "account_data", eventType ]
        , toUpdate =
            \() ->
                ( R.SetAccountData eventType content
                    |> V.MapRoom roomId
                    |> E.ContentUpdate
                , []
                )
        }


setRoomAccountDataV2 : SetRoomAccountDataInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
setRoomAccountDataV2 { content, eventType, roomId, userId } =
    A.request
        { attributes = [ R.accessToken, R.fullBody content ]
        , coder = coderV1
        , contextChange = always identity
        , method = "PUT"
        , path = [ "_matrix", "client", "v3", "user", userId, "rooms", roomId, "account_data", eventType ]
        , toUpdate =
            \() ->
                ( R.SetAccountData eventType content
                    |> V.MapRoom roomId
                    |> E.ContentUpdate
                , []
                )
        }


coderV1 : Json.Coder SetRoomAccountDataOutputV1
coderV1 =
    Json.unit
