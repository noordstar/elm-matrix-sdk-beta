module Internal.Api.SetAccountData.Api exposing (Phantom, setAccountData)

{-|


# Set Account Data

This module allows the developer to set global account data.

@docs Phantom, setAccountData

-}

import Internal.Api.Api as A
import Internal.Api.Request as R
import Internal.Tools.Json as Json
import Internal.Values.Envelope as E
import Internal.Values.Room as R
import Internal.Values.Vault as V


setAccountData : SetAccountDataInput -> A.TaskChain (Phantom a) (Phantom a)
setAccountData =
    A.startWithVersion "r0.0.0" setAccountDataV1
        |> A.sameForVersion "r0.0.1"
        |> A.sameForVersion "r0.1.0"
        |> A.sameForVersion "r0.2.0"
        |> A.sameForVersion "r0.3.0"
        |> A.sameForVersion "r0.4.0"
        |> A.sameForVersion "r0.5.0"
        |> A.sameForVersion "r0.6.0"
        |> A.sameForVersion "r0.6.1"
        |> A.forVersion "v1.1" setAccountDataV2
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


type alias SetAccountDataInput =
    { content : Json.Value, eventType : String, userId : String }


type alias SetAccountDataInputV1 a =
    { a | content : Json.Value, eventType : String, userId : String }


type alias SetAccountDataOutput =
    ()


setAccountDataV1 : SetAccountDataInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
setAccountDataV1 { content, eventType, userId } =
    A.request
        { attributes = [ R.accessToken, R.fullBody content ]
        , coder = coderV1
        , contextChange = always identity
        , method = "PUT"
        , path = [ "_matrix", "client", "r0", "user", userId, "account_data", eventType ]
        , toUpdate =
            \() ->
                ( V.SetAccountData eventType content
                    |> E.ContentUpdate
                , []
                )
        }


setAccountDataV2 : SetAccountDataInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
setAccountDataV2 { content, eventType, userId } =
    A.request
        { attributes = [ R.accessToken, R.fullBody content ]
        , coder = coderV1
        , contextChange = always identity
        , method = "PUT"
        , path = [ "_matrix", "client", "v3", "user", userId, "account_data", eventType ]
        , toUpdate =
            \() ->
                ( V.SetAccountData eventType content
                    |> E.ContentUpdate
                , []
                )
        }


coderV1 : Json.Coder SetAccountDataOutput
coderV1 =
    Json.unit
