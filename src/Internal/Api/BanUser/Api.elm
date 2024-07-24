module Internal.Api.BanUser.Api exposing (Phantom, banUser)

{-|

# Ban user

This module helps to ban users from a room.

@docs Phantom, banUser

-}

import Internal.Api.Api as A
import Internal.Api.Request as R
import Internal.Config.Log exposing (log)
import Internal.Config.Text as Text
import Internal.Tools.Json as Json
import Internal.Values.Envelope as E
import Internal.Values.Room as R
import Internal.Values.User as User exposing (User)
import Internal.Values.Vault as V


banUser : BanUserInput -> A.TaskChain (Phantom a) (Phantom a)
banUser =
    A.startWithVersion "r0.0.0" banUserV1
        |> A.sameForVersion "r0.0.1"
        |> A.sameForVersion "r0.1.0"
        |> A.sameForVersion "r0.2.0"
        |> A.sameForVersion "r0.2.0"
        |> A.sameForVersion "r0.3.0"
        |> A.sameForVersion "r0.4.0"
        |> A.sameForVersion "r0.5.0"
        |> A.sameForVersion "r0.6.0"
        |> A.sameForVersion "r0.6.1"
        |> A.forVersion "v1.1" banUserV2
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
    { a | accessToken : (), baseUrl : (), versions : () }


type alias PhantomV1 a =
    { a | accessToken : (), baseUrl : () }


type alias BanUserInput =
    { reason : Maybe String
    , roomId : String
    , user : User
    }


type alias BanUserInputV1 a =
    { a | reason : Maybe String, roomId : String, user : User }



type alias BanUserOutputV1 =
    ()


banUserV1 : BanUserInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
banUserV1 { reason, roomId, user } =
    A.request
        { attributes =
            [ R.accessToken
            , R.bodyOpString "reason" reason
            , R.bodyString "user_id" (User.toString user)
            ]
        , coder = coderV1
        , contextChange = always identity
        , method = "POST"
        , path = [ "_matrix", "client", "r0", "rooms", roomId, "ban" ]
        , toUpdate =
            \() ->
                ( E.More []
                , []
                )
        }


banUserV2 : BanUserInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
banUserV2 { reason, roomId, user } =
    A.request
        { attributes =
            [ R.accessToken
            , R.bodyOpString "reason" reason
            , R.bodyString "user_id" (User.toString user)
            ]
        , coder = coderV1
        , contextChange = always identity
        , method = "POST"
        , path = [ "_matrix", "client", "v3", "rooms", roomId, "ban" ]
        , toUpdate =
            \() ->
                ( E.More []
                , []
                )
        }


coderV1 : Json.Coder BanUserOutputV1
coderV1 =
    Json.unit
