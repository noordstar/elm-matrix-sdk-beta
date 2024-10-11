module Internal.Api.KickUser.Api exposing (Phantom, kickUser)

{-|


# Kick user

This module helps to kick users from a room.

@docs Phantom, kickUser

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


kickUser : KickUserInput -> A.TaskChain (Phantom a) (Phantom a)
kickUser =
    A.startWithVersion "r0.0.0" kickUserV1
        |> A.sameForVersion "r0.0.1"
        -- NOTE: Kicking a user was first added in r0.1.0
        |> A.forVersion "r0.1.0" kickUserV2
        |> A.sameForVersion "r0.2.0"
        |> A.sameForVersion "r0.2.0"
        |> A.sameForVersion "r0.3.0"
        |> A.sameForVersion "r0.4.0"
        |> A.sameForVersion "r0.5.0"
        |> A.sameForVersion "r0.6.0"
        |> A.sameForVersion "r0.6.1"
        |> A.forVersion "v1.1" kickUserV3
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


type alias Phantom a =
    { a | accessToken : (), baseUrl : (), versions : () }


type alias PhantomV1 a =
    { a | accessToken : (), baseUrl : () }


type alias KickUserInput =
    { avatarUrl : Maybe String
    , displayname : Maybe String
    , reason : Maybe String
    , roomId : String
    , user : User
    }


type alias KickUserInputV1 a =
    { a
        | avatarUrl : Maybe String
        , displayname : Maybe String
        , reason : Maybe String
        , roomId : String
        , user : User
    }


type alias KickUserInputV2 a =
    { a | reason : Maybe String, roomId : String, user : User }


type alias KickUserOutputV1 =
    { eventId : Maybe String }


type alias KickUserOutputV2 =
    ()


kickUserV1 : KickUserInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
kickUserV1 { avatarUrl, displayname, reason, roomId, user } =
    A.request
        { attributes =
            [ R.accessToken
            , R.bodyString "membership" "kick"
            , R.bodyOpString "avatar_url" avatarUrl
            , R.bodyOpString "displayname" displayname
            , R.bodyOpString "reason" reason
            ]
        , coder = coderV1
        , contextChange = always identity
        , method = "PUT"
        , path = [ "_matrix", "client", "r0", "rooms", roomId, "state", "m.room.member", User.toString user ]
        , toUpdate =
            \out ->
                ( E.More []
                , [ "The kick API endpoint does not exist before spec version r0.1.0 - falling back to sending state event directly."
                        |> log.debug
                  , out.eventId
                        |> Text.logs.sendEvent
                        |> log.debug
                  ]
                )
        }


kickUserV2 : KickUserInputV2 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
kickUserV2 { reason, roomId, user } =
    A.request
        { attributes =
            [ R.accessToken
            , R.bodyOpString "reason" reason
            , R.bodyString "user_id" (User.toString user)
            ]
        , coder = coderV2
        , contextChange = always identity
        , method = "POST"
        , path = [ "_matrix", "client", "r0", "rooms", roomId, "kick" ]
        , toUpdate =
            \() ->
                ( E.More []
                , []
                )
        }


kickUserV3 : KickUserInputV2 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
kickUserV3 { reason, roomId, user } =
    A.request
        { attributes =
            [ R.accessToken
            , R.bodyOpString "reason" reason
            , R.bodyString "user_id" (User.toString user)
            ]
        , coder = coderV2
        , contextChange = always identity
        , method = "POST"
        , path = [ "_matrix", "client", "v3", "rooms", roomId, "kick" ]
        , toUpdate =
            \() ->
                ( E.More []
                , []
                )
        }


coderV1 : Json.Coder KickUserOutputV1
coderV1 =
    Json.object1
        { name = "EventResponse"
        , description =
            [ "This object is returned after a state event has been sent."
            ]
        , init = KickUserOutputV1
        }
        (Json.field.optional.value
            { fieldName = "event_id"
            , toField = .eventId
            , description = [ "A unique identifier for the event." ]
            , coder = Json.string
            }
        )


coderV2 : Json.Coder KickUserOutputV2
coderV2 =
    Json.unit
