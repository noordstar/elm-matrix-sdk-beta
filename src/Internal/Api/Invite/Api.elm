module Internal.Api.Invite.Api exposing (InviteInput, Phantom, invite)

{-|


# Invite

This API invites a user to participate in a particular room. They do not start
participating in the room until they actually join the room.

Only users currently in a particular room can invite other users to join that
room.

If the user was invited to the room, the homeserver will append a m.room.member
event to the room.

@docs InviteInput, Phantom, invite

-}

import Internal.Api.Api as A
import Internal.Api.Request as R
import Internal.Config.Log exposing (log)
import Internal.Tools.Json as Json
import Internal.Values.Envelope as E
import Internal.Values.Room as Room
import Internal.Values.User as User exposing (User)
import Internal.Values.Vault as V


{-| Invite a user to a room.
-}
invite : InviteInput -> A.TaskChain (Phantom ph1) (Phantom ph1)
invite =
    A.startWithVersion "r0.0.0" inviteV1
        |> A.sameForVersion "r0.0.1"
        |> A.sameForVersion "r0.1.0"
        |> A.sameForVersion "r0.2.0"
        |> A.sameForVersion "r0.3.0"
        |> A.sameForVersion "r0.4.0"
        |> A.sameForVersion "r0.5.0"
        |> A.sameForVersion "r0.6.0"
        |> A.sameForVersion "r0.6.1"
        |> A.forVersion "v1.1" inviteV2
        |> A.sameForVersion "v1.2"
        |> A.sameForVersion "v1.3"
        |> A.sameForVersion "v1.4"
        |> A.sameForVersion "v1.5"
        |> A.sameForVersion "v1.6"
        |> A.sameForVersion "v1.7"
        |> A.sameForVersion "v1.8"
        |> A.sameForVersion "v1.9"
        |> A.sameForVersion "v1.10"
        |> A.versionChain


{-| Context needed for inviting a user.
-}
type alias Phantom a =
    { a | accessToken : (), versions : () }


type alias PhantomV1 a =
    { a | accessToken : () }


{-| Input for inviting a user.
-}
type alias InviteInput =
    { reason : Maybe String, roomId : String, user : User }


type alias InviteInputV1 a =
    { a | roomId : String, user : User }


type alias InviteInputV2 a =
    { a | roomId : String, user : User, reason : Maybe String }


inviteV1 : InviteInputV1 a -> A.TaskChain (PhantomV1 ph1) (PhantomV1 ph1)
inviteV1 { roomId, user } =
    A.request
        { attributes =
            [ R.accessToken
            , R.bodyString "user_id" (User.toString user)
            , R.onStatusCode 400 "M_UNKNOWN"
            , R.onStatusCode 403 "M_FORBIDDEN"
            , R.onStatusCode 429 "M_LIMIT_EXCEEDED"
            ]
        , coder = Json.value
        , contextChange = always identity
        , method = "POST"
        , path = [ "_matrix", "client", "r0", "rooms", roomId, "invite" ]
        , toUpdate =
            always
                ( E.ContentUpdate <| V.MapRoom roomId (Room.Invite user)
                , String.concat
                    -- TODO: Move to Internal.Config.Text
                    [ "Invited user "
                    , User.toString user
                    , " to room "
                    , roomId
                    ]
                    |> log.debug
                    |> List.singleton
                )
        }


inviteV2 : InviteInputV2 a -> A.TaskChain (PhantomV1 ph1) (PhantomV1 ph1)
inviteV2 { reason, roomId, user } =
    A.request
        { attributes =
            [ R.bodyOpString "reason" reason
            , R.bodyString "user_id" (User.toString user)
            , R.onStatusCode 400 "M_UNKNOWN"
            , R.onStatusCode 403 "M_FORBIDDEN"
            , R.onStatusCode 429 "M_LIMIT_EXCEEDED"
            ]
        , coder = Json.value
        , contextChange = always identity
        , method = "POST"
        , path = [ "_matrix", "client", "v3", "rooms", roomId, "invite" ]
        , toUpdate =
            always
                ( E.ContentUpdate <| V.MapRoom roomId (Room.Invite user)
                , String.concat
                    -- TODO: Move to Internal.Config.Text
                    [ "Invited user "
                    , User.toString user
                    , " to room "
                    , roomId
                    ]
                    |> log.debug
                    |> List.singleton
                )
        }
