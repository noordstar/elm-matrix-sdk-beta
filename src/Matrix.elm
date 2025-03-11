module Matrix exposing
    ( Vault, fromUserId, fromUsername
    , VaultUpdate, update, sync
    , rooms, fromRoomId, invites, fromInviteId
    , getAccountData, setAccountData
    , addAccessToken, leave, sendMessageEvent, whoAmI
    )

{-|


# Matrix SDK

This library forms a mere basis from which an entire functional SDK is
developed for the Matrix protocol.

It is generally quite unusual to regularly publish iterative beta versions on
the public registry, but it is also generally quite unusual to exclusively
support a monolithic public registry. (:


## Vault

@docs Vault, fromUserId, fromUsername


## Keeping the Vault up-to-date

@docs VaultUpdate, update, sync


## Exploring the Vault

@docs rooms, fromRoomId, invites, fromInviteId


## Account data

@docs getAccountData, setAccountData


## Debugging

@docs addAccessToken, leave, sendMessageEvent, whoAmI

-}

import Internal.Api.Main as Api
import Internal.Values.Envelope as Envelope
import Internal.Values.User as User
import Internal.Values.Vault as Internal
import Json.Encode as E
import Types exposing (Vault(..), VaultUpdate(..))


{-| The Vault type stores all relevant information about the Matrix API.

If you make sure that the data type stays up-to-date, you can use it to explore
the latest information about an account.

-}
type alias Vault =
    Types.Vault


{-| The VaultUpdate type is the central type that keeps the Vault up-to-date.
-}
type alias VaultUpdate =
    Types.VaultUpdate


{-| Adds a custom access token to the Vault. This can be done if no password is
provided or known.
-}
addAccessToken : String -> VaultUpdate
addAccessToken _ =
    Debug.todo "Add access token"


{-| Get a specific invite to a given room Id.
-}
fromInviteId : String -> Vault -> Maybe Types.Invite
fromInviteId roomId (Vault vault) =
    Envelope.mapMaybe (Internal.fromInviteId roomId) vault
        |> Maybe.map Types.Invite


{-| Get a room based on its room ID, if the user is a member of that room.
-}
fromRoomId : String -> Vault -> Maybe Types.Room
fromRoomId roomId (Vault vault) =
    Envelope.mapMaybe (Internal.fromRoomId roomId) vault
        |> Maybe.map Types.Room


{-| Get global account data.
-}
getAccountData : String -> Vault -> Maybe E.Value
getAccountData key (Vault vault) =
    Envelope.extract (Internal.getAccountData key) vault


{-| Use a fully-fledged Matrix ID to connect.

    case Matrix.fromUserId "@alice:example.org" of
        Just vault ->
            "We got a vault!"

        Nothing ->
            "Invalid username"

-}
fromUserId : String -> Maybe Vault
fromUserId uid =
    uid
        |> User.fromString
        |> Maybe.map
            (\u ->
                Envelope.init
                    { content = Internal.init
                    , serverName = "https://" ++ User.domain u
                    , user = Just u
                    }
                    |> Envelope.mapContext (\c -> { c | username = Just uid })
            )
        |> Maybe.map Vault


{-| Using a username and an address, create a Vault.

The username can either be the localpart or the full Matrix ID. For example,
you can either insert `alice` or `@alice:example.org`.

-}
fromUsername : { username : String, host : String, port_ : Maybe Int } -> Vault
fromUsername { username, host, port_ } =
    { content = Internal.init
    , serverName =
        port_
            |> Maybe.map String.fromInt
            |> Maybe.map ((++) ":")
            |> Maybe.withDefault ""
            |> (++) host
    , user = User.fromString username
    }
        |> Envelope.init
        |> Envelope.mapContext (\c -> { c | username = Just username })
        |> Vault


{-| Get a list of all invites that the user has received.
-}
invites : Vault -> List Types.Invite
invites (Vault vault) =
    Envelope.mapList Internal.invites vault
        |> List.map Types.Invite


{-| Leave a room. This stops a user from participating in a room.

If the user was already in the room, they will no longer be able to see new events in the room. If the room requires an invite to join, they will need to be re-invited before they can re-join.

If the user was invited to the room, but had not joined, this call serves to reject the invite.

The user will still be allowed to retrieve history from the room which they were previously allowed to see.

-}
leave :
    { reason : Maybe String
    , roomId : String
    }
    -> VaultUpdate
leave _ =
    Debug.todo "Leave a chosen room"


{-| Get a list of all the rooms that the user has joined.
-}
rooms : Vault -> List Types.Room
rooms (Vault vault) =
    Envelope.mapList Internal.rooms vault
        |> List.map Types.Room


{-| Send a message event to a room.

This function can be used in a scenario where the user does not want to sync
the client, or is unable to. This function doesn't check whether the given room
exists and the user is able to send a message to, and instead just sends the
request to the Matrix API.

The fields stand for the following:

  - `content` is the JSON object that is sent to the Matrix room.
  - `eventType` is the event type that is sent to the Matrix room.
  - `roomId` is the Matrix room ID.
  - `transactionId` is a unique identifier that helps the Matrix server
    distringuish messages. If you send the same message with the same transactionId,
    the server promises to register it only once.

-}
sendMessageEvent :
    { content : E.Value
    , eventType : String
    , roomId : String
    , transactionId : String
    }
    -> VaultUpdate
sendMessageEvent _ =
    Debug.todo "Send message event"


{-| Set global account data.
-}
setAccountData :
    { content : E.Value
    , eventType : String
    }
    -> VaultUpdate
setAccountData _ =
    Debug.todo "Set global account data"


{-| Synchronize the Vault with the Matrix API.

Effectively, this task asks the Matrix API to provide the latest information,
which will be returned as your VaultUpdate.

-}
sync : (VaultUpdate -> msg) -> Sub msg
sync _ =
    Debug.todo "Sync vault"


{-| Using new VaultUpdate information, update the Vault accordingly.

This allows us to change our perception of the Matrix environment: has anyone
sent a new message? Did someone send us an invite for a new room?

-}
update : VaultUpdate -> Vault -> ( Vault, Cmd VaultUpdate )
update (VaultUpdate vu) (Vault vault) =
    Debug.todo "Update vault"


{-| Get personal information about the vault, such as the user id, the device
id or other vital information.

When the vault logs in, it will automatically know this information. However,
if you plug in an access token, the vault might not know. If you wish to ensure
that such information is available, you can use this function to download that
information.

-}
whoAmI : (VaultUpdate -> msg) -> VaultUpdate
whoAmI _ =
    Debug.todo "Who am I?"
