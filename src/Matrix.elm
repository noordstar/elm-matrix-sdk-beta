module Matrix exposing
    ( Vault, fromUserId, fromUsername
    , VaultUpdate, update, sync, logs
    , rooms, fromRoomId
    , addAccessToken, sendMessageEvent
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

@docs VaultUpdate, update, sync, logs


## Exploring the Vault

@docs rooms, fromRoomId


## Debugging

@docs addAccessToken, sendMessageEvent

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
addAccessToken : String -> Vault -> Vault
addAccessToken token (Vault vault) =
    Envelope.mapContext (\c -> { c | suggestedAccessToken = Just token }) vault
        |> Vault


{-| Get a room based on its room ID, if the user is a member of that room.
-}
fromRoomId : String -> Vault -> Maybe Types.Room
fromRoomId roomId (Vault vault) =
    Envelope.mapMaybe (Internal.fromRoomId roomId) vault
        |> Maybe.map Types.Room


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
                    { serverName = "https://" ++ User.domain u
                    , content = Internal.init (Just u)
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
    { serverName =
        port_
            |> Maybe.map String.fromInt
            |> Maybe.map ((++) ":")
            |> Maybe.withDefault ""
            |> (++) host
    , content = Internal.init (User.fromString username)
    }
        |> Envelope.init
        |> Envelope.mapContext (\c -> { c | username = Just username })
        |> Vault


{-| Get a list of all the rooms that the user has joined.
-}
rooms : Vault -> List Types.Room
rooms (Vault vault) =
    Envelope.mapList Internal.rooms vault
        |> List.map Types.Room


{-| The VaultUpdate is a complex type that helps update the Vault. However,
it also contains a human output!

Using this function, you can get a human output that describes everything that
the VaultUpdate has to tell the Vault.

The `channel` field describes the context of the log, allowing you to filter
further. For example:

  - `debug` is a comprehensive channel describing everything the Elm runtime has
    executed.
  - `warn` contains warnings that aren't breaking, but relevant.
  - `securityWarn` warns about potential security issues or potential attacks.
  - `error` has errors that were encountered.
  - `caughtError` has errors that were dealt with successfully.

-}
logs : VaultUpdate -> List { channel : String, content : String }
logs (VaultUpdate vu) =
    vu.logs


{-| Send a message event to a room.

This function can be used in a scenario where the user does not want to sync
the client, or is unable to. This function doesn't check whether the given room
exists and the user is able to send a message to, and instead just sends the
request to the Matrix API.

The fields stand for the following:

  - `content` is the JSON object that is sent to the Matrix room.
  - `eventType` is the event type that is sent to the Matrix room.
  - `roomId` is the Matrix room ID.
  - `toMsg` is the `msg` type that is returned after the message has been sent.
  - `transactionId` is a unique identifier that helps the Matrix server
    distringuish messages. If you send the same message with the same transactionId,
    the server promises to register it only once.
  - `vault` is the Matrix Vault that contains all the latest and most relevant
    information.

-}
sendMessageEvent :
    { content : E.Value
    , eventType : String
    , roomId : String
    , toMsg : VaultUpdate -> msg
    , transactionId : String
    , vault : Vault
    }
    -> Cmd msg
sendMessageEvent data =
    case data.vault of
        Vault vault ->
            Api.sendMessageEvent vault
                { content = data.content
                , eventType = data.eventType
                , roomId = data.roomId
                , toMsg = Types.VaultUpdate >> data.toMsg
                , transactionId = data.transactionId
                }


{-| Synchronize the Vault with the Matrix API.

Effectively, this task asks the Matrix API to provide the latest information,
which will be returned as your VaultUpdate.

-}
sync : (VaultUpdate -> msg) -> Vault -> Cmd msg
sync toMsg (Vault vault) =
    Api.sync vault { toMsg = Types.VaultUpdate >> toMsg }


{-| Using new VaultUpdate information, update the Vault accordingly.

This allows us to change our perception of the Matrix environment: has anyone
sent a new message? Did someone send us an invite for a new room?

-}
update : VaultUpdate -> Vault -> Vault
update (VaultUpdate vu) (Vault vault) =
    vu.messages
        |> List.foldl (Envelope.update Internal.update) vault
        |> Vault
