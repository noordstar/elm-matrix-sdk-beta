module Matrix exposing
    ( Vault, fromUserId
    , VaultUpdate, update
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

@docs Vault, fromUserId


## Keeping the Vault up-to-date

@docs VaultUpdate, update


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


{-| Send a message event to a room.

This function can be used in a scenario where the user does not want to sync
the client, or is unable to. This function doesn't check whether the given room
exists and the user is able to send a message to, and instead just sends the
request to the Matrix API.

-}
sendMessageEvent : Vault -> { content : E.Value, eventType : String, roomId : String, toMsg : VaultUpdate -> msg, transactionId : String } -> Cmd msg
sendMessageEvent (Vault vault) data =
    Api.sendMessageEvent vault
        { content = data.content
        , eventType = data.eventType
        , roomId = data.roomId
        , toMsg = Types.VaultUpdate >> data.toMsg
        , transactionId = data.transactionId
        }


{-| Using new VaultUpdate information, update the Vault accordingly.

This allows us to change our perception of the Matrix environment: has anyone
sent a new message? Did someone send us an invite for a new room?

-}
update : VaultUpdate -> Vault -> Vault
update (VaultUpdate vu) (Vault vault) =
    vu.messages
        |> List.foldl (Envelope.update Internal.update) vault
        |> Vault
