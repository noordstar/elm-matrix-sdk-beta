module Matrix exposing
    ( Vault
    , VaultUpdate, update
    )

{-|


# Matrix SDK

This library forms a mere basis from which an entire functional SDK is
developed for the Matrix protocol.

It is generally quite unusual to regularly publish iterative beta versions on
the public registry, but it is also generally quite unusual to exclusively
support a monolithic public registry. (:


## Vault

@docs Vault


## Keeping the Vault up-to-date

@docs VaultUpdate, update

-}

import Internal.Values.Envelope as Envelope
import Internal.Values.Vault as Internal
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


{-| Using new VaultUpdate information, update the Vault accordingly.

This allows us to change our perception of the Matrix environment: has anyone
sent a new message? Did someone send us an invite for a new room?

-}
update : VaultUpdate -> Vault -> Vault
update (VaultUpdate vu) (Vault vault) =
    vault
        |> Envelope.update Internal.update vu
        |> Vault
