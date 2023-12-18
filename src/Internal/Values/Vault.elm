module Internal.Values.Vault exposing (Vault)

{-| This module hosts the Vault module.

@docs Vault

-}

import Internal.Values.Envelope as Envelope


{-| This is the Vault type.
-}
type alias Vault =
    Envelope.Envelope {}
