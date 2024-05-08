module Matrix exposing (Vault)

{-|


# Matrix SDK

This first version forms a mere basis from which we will create iterative builds
that slowly improve the codebase.

It is generally quite unusual to regularly publish iterative beta versions on
the public registry, but it is also generally quite unusual to exclusively
support a monolithic public registry. (:


## Vault

@docs Vault

-}

import Types


{-| The Vault type stores all relevant information about the Matrix API.

If you make sure that the data type stays up-to-date, you can use it to explore
the latest information about an account.

-}
type alias Vault =
    Types.Vault
