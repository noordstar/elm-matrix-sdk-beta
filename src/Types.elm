module Types exposing (Vault(..), Event(..))

{-| The Elm SDK uses a lot of records and values that are easy to manipulate.
Yet, the [Elm design guidelines](https://package.elm-lang.org/help/design-guidelines#keep-tags-and-record-constructors-secret)
highly recommend using opaque types in order to avoid breaking everyone's code
in a future major release.

This module forms as a protective layer between the internal modules and the
exposed modules, hiding all exposed types behind opaque types so the user cannot
access their content directly.

The opaque types are placed in a central module so all exposed modules can
safely access all exposed data types without risking to create circular imports.

@docs Vault, Event

-}

import Internal.Values.Event as Event
import Internal.Values.Vault as Vault


{-| Opaque type for Matrix Event
-}
type Event
    = Event Event.Event


{-| Opaque type for Matrix Vault
-}
type Vault
    = Vault Vault.Vault
