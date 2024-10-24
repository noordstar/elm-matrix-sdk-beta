module Types exposing
    ( Vault(..), Event(..), Invite(..), Room(..), User(..), VaultUpdate(..)
    , InviteEvent(..)
    )

{-| The Elm SDK uses a lot of records and values that are easy to manipulate.
Yet, the [Elm design guidelines](https://package.elm-lang.org/help/design-guidelines#keep-tags-and-record-constructors-secret)
highly recommend using opaque types in order to avoid breaking everyone's code
in a future major release.

This module forms as a protective layer between the internal modules and the
exposed modules, hiding all exposed types behind opaque types so the user cannot
access their content directly.

The opaque types are placed in a central module so all exposed modules can
safely access all exposed data types without risking to create circular imports.

@docs Vault, Event, Invite, Room, User, VaultUpdate

-}

import Internal.Api.Main as Api
import Internal.Values.Envelope as Envelope
import Internal.Values.Event as Event
import Internal.Values.Invite as Invite
import Internal.Values.Room as Room
import Internal.Values.User as User
import Internal.Values.Vault as Vault


{-| Opaque type for Matrix Event
-}
type Event
    = Event (Envelope.Envelope Event.Event)


{-| Opaque type for Matrix Invite
-}
type Invite
    = Invite (Envelope.Envelope Invite.Invite)


{-| Opauqe type for Matrix InviteEvent
-}
type InviteEvent
    = InviteEvent (Envelope.Envelope Invite.InviteEvent)


{-| Opaque type for Matrix Room
-}
type Room
    = Room (Envelope.Envelope Room.Room)


{-| Opaque type for Matrix User
-}
type User
    = User (Envelope.Envelope User.User)


{-| Opaque type for Matrix Vault
-}
type Vault
    = Vault (Envelope.Envelope Vault.Vault)


{-| Opaque type for Matrix VaultUpdate
-}
type VaultUpdate
    = VaultUpdate Api.Msg
