module Internal.Values.Vault exposing
    ( Vault, init
    , VaultUpdate(..), update
    , rooms, fromRoomId, mapRoom, updateRoom
    , invites, fromInviteId, mapInvite, updateInvite
    , getAccountData, setAccountData
    , coder
    )

{-| This module hosts the Vault module. The Vault is the data type storing all
credentials, all user information and all other information that the user
can receive from the Matrix API.


## Vault type

@docs Vault, init

To update the Vault, one uses VaultUpdate types.

@docs VaultUpdate, update


## Rooms

Rooms are environments where people can have a conversation with each other.

@docs rooms, fromRoomId, mapRoom, updateRoom


## Invites

Invites are invitations for a user to join a given room.

@docs invites, fromInviteId, mapInvite, updateInvite


## Account data

@docs getAccountData, setAccountData


## JSON

@docs coder

-}

import FastDict as Dict exposing (Dict)
import Internal.Config.Text as Text
import Internal.Tools.Hashdict as Hashdict exposing (Hashdict)
import Internal.Tools.Json as Json
import Internal.Values.Invite as Invite exposing (Invite)
import Internal.Values.Room as Room exposing (Room)
import Recursion
import Recursion.Fold


{-| This is the Vault type.
-}
type alias Vault =
    { accountData : Dict String Json.Value
    , invites : Hashdict Invite
    , nextBatch : Maybe String
    , rooms : Hashdict Room
    }


{-| The VaultUpdate type is a type that instructs the Vault to update itself
based on new information provided by the Matrix API.
-}
type VaultUpdate
    = CreateRoomIfNotExists String
    | MapRoom String Room.RoomUpdate
    | More (List VaultUpdate)
    | Optional (Maybe VaultUpdate)
    | RemoveInvite String
    | SetAccountData String Json.Value
    | SetInvite Invite
    | SetNextBatch String


{-| Convert a Vault to and from a JSON object.
-}
coder : Json.Coder Vault
coder =
    Json.object4
        { name = Text.docs.vault.name
        , description = Text.docs.vault.description
        , init = Vault
        }
        (Json.field.required
            { fieldName = "accountData"
            , toField = .accountData
            , description = Text.fields.vault.accountData
            , coder = Json.fastDict Json.value
            }
        )
        (Json.field.optional.withDefault
            { fieldName = "invites"
            , toField = .invites
            , description = Text.fields.vault.invites
            , coder = Hashdict.coder .roomId Invite.coder
            , default = ( Hashdict.empty .roomId, [] )
            }
        )
        (Json.field.optional.value
            { fieldName = "nextBatch"
            , toField = .nextBatch
            , description = Text.fields.vault.nextBatch
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "rooms"
            , toField = .rooms
            , description = Text.fields.vault.rooms
            , coder = Hashdict.coder .roomId Room.coder
            }
        )


{-| Get a given invite by the room id of the room that the invite invites the user to join.
-}
fromInviteId : String -> Vault -> Maybe Invite
fromInviteId roomId vault =
    Hashdict.get roomId vault.invites


{-| Get a given room by its room id.
-}
fromRoomId : String -> Vault -> Maybe Room
fromRoomId roomId vault =
    Hashdict.get roomId vault.rooms


{-| Get a piece of account data as information from the room.
-}
getAccountData : String -> Vault -> Maybe Json.Value
getAccountData key vault =
    Dict.get key vault.accountData


{-| Initiate a new Vault type.
-}
init : Vault
init =
    { accountData = Dict.empty
    , invites = Hashdict.empty .roomId
    , nextBatch = Nothing
    , rooms = Hashdict.empty .roomId
    }


{-| Get a list of all invites that are currently open.
-}
invites : Vault -> List Invite
invites vault =
    Hashdict.values vault.invites


{-| Update an invite, if it exists. If the invite isn't known, this operation
is ignored.
-}
mapInvite : String -> (Invite -> Invite) -> Vault -> Vault
mapInvite roomId f vault =
    { vault | invites = Hashdict.map roomId f vault.invites }


{-| Update a room, if it exists. If the room isn´t known, this operation is
ignored.
-}
mapRoom : String -> (Room -> Room) -> Vault -> Vault
mapRoom roomId f vault =
    { vault | rooms = Hashdict.map roomId f vault.rooms }


{-| Get a list of all joined rooms present in the vault.
-}
rooms : Vault -> List Room
rooms vault =
    Hashdict.values vault.rooms


{-| Set a piece of account data as information in the global vault data.
-}
setAccountData : String -> Json.Value -> Vault -> Vault
setAccountData key value vault =
    { vault | accountData = Dict.insert key value vault.accountData }


{-| Update an Invite based on whether it exists or not.
-}
updateInvite : String -> (Maybe Invite -> Maybe Invite) -> Vault -> Vault
updateInvite roomId f vault =
    { vault | invites = Hashdict.update roomId f vault.invites }


{-| Update a Room based on whether it exists or not.
-}
updateRoom : String -> (Maybe Room -> Maybe Room) -> Vault -> Vault
updateRoom roomId f vault =
    { vault | rooms = Hashdict.update roomId f vault.rooms }


{-| Update the Vault using a VaultUpdate type.
-}
update : VaultUpdate -> Vault -> Vault
update vaultUpdate startVault =
    Recursion.runRecursion
        (\vu ->
            case vu of
                CreateRoomIfNotExists roomId ->
                    (Maybe.withDefault (Room.init roomId) >> Maybe.Just)
                        |> updateRoom roomId
                        |> Recursion.base

                MapRoom roomId ru ->
                    Recursion.base (mapRoom roomId (Room.update ru))

                More items ->
                    Recursion.Fold.foldList (<<) identity items

                Optional (Just u) ->
                    Recursion.recurse u

                Optional Nothing ->
                    Recursion.base identity

                RemoveInvite roomId ->
                    Recursion.base (updateInvite roomId (always Nothing))

                SetAccountData key value ->
                    Recursion.base (setAccountData key value)

                SetInvite invite ->
                    (\currentInvite ->
                        case currentInvite of
                            Just ci ->
                                invite
                                    |> Invite.toList
                                    |> List.foldl Invite.addInviteEvent ci
                                    |> Just

                            Nothing ->
                                Just invite
                    )
                        |> updateInvite invite.roomId
                        |> Recursion.base

                SetNextBatch nb ->
                    Recursion.base
                        (\vault ->
                            { vault | nextBatch = Just nb }
                        )
        )
        vaultUpdate
        startVault
