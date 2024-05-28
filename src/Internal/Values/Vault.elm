module Internal.Values.Vault exposing
    ( Vault, init
    , VaultUpdate(..), update
    , fromRoomId, mapRoom, updateRoom
    , getAccountData, setAccountData
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

@docs fromRoomId, mapRoom, updateRoom


## Account data

@docs getAccountData, setAccountData

-}

import FastDict as Dict exposing (Dict)
import Internal.Config.Text as Text
import Internal.Tools.Hashdict as Hashdict exposing (Hashdict)
import Internal.Tools.Json as Json
import Internal.Values.Room as Room exposing (Room)
import Internal.Values.User as User exposing (User)


{-| This is the Vault type.
-}
type alias Vault =
    { accountData : Dict String Json.Value
    , rooms : Hashdict Room
    , user : User
    }


{-| The VaultUpdate type is a type that instructs the Vault to update itself
based on new information provided by the Matrix API.
-}
type VaultUpdate
    = CreateRoomIfNotExists String
    | MapRoom String Room.RoomUpdate
    | More (List VaultUpdate)
    | SetAccountData String Json.Value
    | SetUser User


coder : Json.Coder Vault
coder =
    Json.object3
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
        (Json.field.required
            { fieldName = "rooms"
            , toField = .rooms
            , description = Text.fields.vault.rooms
            , coder = Hashdict.coder .roomId Room.coder
            }
        )
        (Json.field.required
            { fieldName = "user"
            , toField = .user
            , description = Text.fields.vault.user
            , coder = User.coder
            }
        )


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
init : User -> Vault
init user =
    { accountData = Dict.empty
    , rooms = Hashdict.empty .roomId
    , user = user
    }


{-| Update a room, if it exists. If the room isn´t known, this operation is
ignored.
-}
mapRoom : String -> (Room -> Room) -> Vault -> Vault
mapRoom roomId f vault =
    { vault | rooms = Hashdict.map roomId f vault.rooms }


{-| Set a piece of account data as information in the global vault data.
-}
setAccountData : String -> Json.Value -> Vault -> Vault
setAccountData key value vault =
    { vault | accountData = Dict.insert key value vault.accountData }


{-| Update a Room based on whether it exists or not.
-}
updateRoom : String -> (Maybe Room -> Maybe Room) -> Vault -> Vault
updateRoom roomId f vault =
    { vault | rooms = Hashdict.update roomId f vault.rooms }


{-| Update the Vault using a VaultUpdate type.
-}
update : VaultUpdate -> Vault -> Vault
update vu vault =
    case vu of
        CreateRoomIfNotExists roomId ->
            updateRoom roomId
                (Maybe.withDefault (Room.init roomId) >> Maybe.Just)
                vault

        MapRoom roomId ru ->
            mapRoom roomId (Room.update ru) vault

        More items ->
            List.foldl update vault items

        SetAccountData key value ->
            setAccountData key value vault

        SetUser user ->
            { vault | user = user }
