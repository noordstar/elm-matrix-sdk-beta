module Internal.Values.Vault exposing
    ( Vault, init
    , VaultUpdate(..), update
    , rooms, fromRoomId, mapRoom, updateRoom
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

@docs rooms, fromRoomId, mapRoom, updateRoom


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
    , nextBatch : Maybe String
    , rooms : Hashdict Room
    , user : Maybe User
    }


{-| The VaultUpdate type is a type that instructs the Vault to update itself
based on new information provided by the Matrix API.
-}
type VaultUpdate
    = CreateRoomIfNotExists String
    | MapRoom String Room.RoomUpdate
    | More (List VaultUpdate)
    | Optional (Maybe VaultUpdate)
    | SetAccountData String Json.Value
    | SetNextBatch String
    | SetUser User


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
        (Json.field.optional.value
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
init : Maybe User -> Vault
init mUser =
    { accountData = Dict.empty
    , nextBatch = Nothing
    , rooms = Hashdict.empty .roomId
    , user = mUser
    }


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

        Optional (Just u) ->
            update u vault

        Optional Nothing ->
            vault

        SetAccountData key value ->
            setAccountData key value vault

        SetNextBatch nb ->
            { vault | nextBatch = Just nb }

        SetUser user ->
            { vault | user = Just user }
