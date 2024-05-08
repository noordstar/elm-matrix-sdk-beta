module Internal.Values.Vault exposing
    ( fromRoomId, mapRoom, updateRoom
    , getAccountData, setAccountData
    , Vault
    )

{-| This module hosts the Vault module. The Vault is the data type storing all
credentials, all user information and all other information that the user
can receive from the Matrix API.




## Rooms

Rooms are environments where people can have a conversation with each other.

@docs fromRoomId, mapRoom, updateRoom


## Account data

@docs getAccountData, setAccountData

-}

import FastDict as Dict exposing (Dict)
import Internal.Tools.Hashdict as Hashdict exposing (Hashdict)
import Internal.Tools.Json as Json
import Internal.Values.Room exposing (Room)


{-| This is the Vault type.
-}
type alias Vault =
    { accountData : Dict String Json.Value
    , rooms : Hashdict Room
    }


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
