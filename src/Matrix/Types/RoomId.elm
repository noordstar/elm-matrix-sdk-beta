module Matrix.Types.RoomId exposing
    ( RoomId, fromString, toString
    , encode, decoder
    )

{-| Room identifiers.

A room ID uniquely identifies one room across the whole Matrix network.


## RoomId

@docs RoomId, fromString, toString


## JSON

@docs encode, decoder

-}

import Json.Decode as D
import Json.Encode as E


{-| The ID of a Matrix room.
-}
type RoomId
    = RoomId String


{-| Encode a room ID as a JSON string.

    RoomId.encode roomId

-}
encode : RoomId -> E.Value
encode =
    toString >> E.string


{-| Decode a room ID from a JSON string.

    import Json.Decode as D
    import Matrix.Types.RoomId as RoomId

    D.decodeString RoomId.decoder "\"!room:example.org\""
    -- Ok <roomId>

-}
decoder : D.Decoder RoomId
decoder =
    D.map fromString D.string


{-| Create a room ID from its raw string value.

    RoomId.fromString "!room:example.org"

-}
fromString : String -> RoomId
fromString =
    RoomId


{-| Extract the raw string value from a room ID.

    RoomId.toString roomId
    -- "!room:example.org"

-}
toString : RoomId -> String
toString (RoomId s) =
    s
