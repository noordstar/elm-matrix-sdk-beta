module Matrix.Types.RoomId exposing
    ( RoomId, fromString, toString
    , equals
    , encode, decoder
    )

{-| Room identifiers.

A room ID uniquely identifies one room across the whole Matrix network.


## RoomId

@docs RoomId, fromString, toString


## Comparison

@docs equals


## JSON

@docs encode, decoder

-}

import Json.Decode as D
import Json.Encode as E


{-| The ID of a Matrix room.

Room IDs can be compared safely with the `(==)` operator. If you prefer an
explicit function, use [equals](#equals).

-}
type RoomId
    = RoomId String


{-| Encode a room ID as a JSON string.

    RoomId.encode roomId

-}
encode : RoomId -> E.Value
encode =
    toString >> E.string


{-| Check whether two room IDs point to the same room. This is shorthand for
the normal `(==)` operator.
-}
equals : RoomId -> RoomId -> Bool
equals =
    (==)


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
