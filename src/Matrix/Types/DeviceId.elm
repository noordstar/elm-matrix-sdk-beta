module Matrix.Types.DeviceId exposing
    ( DeviceId, fromString, toString
    , encode, decoder
    )

{-| Device identifiers.

A Matrix account can be logged in on multiple devices at once. Each login gets
its own `DeviceId`.


## DeviceId

@docs DeviceId, fromString, toString


## JSON

@docs encode, decoder

-}

import Json.Decode as D
import Json.Encode as E


{-| The ID of one logged-in Matrix device session.
-}
type DeviceId
    = DeviceId String


{-| Encode a device ID as a JSON string.

    DeviceId.encode deviceId

-}
encode : DeviceId -> E.Value
encode =
    toString >> E.string


{-| Decode a device ID from a JSON string.

    import Json.Decode as D
    import Matrix.Types.DeviceId as DeviceId

    D.decodeString DeviceId.decoder "\"DEVICE123\""
    -- Ok <deviceId>

-}
decoder : D.Decoder DeviceId
decoder =
    D.map fromString D.string


{-| Create a device ID from its raw string value.

    DeviceId.fromString "DEVICE123"

-}
fromString : String -> DeviceId
fromString =
    DeviceId


{-| Extract the raw string value from a device ID.

    DeviceId.toString deviceId
    -- "DEVICE123"

-}
toString : DeviceId -> String
toString (DeviceId s) =
    s
