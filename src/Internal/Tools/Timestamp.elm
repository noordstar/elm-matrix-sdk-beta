module Internal.Tools.Timestamp exposing
    ( Timestamp
    , coder, encode, decoder
    )

{-| The Timestamp module is a simplification of the Timestamp as delivered by
elm/time. This module offers ways to work with the timestamp in meaningful ways.


## Timestamp

@docs Timestamp


## JSON coders

@docs coder, encode, decoder

-}

import Internal.Tools.Json as Json
import Time


{-| The Timestamp data type representing a moment in time.
-}
type alias Timestamp =
    Time.Posix


{-| Create a Json coder
-}
coder : Json.Coder Timestamp
coder =
    Json.map
        { back = Time.posixToMillis
        , forth = Time.millisToPosix
        , name = "Milliseconds to POSIX"
        , description =
            [ "Converts the timestamp from milliseconds to a POSIX timestamp."
            ]
        }
        Json.int


{-| Encode a timestamp into a JSON value.
-}
encode : Json.Encoder Timestamp
encode =
    Json.encode coder


{-| Decode a timestamp from a JSON value.
-}
decoder : Json.Decoder Timestamp
decoder =
    Json.decode coder
