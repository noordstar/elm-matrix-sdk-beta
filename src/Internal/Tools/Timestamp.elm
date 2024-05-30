module Internal.Tools.Timestamp exposing
    ( Timestamp
    , add, toMs
    , coder, encode, decoder
    )

{-| The Timestamp module is a simplification of the Timestamp as delivered by
elm/time. This module offers ways to work with the timestamp in meaningful ways.


## Timestamp

@docs Timestamp


## Calculate

@docs add, toMs


## JSON coders

@docs coder, encode, decoder

-}

import Internal.Tools.Json as Json
import Time


{-| The Timestamp data type representing a moment in time.
-}
type alias Timestamp =
    Time.Posix


{-| Add a given number of miliseconds to a given Timestamp.
-}
add : Int -> Timestamp -> Timestamp
add m =
    Time.posixToMillis
        >> (+) m
        >> Time.millisToPosix


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


{-| Turn a Timestamp into a number of miliseconds
-}
toMs : Timestamp -> Int
toMs =
    Time.posixToMillis
