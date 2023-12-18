module Internal.Tools.Timestamp exposing
    ( Timestamp
    , encode, decoder
    )

{-| The Timestamp module is a simplification of the Timestamp as delivered by
elm/time. This module offers ways to work with the timestamp in meaningful ways.


## Timestamp

@docs Timestamp


## JSON coders

@docs encode, decoder

-}

import Json.Decode as D
import Json.Encode as E
import Time


{-| The Timestamp data type representing a moment in time.
-}
type alias Timestamp =
    Time.Posix


{-| Encode a timestamp into a JSON value.
-}
encode : Timestamp -> E.Value
encode =
    Time.posixToMillis >> E.int


{-| Decode a timestamp from a JSON value.
-}
decoder : D.Decoder Timestamp
decoder =
    D.map Time.millisToPosix D.int
