module Matrix.Types.BatchToken exposing
    ( BatchToken, fromString, toString
    , equals
    , encode, decoder
    )

{-| Batch tokens.

A batch token indicates a place on the [Timeline](Matrix.Types.Sync#Timeline).

Batch tokens are simple strings. I built this separate type, however, because
you're supposed to treat batch tokens are opaque values. You shouldn't mess
with them or try to interpret them!

Nonetheless, this module allows you to escape that restriction if you deem it
necessary.


## BatchToken

@docs BatchToken, fromString, toString


## Comparison

@docs equals


## JSON

@docs encode, decoder

-}

import Json.Decode as D
import Json.Encode as E


{-| The batch token pointing to a location on the
[Timeline](Matrix.Types.Sync#Timeline).

Note that it is computable and that you can safely run comparisons through
the `(==)` operator. Alternatively, use the
[equals](Matrix.Types.BatchToken#equals) if you prefer an explicit function.

-}
type BatchToken
    = BatchToken String


{-| Encode a batch token as a JSON string.

    BatchToken.encode batchToken

-}
encode : BatchToken -> E.Value
encode =
    toString >> E.string


{-| Compares whether two batch tokens match. This function is shorthand for the
normal `(==)` operator.
-}
equals : BatchToken -> BatchToken -> Bool
equals =
    (==)


{-| Decode a batch token from a JSON string.

    import Json.Decode as D
    import Matrix.Types.BatchToken as BatchToken

    D.decodeString BatchToken.decoder "\"YmF0Y2ggdG9rZW4=\""
    -- Ok <BatchToken>

-}
decoder : D.Decoder BatchToken
decoder =
    D.map fromString D.string


{-| Create a room ID from its raw string value.

    BatchToken.fromString "YmF0Y2ggdG9rZW4="

-}
fromString : String -> BatchToken
fromString =
    BatchToken


{-| Extract the raw string value from a room ID.

    BatchToken.toString batchToken
    -- "YmF0Y2ggdG9rZW4="

-}
toString : BatchToken -> String
toString (BatchToken s) =
    s
