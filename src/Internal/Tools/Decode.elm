module Internal.Tools.Decode exposing
    ( opField, opFieldWithDefault
    , map9, map10, map11
    )

{-|


# Decode module

This module contains helper functions that help decode JSON.


## Optional field decoders

@docs opField, opFieldWithDefault


## Extended map functions

@docs map9, map10, map11

-}

import Json.Decode as D


{-| Add an optional field decoder. If the field exists, the decoder will fail
if the field doesn't decode properly.

This decoder standard out from `D.maybe <| D.field fieldName decoder` because
that will decode into a `Nothing` if the `decoder` fails. This function will
only decode into a `Nothing` if the field doesn't exist, and will fail if
`decoder` fails.

The function also returns Nothing if the field exists but it is null.

-}
opField : String -> D.Decoder a -> D.Decoder (Maybe a)
opField fieldName decoder =
    D.value
        |> D.field fieldName
        |> D.maybe
        |> D.andThen
            (\v ->
                case v of
                    Just _ ->
                        D.oneOf
                            [ D.null Nothing
                            , D.map Just decoder
                            ]
                            |> D.field fieldName

                    Nothing ->
                        D.succeed Nothing
            )


{-| Add an optional field decoder. If the field is not given, the decoder will
return a default value. If the field exists, the decoder will fail if the field
doesn't decode properly.
-}
opFieldWithDefault : String -> a -> D.Decoder a -> D.Decoder a
opFieldWithDefault fieldName default decoder =
    opField fieldName decoder |> D.map (Maybe.withDefault default)


{-| Try 9 decoders and combine the result.
-}
map9 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> value)
    -> D.Decoder a
    -> D.Decoder b
    -> D.Decoder c
    -> D.Decoder d
    -> D.Decoder e
    -> D.Decoder f
    -> D.Decoder g
    -> D.Decoder h
    -> D.Decoder i
    -> D.Decoder value
map9 func da db dc dd de df dg dh di =
    D.map8
        (\a b c d e f g ( h, i ) ->
            func a b c d e f g h i
        )
        da
        db
        dc
        dd
        de
        df
        dg
        (D.map2 Tuple.pair dh di)


{-| Try 10 decoders and combine the result.
-}
map10 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> value)
    -> D.Decoder a
    -> D.Decoder b
    -> D.Decoder c
    -> D.Decoder d
    -> D.Decoder e
    -> D.Decoder f
    -> D.Decoder g
    -> D.Decoder h
    -> D.Decoder i
    -> D.Decoder j
    -> D.Decoder value
map10 func da db dc dd de df dg dh di dj =
    D.map8
        (\a b c d e f ( g, h ) ( i, j ) ->
            func a b c d e f g h i j
        )
        da
        db
        dc
        dd
        de
        df
        (D.map2 Tuple.pair dg dh)
        (D.map2 Tuple.pair di dj)


{-| Try 11 decoders and combine the result.
-}
map11 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> value)
    -> D.Decoder a
    -> D.Decoder b
    -> D.Decoder c
    -> D.Decoder d
    -> D.Decoder e
    -> D.Decoder f
    -> D.Decoder g
    -> D.Decoder h
    -> D.Decoder i
    -> D.Decoder j
    -> D.Decoder k
    -> D.Decoder value
map11 func da db dc dd de df dg dh di dj dk =
    D.map8
        (\a b c d e ( f, g ) ( h, i ) ( j, k ) ->
            func a b c d e f g h i j k
        )
        da
        db
        dc
        dd
        de
        (D.map2 Tuple.pair df dg)
        (D.map2 Tuple.pair dh di)
        (D.map2 Tuple.pair dj dk)
