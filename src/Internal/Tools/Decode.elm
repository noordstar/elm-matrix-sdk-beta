module Internal.Tools.Decode exposing
    ( Decoder, string, bool, int, float
    , nullable, list, slowDict, fastDict, keyValuePairs
    , field, at, index, opField, opFieldWithDefault
    , maybe, oneOf
    , map, map2, map3, map4, map5, map6, map7, map8, map9, map10, map11
    , lazy, value, null, succeed, fail, andThen
    , pString, pBool, pInt, pList
    )

{-|


# Advanced security Json.Decode

This module extends the standard JSON encode / decode library for security
measures. Most Elm libraries do not access an API this often without insight
for the user, and hence this module aims to offer the user more insight into
what is going on.

Additionally, the decoder will warn for suspicious values, and provide helpful
errors when the JSON fails to decode.


## Primitives

@docs Decoder, string, bool, int, float


## Data structures

@docs nullable, list, slowDict, fastDict, keyValuePairs


## Object primitives

@docs field, at, index, opField, opFieldWithDefault


## Inconsistent structure

@docs maybe, oneOf


## Mapping

@docs map, map2, map3, map4, map5, map6, map7, map8, map9, map10, map11


## Fancy decoding

@docs lazy, value, null, succeed, fail, andThen


## Phantom decoding

Phantom decoders allow you to create phantom types of standard Elm types.

@docs pString, pBool, pInt, pList

-}

import Dict as SDict
import FastDict as FDict
import Internal.Config.Leaks as L
import Internal.Config.Log as Log
import Internal.Config.Phantom as Phantom
import Internal.Config.Text as Text
import Internal.Tools.DecodeExtra as D
import Json.Decode as D
import Set


{-| A value that knows how to decode JSON values.
-}
type alias Decoder a =
    D.Decoder { content : a, messages : List ( String, String ) }


{-| Create decoders that depend on previous results.
-}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen func =
    D.andThen
        (\a ->
            D.map
                (\b -> { b | messages = a.messages ++ b.messages })
                (func a.content)
        )


{-| Decode a nested JSON object, requiring certain fields.
-}
at : List String -> Decoder a -> Decoder a
at =
    D.at


{-| Decode a JSON boolean into an Elm bool.
-}
bool : Decoder Bool
bool =
    D.map empty D.bool


{-| Initialize a standard object for the decoder.
-}
empty : a -> { content : a, messages : List ( String, String ) }
empty x =
    { content = x, messages = [] }


{-| Ignore the JSON and make the decoder fail.
-}
fail : String -> Decoder a
fail =
    D.fail


{-| Decode a JSON object into a fast Elm dict from miniBill/elm-fast-dict.
-}
fastDict : Decoder a -> Decoder (FDict.Dict String a)
fastDict x =
    keyValuePairs x
        |> andThen
            (\pairs ->
                let
                    dict =
                        FDict.fromList pairs

                    oldLength =
                        List.length pairs

                    newLength =
                        FDict.size dict
                in
                if oldLength == newLength then
                    succeed dict

                else
                    D.succeed
                        { content = dict
                        , messages =
                            [ ( Log.warn, Text.decodedDictSize oldLength newLength ) ]
                        }
            )


{-| Decode a JSON object, requiring a particular field.
-}
field : String -> Decoder a -> Decoder a
field =
    D.field


{-| Decode a JSON number into an Elm flaot.
-}
float : Decoder Float
float =
    D.map empty D.float


{-| Decode a JSON array, requiring a particular index.
-}
index : Int -> Decoder a -> Decoder a
index =
    D.index


{-| Decode a JSON number into an Elm int.
-}
int : Decoder Int
int =
    D.map empty D.int


{-| Decode a JSON object into a list of pairs.
-}
keyValuePairs : Decoder a -> Decoder (List ( String, a ))
keyValuePairs x =
    D.map
        (\result ->
            { content = List.map (Tuple.mapSecond .content) result
            , messages =
                result
                    |> List.map Tuple.second
                    |> List.map .messages
                    |> List.concat
            }
        )
        (D.keyValuePairs x)


{-| Sometimes you have JSON with recursive structure, like nested comments.
You can use `lazy` to make sure your decoder unrolls lazily.
-}
lazy : (() -> Decoder a) -> Decoder a
lazy =
    D.lazy


{-| Decode a JSON array into an Elm list.
-}
list : Decoder a -> Decoder (List a)
list x =
    D.map
        (\result ->
            { content = List.map .content result
            , messages =
                result
                    |> List.map .messages
                    |> List.concat
            }
        )
        (D.list x)


{-| Transform a decoder.
-}
map : (a -> value) -> Decoder a -> Decoder value
map func da =
    D.map
        (\a ->
            { content = func a.content
            , messages = a.messages
            }
        )
        da


{-| Try two decoders and combine the result.
-}
map2 : (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
map2 func da db =
    D.map2
        (\a b ->
            { content = func a.content b.content
            , messages =
                List.concat
                    [ a.messages
                    , b.messages
                    ]
            }
        )
        da
        db


{-| Try three decoders and combine the result.
-}
map3 : (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
map3 func da db dc =
    D.map3
        (\a b c ->
            { content = func a.content b.content c.content
            , messages =
                List.concat
                    [ a.messages
                    , b.messages
                    , c.messages
                    ]
            }
        )
        da
        db
        dc


{-| Try four decoders and combine the result.
-}
map4 : (a -> b -> c -> d -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder value
map4 func da db dc dd =
    D.map4
        (\a b c d ->
            { content = func a.content b.content c.content d.content
            , messages =
                List.concat
                    [ a.messages
                    , b.messages
                    , c.messages
                    , d.messages
                    ]
            }
        )
        da
        db
        dc
        dd


{-| Try five decoders and combine the result.
-}
map5 : (a -> b -> c -> d -> e -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder value
map5 func da db dc dd de =
    D.map5
        (\a b c d e ->
            { content = func a.content b.content c.content d.content e.content
            , messages =
                List.concat
                    [ a.messages
                    , b.messages
                    , c.messages
                    , d.messages
                    , e.messages
                    ]
            }
        )
        da
        db
        dc
        dd
        de


{-| Try six decoders and combine the result.
-}
map6 : (a -> b -> c -> d -> e -> f -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder value
map6 func da db dc dd de df =
    D.map6
        (\a b c d e f ->
            { content = func a.content b.content c.content d.content e.content f.content
            , messages =
                List.concat
                    [ a.messages
                    , b.messages
                    , c.messages
                    , d.messages
                    , e.messages
                    , f.messages
                    ]
            }
        )
        da
        db
        dc
        dd
        de
        df


{-| Try seven decoders and combine the result.
-}
map7 : (a -> b -> c -> d -> e -> f -> g -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder value
map7 func da db dc dd de df dg =
    D.map7
        (\a b c d e f g ->
            { content = func a.content b.content c.content d.content e.content f.content g.content
            , messages =
                List.concat
                    [ a.messages
                    , b.messages
                    , c.messages
                    , d.messages
                    , e.messages
                    , f.messages
                    , g.messages
                    ]
            }
        )
        da
        db
        dc
        dd
        de
        df
        dg


{-| Try eight decoders and combine the result.
-}
map8 : (a -> b -> c -> d -> e -> f -> g -> h -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder value
map8 func da db dc dd de df dg dh =
    D.map8
        (\a b c d e f g h ->
            { content = func a.content b.content c.content d.content e.content f.content g.content h.content
            , messages =
                List.concat
                    [ a.messages
                    , b.messages
                    , c.messages
                    , d.messages
                    , e.messages
                    , f.messages
                    , g.messages
                    , h.messages
                    ]
            }
        )
        da
        db
        dc
        dd
        de
        df
        dg
        dh


{-| Try 9 decoders and combine the result.
-}
map9 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> value)
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder g
    -> Decoder h
    -> Decoder i
    -> Decoder value
map9 func da db dc dd de df dg dh di =
    D.map8
        (\a b c d e f g ( h, i ) ->
            { content = func a.content b.content c.content d.content e.content f.content g.content h.content i.content
            , messages =
                List.concat
                    [ a.messages
                    , b.messages
                    , c.messages
                    , d.messages
                    , e.messages
                    , f.messages
                    , g.messages
                    , h.messages
                    , i.messages
                    ]
            }
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
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder g
    -> Decoder h
    -> Decoder i
    -> Decoder j
    -> Decoder value
map10 func da db dc dd de df dg dh di dj =
    D.map8
        (\a b c d e f ( g, h ) ( i, j ) ->
            { content = func a.content b.content c.content d.content e.content f.content g.content h.content i.content j.content
            , messages =
                List.concat
                    [ a.messages
                    , b.messages
                    , c.messages
                    , d.messages
                    , e.messages
                    , f.messages
                    , g.messages
                    , h.messages
                    , i.messages
                    , j.messages
                    ]
            }
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
    -> Decoder a
    -> Decoder b
    -> Decoder c
    -> Decoder d
    -> Decoder e
    -> Decoder f
    -> Decoder g
    -> Decoder h
    -> Decoder i
    -> Decoder j
    -> Decoder k
    -> Decoder value
map11 func da db dc dd de df dg dh di dj dk =
    D.map8
        (\a b c d e ( f, g ) ( h, i ) ( j, k ) ->
            { content = func a.content b.content c.content d.content e.content f.content g.content h.content i.content j.content k.content
            , messages =
                List.concat
                    [ a.messages
                    , b.messages
                    , c.messages
                    , d.messages
                    , e.messages
                    , f.messages
                    , g.messages
                    , h.messages
                    , i.messages
                    , j.messages
                    , k.messages
                    ]
            }
        )
        da
        db
        dc
        dd
        de
        (D.map2 Tuple.pair df dg)
        (D.map2 Tuple.pair dh di)
        (D.map2 Tuple.pair dj dk)


{-| Helpful for dealing with optional fields
-}
maybe : Decoder a -> Decoder (Maybe a)
maybe x =
    D.map
        (\result ->
            case result of
                Just { content, messages } ->
                    { content = Just content, messages = messages }

                Nothing ->
                    empty Nothing
        )
        (D.maybe x)


{-| Decode a `null` value into some Elm value.
-}
null : a -> Decoder a
null =
    D.null >> D.map empty


{-| Decode a nullable JSON value into an Elm value.
-}
nullable : Decoder a -> Decoder (Maybe a)
nullable x =
    D.map
        (\result ->
            case result of
                Just { content, messages } ->
                    { content = Just content, messages = messages }

                Nothing ->
                    empty Nothing
        )
        (D.nullable x)


{-| Try a bunch of different decoders. This can be useful if the JSON may come
in a couple different formats.
-}
oneOf : List (Decoder a) -> Decoder a
oneOf =
    D.oneOf


{-| Decode a JSON object, requiring a particular field:

  - If the field does not exist, the decoder decodes `Nothing`
  - If the field DOES exist, the decoder must always return a `Just content` or fail

-}
opField : String -> Decoder a -> Decoder (Maybe a)
opField key x =
    D.map
        (\result ->
            case result of
                Just { content, messages } ->
                    { content = Just content, messages = messages }

                Nothing ->
                    empty Nothing
        )
        (D.opField key x)


{-| Decode a JSON object, requiring a particular field or raising a default:

  - If the field does not exist, the decoder returns the default
  - If the field DOES exist, the decoder must always return value or fail

-}
opFieldWithDefault : String -> a -> Decoder a -> Decoder a
opFieldWithDefault key default x =
    opField key x |> map (Maybe.withDefault default)


{-| Transform a JSON boolean into a phantom Elm bool.
-}
pBool : Decoder Bool -> Decoder (Phantom.PBool ph)
pBool =
    map Phantom.PBool


{-| Transform a JSON number into a phantom Elm int.
-}
pInt : Decoder Int -> Decoder (Phantom.PInt ph)
pInt =
    map Phantom.PInt


{-| Transform a JSON list into a phantom Elm list.
-}
pList : Decoder (List a) -> Decoder (Phantom.PList ph a)
pList =
    map Phantom.PList


{-| Transform a JSON string into a phantom Elm string.
-}
pString : Decoder String -> Decoder (Phantom.PString ph)
pString =
    map Phantom.PString


{-| Decode a JSON object into an Elm dict.
-}
slowDict : Decoder a -> Decoder (SDict.Dict String a)
slowDict x =
    D.map
        (\result ->
            { content =
                result
                    |> List.map (Tuple.mapSecond .content)
                    |> SDict.fromList
            , messages =
                result
                    |> List.map Tuple.second
                    |> List.map .messages
                    |> List.concat
            }
        )
        (D.keyValuePairs x)


{-| Decode a JSON string into an Elm string.

This decoder also checks for suspicious inputs, such as the
[Leaking values](Internal-Config-Leaks), to look for suspicious inputs.

-}
string : Decoder String
string =
    D.map
        (\content ->
            { content = content
            , messages =
                if Set.member content L.allLeaks then
                    [ ( Log.securityWarn, Text.leakingValueFound content )
                    ]

                else
                    []
            }
        )
        D.string


{-| Ignore the JSON and produce a certain Elm value.
-}
succeed : a -> Decoder a
succeed =
    D.succeed >> D.map empty


{-| Do not do anything with a JSON value, just bring it into Elm as a `Value`.
-}
value : Decoder D.Value
value =
    D.map empty D.value
