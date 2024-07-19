module Internal.Tools.Json exposing
    ( Coder, string, bool, int, float, value, unit
    , Encoder, encode, Decoder, decode, Value
    , succeed, fail, andThen, lazy, map
    , Docs(..), RequiredField(..), toDocs
    , list, listWithOne, slowDict, fastDict, fastIntDict, set, iddict, maybe
    , Field, field, parser
    , object1, object2, object3, object4, object5, object6, object7, object8, object9, object10, object11, object12, object13
    )

{-|


# JSON module

The JSON module wrapper helps define JSON encoders and decoders in a structural
manner.

While developing the Elm SDK, a huge amount of encoders and decoders had to
be written that also gained more requirements as the project got more complex:

1.  Objects needed JSON encoders
2.  Objects needed JSON decoders
3.  Objects needed documentation about how their JSON encodes/decodes
4.  Objects needed additional logs in case of special decoded values

To meet all these requirements, this module helps translate between JSON and
data types. Because this module uses dynamic builder types, this also means it
is relatively easy to write documentation for any data type that uses this
module to build its encoders and decoders.

@docs Coder, string, bool, int, float, value, unit


## JSON Coding

@docs Encoder, encode, Decoder, decode, Value


## Optional coding

@docs succeed, fail, andThen, lazy, map


## Documentation

@docs Docs, RequiredField, toDocs


## Data types

@docs list, listWithOne, slowDict, fastDict, fastIntDict, set, iddict, maybe


## Objects

This section creates objects that can be (re)used in the library's JSON
specification. For this, the user needs to construct fields for the object
first.

@docs Field, field, parser

Once all fields are constructed, the user can create JSON objects.

@docs object1, object2, object3, object4, object5, object6, object7, object8, object9, object10, object11, object12, object13

-}

import Dict as SlowDict
import FastDict
import Iddict exposing (Iddict)
import Internal.Config.Log as Log exposing (Log)
import Internal.Config.Text as Text
import Internal.Tools.DecodeExtra as D
import Internal.Tools.EncodeExtra as E
import Json.Decode as D
import Json.Encode as E
import Parser as P
import Set exposing (Set)


{-| A field of type `a` as a subtype of an object `object`.

In concrete terms, to construct a data type

    type alias User =
        { name : String
        , age : Int
        , hobbies : List String
        }

The user needs to construct the field types:

  - `Field String User`,
  - `Field Int User`,
  - and `Field (List String) User`.

-}
type Field a object
    = Field
        { fieldName : String
        , description : List String
        , encoder : a -> Maybe E.Value
        , decoder : D.Decoder ( a, List Log )
        , docs : Docs
        , toField : object -> a
        , requiredness : RequiredField
        }


{-| Builder type that helps create JSON encoders, JSON decoders, data type
documentation and various other data types.
-}
type Coder a
    = Coder
        { encoder : a -> E.Value
        , decoder : D.Decoder ( a, List Log )
        , docs : Docs
        }


type DecodeResult a
    = Success ( a, List Log )
    | Fail ( String, List Log )


{-| Decoder type that describes the format of a JSON value that can be decoded
as a given type.
-}
type alias Decoder a =
    D.Decoder ( a, List Log )


type alias Descriptive a =
    { a | name : String, description : List String }


{-| Structure of JSON documentation. It is up to an external module to turn the
documentation structure into a readable format.
-}
type Docs
    = DocsBool
    | DocsDict Docs
    | DocsFloat
    | DocsIddict Docs
    | DocsInt
    | DocsIntDict Docs
    | DocsLazy (() -> Docs)
    | DocsList Docs
    | DocsListWithOne Docs
    | DocsMap (Descriptive { content : Docs })
    | DocsObject
        (Descriptive
            { keys :
                List
                    { field : String
                    , description : List String
                    , required : RequiredField
                    , content : Docs
                    }
            }
        )
    | DocsOptional Docs
    | DocsParser String
    | DocsRiskyMap (Descriptive { content : Docs, failure : List String })
    | DocsSet Docs
    | DocsString
    | DocsUnit
    | DocsValue


{-| Encoder type that takes an input and converts it to a JSON value.
-}
type alias Encoder a =
    a -> E.Value


{-| Value that tells whether an object field is required to be included. If it
is not required, it can either be omitted - or a given default will be assumed.
The given default is a string representation, not the actual value.
-}
type RequiredField
    = RequiredField
    | OptionalField
    | OptionalFieldWithDefault String


{-| Represents an arbitary JavaScript value.
-}
type alias Value =
    E.Value


{-| Continue decoding a result. This function tests if it meets the criteria,
and then it manages the results.
-}
andThen : Descriptive { back : b -> a, forth : a -> DecodeResult b, failure : List String } -> Coder a -> Coder b
andThen { name, description, failure, back, forth } (Coder old) =
    Coder
        { encoder = back >> old.encoder
        , decoder =
            old.decoder
                |> D.andThen
                    (\result ->
                        case result of
                            ( out, logs ) ->
                                case forth out of
                                    Success x ->
                                        x
                                            |> Tuple.mapSecond (List.append logs)
                                            |> D.succeed

                                    Fail ( f, _ ) ->
                                        D.fail f
                    )
        , docs =
            DocsRiskyMap
                { name = name
                , description = description
                , content = old.docs
                , failure = failure
                }
        }


{-| Define a boolean value.
-}
bool : Coder Bool
bool =
    Coder
        { encoder = E.bool
        , decoder = D.map empty D.bool
        , docs = DocsBool
        }


{-| Get a JSON coder's decode value
-}
decode : Coder a -> D.Decoder ( a, List Log )
decode (Coder data) =
    data.decoder


{-| Generate documentation from a Coder definition.
-}
toDocs : Coder a -> Docs
toDocs (Coder data) =
    data.docs


{-| Create a tuple with no logs
-}
empty : a -> ( a, List Log )
empty x =
    ( x, [] )


{-| Get a JSON coder's encode value


    text : Json.Encode.Value
    text =
        encode string "test"

    -- == Json.Encode.string "test"

-}
encode : Coder a -> (a -> E.Value)
encode (Coder data) =
    data.encoder


{-| Fail a decoder.
-}
fail : String -> List Log -> DecodeResult a
fail reason logs =
    Fail ( reason, logs )


{-| Define a fast dict. The dict can only have strings as keys.
-}
fastDict : Coder value -> Coder (FastDict.Dict String value)
fastDict (Coder old) =
    Coder
        { encoder = FastDict.toCoreDict >> E.dict identity old.encoder
        , decoder =
            old.decoder
                |> D.keyValuePairs
                |> D.map
                    (\items ->
                        ( items
                            |> List.map (Tuple.mapSecond Tuple.first)
                            |> FastDict.fromList
                        , items
                            |> List.map Tuple.second
                            |> List.concatMap Tuple.second
                        )
                    )
        , docs = DocsDict old.docs
        }


{-| Define a fast dict where the keys are integers, not strings.
-}
fastIntDict : Coder value -> Coder (FastDict.Dict Int value)
fastIntDict (Coder old) =
    Coder
        { encoder = FastDict.toCoreDict >> E.dict String.fromInt old.encoder
        , decoder =
            old.decoder
                |> D.keyValuePairs
                |> D.map
                    (\items ->
                        ( items
                            |> List.map (Tuple.mapSecond Tuple.first)
                            |> List.filterMap
                                (\( k, v ) ->
                                    Maybe.map (\a -> ( a, v )) (String.toInt k)
                                )
                            |> FastDict.fromList
                        , List.concat
                            [ items
                                |> List.map Tuple.first
                                |> List.filter
                                    (\k ->
                                        case String.toInt k of
                                            Just _ ->
                                                True

                                            Nothing ->
                                                False
                                    )
                                |> List.map Text.logs.keyIsNotAnInt
                                |> List.map Log.log.warn
                            , items
                                |> List.map Tuple.second
                                |> List.concatMap Tuple.second
                            ]
                        )
                    )
        , docs = DocsIntDict old.docs
        }


{-| Create a new field using any of the three provided options.

For example, suppose we are creating a `Field String User` to represent the
`name` field in

    type alias User =
        { name : String
        , age : Int
        , hobbies : List String
        }

then the following field type would be used:

    field.required
        { fieldName = "name" -- Field name when encoded into JSON
        , toField = .name
        , description =
            [ "This description describes this field's information content."
            , "Here's another paragraph!"
            ]
        , coder = string
        }

Suppose the JSON isn't obligated to provide a list of hobbies, and the list would
by default be overriden with an empty list, then we would use the following
field type:

    field.optional.withDefault
        { fieldName = "hobbies"
        , toField = .hobbies
        , description =
            [ "The hobbies of the person. Can be omitted."
            ]
        , coder = list string
        , default = ( [ "football" ], [] ) -- The `List Log` can be inserted in case you wish to insert a message when relying on a default
        }

-}
field :
    { required : { fieldName : String, toField : object -> a, description : List String, coder : Coder a } -> Field a object
    , optional :
        { value : { fieldName : String, toField : object -> Maybe a, description : List String, coder : Coder a } -> Field (Maybe a) object
        , withDefault : { fieldName : String, toField : object -> a, description : List String, coder : Coder a, default : ( a, List Log ) } -> Field a object
        }
    }
field =
    { required =
        \{ fieldName, toField, description, coder } ->
            case coder of
                Coder { encoder, decoder, docs } ->
                    Field
                        { fieldName = fieldName
                        , toField = toField
                        , description = description
                        , encoder = encoder >> Maybe.Just
                        , decoder = D.field fieldName decoder
                        , docs = docs
                        , requiredness = RequiredField
                        }
    , optional =
        { value =
            \{ fieldName, toField, description, coder } ->
                case coder of
                    Coder { encoder, decoder, docs } ->
                        Field
                            { fieldName = fieldName
                            , toField = toField
                            , description = description
                            , encoder = Maybe.map encoder
                            , decoder =
                                decoder
                                    |> D.opField fieldName
                                    |> D.map
                                        (\out ->
                                            case out of
                                                Just ( v, l ) ->
                                                    ( Just v, l )

                                                Nothing ->
                                                    ( Nothing, [] )
                                        )
                            , docs = docs
                            , requiredness = OptionalField
                            }
        , withDefault =
            \{ fieldName, toField, description, coder, default } ->
                case coder of
                    Coder { encoder, decoder, docs } ->
                        Field
                            { fieldName = fieldName
                            , toField = toField
                            , description = description
                            , encoder =
                                \o ->
                                    let
                                        v =
                                            encoder o
                                    in
                                    -- If the value matches the default, do not record
                                    if E.encode 0 v == E.encode 0 (encoder (Tuple.first default)) then
                                        Nothing

                                    else
                                        Maybe.Just v
                            , decoder = D.opFieldWithDefault fieldName default decoder
                            , docs = docs
                            , requiredness =
                                default
                                    |> Tuple.first
                                    |> encoder
                                    |> E.encode 0
                                    |> OptionalFieldWithDefault
                            }
        }
    }


{-| Define a float value.
-}
float : Coder Float
float =
    Coder
        { encoder = E.float
        , decoder = D.map empty D.float
        , docs = DocsFloat
        }


{-| Define an Iddict as defined in
[noordstar/elm-iddict](https://package.elm-lang.org/packages/noordstar/elm-iddict/latest/).
-}
iddict : Coder a -> Coder (Iddict a)
iddict (Coder old) =
    Coder
        { encoder = Iddict.encode old.encoder
        , decoder =
            Iddict.decoder old.decoder
                |> D.map
                    (\out ->
                        ( Iddict.map (always Tuple.first) out
                        , Iddict.values out
                            |> List.concatMap Tuple.second
                        )
                    )
        , docs = DocsIddict old.docs
        }


{-| Define an int value.
-}
int : Coder Int
int =
    Coder
        { encoder = E.int
        , decoder = D.map empty D.int
        , docs = DocsInt
        }


{-| Define a lazy coder. This is useful when defining recursive structures.
-}
lazy : (() -> Coder value) -> Coder value
lazy f =
    Coder
        { encoder =
            \v ->
                case f () of
                    Coder old ->
                        old.encoder v
        , decoder =
            D.lazy
                (\() ->
                    case f () of
                        Coder old ->
                            old.decoder
                )
        , docs = DocsLazy (f >> toDocs)
        }


{-| Define a list.
-}
list : Coder a -> Coder (List a)
list (Coder old) =
    Coder
        { encoder = E.list old.encoder
        , decoder =
            old.decoder
                |> D.list
                |> D.map
                    (\items ->
                        ( List.map Tuple.first items
                        , List.concatMap Tuple.second items
                        )
                    )
        , docs = DocsList old.docs
        }


{-| Define a list that has at least one value
-}
listWithOne : Coder a -> Coder ( a, List a )
listWithOne (Coder old) =
    Coder
        { encoder = \( h, t ) -> E.list old.encoder (h :: t)
        , decoder =
            old.decoder
                |> D.list
                |> D.andThen
                    (\items ->
                        case items of
                            [] ->
                                D.fail Text.failures.listWithOne

                            ( h, l1 ) :: t ->
                                D.succeed
                                    ( ( h, List.map Tuple.first items )
                                    , List.concatMap Tuple.second t
                                        |> List.append l1
                                    )
                    )
        , docs = DocsListWithOne old.docs
        }


{-| Map a value.

Given that the value needs to be both encoded and decoded, the map function
should be invertible.

-}
map : Descriptive { back : b -> a, forth : a -> b } -> Coder a -> Coder b
map { name, description, back, forth } (Coder old) =
    Coder
        { encoder = back >> old.encoder
        , decoder = D.map (Tuple.mapFirst forth) old.decoder
        , docs =
            DocsMap
                { name = name, description = description, content = old.docs }
        }


{-| Define a maybe value.

NOTE: most of the time, you wish to avoid this function! Make sure to look at
objects instead.

-}
maybe : Coder a -> Coder (Maybe a)
maybe (Coder old) =
    Coder
        { encoder = Maybe.map old.encoder >> Maybe.withDefault E.null
        , decoder =
            old.decoder
                |> D.nullable
                |> D.map
                    (\out ->
                        case out of
                            Just ( v, logs ) ->
                                ( Just v, logs )

                            Nothing ->
                                empty Nothing
                    )
        , docs = DocsOptional old.docs
        }


{-| Use an objectEncoder to encode a list of items into a single object.
-}
objectEncoder : List ( String, object -> Maybe E.Value ) -> object -> E.Value
objectEncoder items object =
    items
        |> List.map (Tuple.mapSecond (\f -> f object))
        |> E.maybeObject


object1 :
    Descriptive { init : a -> object }
    -> Field a object
    -> Coder object
object1 { name, description, init } fa =
    Coder
        { encoder = objectEncoder [ toEncodeField fa ]
        , decoder = D.map (Tuple.mapFirst init) (toDecoderField fa)
        , docs =
            DocsObject
                { name = name
                , description = description
                , keys = [ toDocsField fa ]
                }
        }


{-| Define an object with 2 keys

    type alias Human =
        { name : String, age : Maybe Int }

    humanCoder : Coder Human
    humanCoder =
        object2
            { name = "Human"
            , description =
                [ "Documentation description of the human type."
                ]
            , init = Human
            }
            (field.required
                { fieldName = "name"
                , toField = .name
                , description =
                    [ "Human's name."
                    ]
                , coder = string
                }
            )
            (field.optional.value
                { fieldName = "age"
                , toField = .age
                , description =
                    [ "(Optional) human's age"
                    ]
                , coder = int
                }
            )

-}
object2 :
    Descriptive { init : a -> b -> object }
    -> Field a object
    -> Field b object
    -> Coder object
object2 { name, description, init } fa fb =
    Coder
        { encoder =
            objectEncoder
                [ toEncodeField fa
                , toEncodeField fb
                ]
        , decoder =
            D.map2
                (\( a, la ) ( b, lb ) ->
                    ( init a b
                    , List.concat [ la, lb ]
                    )
                )
                (toDecoderField fa)
                (toDecoderField fb)
        , docs =
            DocsObject
                { name = name
                , description = description
                , keys =
                    [ toDocsField fa
                    , toDocsField fb
                    ]
                }
        }


{-| Define an object with 3 keys
-}
object3 :
    Descriptive { init : a -> b -> c -> object }
    -> Field a object
    -> Field b object
    -> Field c object
    -> Coder object
object3 { name, description, init } fa fb fc =
    Coder
        { encoder =
            objectEncoder
                [ toEncodeField fa
                , toEncodeField fb
                , toEncodeField fc
                ]
        , decoder =
            D.map3
                (\( a, la ) ( b, lb ) ( c, lc ) ->
                    ( init a b c
                    , List.concat [ la, lb, lc ]
                    )
                )
                (toDecoderField fa)
                (toDecoderField fb)
                (toDecoderField fc)
        , docs =
            DocsObject
                { name = name
                , description = description
                , keys =
                    [ toDocsField fa
                    , toDocsField fb
                    , toDocsField fc
                    ]
                }
        }


{-| Define an object with 4 keys
-}
object4 :
    Descriptive { init : a -> b -> c -> d -> object }
    -> Field a object
    -> Field b object
    -> Field c object
    -> Field d object
    -> Coder object
object4 { name, description, init } fa fb fc fd =
    Coder
        { encoder =
            objectEncoder
                [ toEncodeField fa
                , toEncodeField fb
                , toEncodeField fc
                , toEncodeField fd
                ]
        , decoder =
            D.map4
                (\( a, la ) ( b, lb ) ( c, lc ) ( d, ld ) ->
                    ( init a b c d
                    , List.concat [ la, lb, lc, ld ]
                    )
                )
                (toDecoderField fa)
                (toDecoderField fb)
                (toDecoderField fc)
                (toDecoderField fd)
        , docs =
            DocsObject
                { name = name
                , description = description
                , keys =
                    [ toDocsField fa
                    , toDocsField fb
                    , toDocsField fc
                    , toDocsField fd
                    ]
                }
        }


{-| Define an object with 5 keys
-}
object5 :
    Descriptive { init : a -> b -> c -> d -> e -> object }
    -> Field a object
    -> Field b object
    -> Field c object
    -> Field d object
    -> Field e object
    -> Coder object
object5 { name, description, init } fa fb fc fd fe =
    Coder
        { encoder =
            objectEncoder
                [ toEncodeField fa
                , toEncodeField fb
                , toEncodeField fc
                , toEncodeField fd
                , toEncodeField fe
                ]
        , decoder =
            D.map5
                (\( a, la ) ( b, lb ) ( c, lc ) ( d, ld ) ( e, le ) ->
                    ( init a b c d e
                    , List.concat [ la, lb, lc, ld, le ]
                    )
                )
                (toDecoderField fa)
                (toDecoderField fb)
                (toDecoderField fc)
                (toDecoderField fd)
                (toDecoderField fe)
        , docs =
            DocsObject
                { name = name
                , description = description
                , keys =
                    [ toDocsField fa
                    , toDocsField fb
                    , toDocsField fc
                    , toDocsField fd
                    , toDocsField fe
                    ]
                }
        }


{-| Define an object with 6 keys
-}
object6 :
    Descriptive { init : a -> b -> c -> d -> e -> f -> object }
    -> Field a object
    -> Field b object
    -> Field c object
    -> Field d object
    -> Field e object
    -> Field f object
    -> Coder object
object6 { name, description, init } fa fb fc fd fe ff =
    Coder
        { encoder =
            objectEncoder
                [ toEncodeField fa
                , toEncodeField fb
                , toEncodeField fc
                , toEncodeField fd
                , toEncodeField fe
                , toEncodeField ff
                ]
        , decoder =
            D.map6
                (\( a, la ) ( b, lb ) ( c, lc ) ( d, ld ) ( e, le ) ( f, lf ) ->
                    ( init a b c d e f
                    , List.concat [ la, lb, lc, ld, le, lf ]
                    )
                )
                (toDecoderField fa)
                (toDecoderField fb)
                (toDecoderField fc)
                (toDecoderField fd)
                (toDecoderField fe)
                (toDecoderField ff)
        , docs =
            DocsObject
                { name = name
                , description = description
                , keys =
                    [ toDocsField fa
                    , toDocsField fb
                    , toDocsField fc
                    , toDocsField fd
                    , toDocsField fe
                    , toDocsField ff
                    ]
                }
        }


{-| Define an object with 7 keys
-}
object7 :
    Descriptive { init : a -> b -> c -> d -> e -> f -> g -> object }
    -> Field a object
    -> Field b object
    -> Field c object
    -> Field d object
    -> Field e object
    -> Field f object
    -> Field g object
    -> Coder object
object7 { name, description, init } fa fb fc fd fe ff fg =
    Coder
        { encoder =
            objectEncoder
                [ toEncodeField fa
                , toEncodeField fb
                , toEncodeField fc
                , toEncodeField fd
                , toEncodeField fe
                , toEncodeField ff
                , toEncodeField fg
                ]
        , decoder =
            D.map7
                (\( a, la ) ( b, lb ) ( c, lc ) ( d, ld ) ( e, le ) ( f, lf ) ( g, lg ) ->
                    ( init a b c d e f g
                    , List.concat [ la, lb, lc, ld, le, lf, lg ]
                    )
                )
                (toDecoderField fa)
                (toDecoderField fb)
                (toDecoderField fc)
                (toDecoderField fd)
                (toDecoderField fe)
                (toDecoderField ff)
                (toDecoderField fg)
        , docs =
            DocsObject
                { name = name
                , description = description
                , keys =
                    [ toDocsField fa
                    , toDocsField fb
                    , toDocsField fc
                    , toDocsField fd
                    , toDocsField fe
                    , toDocsField ff
                    , toDocsField fg
                    ]
                }
        }


{-| Define an object with 8 keys
-}
object8 :
    Descriptive { init : a -> b -> c -> d -> e -> f -> g -> h -> object }
    -> Field a object
    -> Field b object
    -> Field c object
    -> Field d object
    -> Field e object
    -> Field f object
    -> Field g object
    -> Field h object
    -> Coder object
object8 { name, description, init } fa fb fc fd fe ff fg fh =
    Coder
        { encoder =
            objectEncoder
                [ toEncodeField fa
                , toEncodeField fb
                , toEncodeField fc
                , toEncodeField fd
                , toEncodeField fe
                , toEncodeField ff
                , toEncodeField fg
                , toEncodeField fh
                ]
        , decoder =
            D.map8
                (\( a, la ) ( b, lb ) ( c, lc ) ( d, ld ) ( e, le ) ( f, lf ) ( g, lg ) ( h, lh ) ->
                    ( init a b c d e f g h
                    , List.concat [ la, lb, lc, ld, le, lf, lg, lh ]
                    )
                )
                (toDecoderField fa)
                (toDecoderField fb)
                (toDecoderField fc)
                (toDecoderField fd)
                (toDecoderField fe)
                (toDecoderField ff)
                (toDecoderField fg)
                (toDecoderField fh)
        , docs =
            DocsObject
                { name = name
                , description = description
                , keys =
                    [ toDocsField fa
                    , toDocsField fb
                    , toDocsField fc
                    , toDocsField fd
                    , toDocsField fe
                    , toDocsField ff
                    , toDocsField fg
                    , toDocsField fh
                    ]
                }
        }


{-| Define an object with 9 keys
-}
object9 :
    Descriptive { init : a -> b -> c -> d -> e -> f -> g -> h -> i -> object }
    -> Field a object
    -> Field b object
    -> Field c object
    -> Field d object
    -> Field e object
    -> Field f object
    -> Field g object
    -> Field h object
    -> Field i object
    -> Coder object
object9 { name, description, init } fa fb fc fd fe ff fg fh fi =
    Coder
        { encoder =
            objectEncoder
                [ toEncodeField fa
                , toEncodeField fb
                , toEncodeField fc
                , toEncodeField fd
                , toEncodeField fe
                , toEncodeField ff
                , toEncodeField fg
                , toEncodeField fh
                , toEncodeField fi
                ]
        , decoder =
            D.map9
                (\( a, la ) ( b, lb ) ( c, lc ) ( d, ld ) ( e, le ) ( f, lf ) ( g, lg ) ( h, lh ) ( i, li ) ->
                    ( init a b c d e f g h i
                    , List.concat [ la, lb, lc, ld, le, lf, lg, lh, li ]
                    )
                )
                (toDecoderField fa)
                (toDecoderField fb)
                (toDecoderField fc)
                (toDecoderField fd)
                (toDecoderField fe)
                (toDecoderField ff)
                (toDecoderField fg)
                (toDecoderField fh)
                (toDecoderField fi)
        , docs =
            DocsObject
                { name = name
                , description = description
                , keys =
                    [ toDocsField fa
                    , toDocsField fb
                    , toDocsField fc
                    , toDocsField fd
                    , toDocsField fe
                    , toDocsField ff
                    , toDocsField fg
                    , toDocsField fh
                    , toDocsField fi
                    ]
                }
        }


{-| Define an object with 10 keys
-}
object10 :
    Descriptive { init : a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> object }
    -> Field a object
    -> Field b object
    -> Field c object
    -> Field d object
    -> Field e object
    -> Field f object
    -> Field g object
    -> Field h object
    -> Field i object
    -> Field j object
    -> Coder object
object10 { name, description, init } fa fb fc fd fe ff fg fh fi fj =
    Coder
        { encoder =
            objectEncoder
                [ toEncodeField fa
                , toEncodeField fb
                , toEncodeField fc
                , toEncodeField fd
                , toEncodeField fe
                , toEncodeField ff
                , toEncodeField fg
                , toEncodeField fh
                , toEncodeField fi
                , toEncodeField fj
                ]
        , decoder =
            D.map10
                (\( a, la ) ( b, lb ) ( c, lc ) ( d, ld ) ( e, le ) ( f, lf ) ( g, lg ) ( h, lh ) ( i, li ) ( j, lj ) ->
                    ( init a b c d e f g h i j
                    , List.concat [ la, lb, lc, ld, le, lf, lg, lh, li, lj ]
                    )
                )
                (toDecoderField fa)
                (toDecoderField fb)
                (toDecoderField fc)
                (toDecoderField fd)
                (toDecoderField fe)
                (toDecoderField ff)
                (toDecoderField fg)
                (toDecoderField fh)
                (toDecoderField fi)
                (toDecoderField fj)
        , docs =
            DocsObject
                { name = name
                , description = description
                , keys =
                    [ toDocsField fa
                    , toDocsField fb
                    , toDocsField fc
                    , toDocsField fd
                    , toDocsField fe
                    , toDocsField ff
                    , toDocsField fg
                    , toDocsField fh
                    , toDocsField fi
                    , toDocsField fj
                    ]
                }
        }


{-| Define an object with 11 keys
-}
object11 :
    Descriptive { init : a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> object }
    -> Field a object
    -> Field b object
    -> Field c object
    -> Field d object
    -> Field e object
    -> Field f object
    -> Field g object
    -> Field h object
    -> Field i object
    -> Field j object
    -> Field k object
    -> Coder object
object11 { name, description, init } fa fb fc fd fe ff fg fh fi fj fk =
    Coder
        { encoder =
            objectEncoder
                [ toEncodeField fa
                , toEncodeField fb
                , toEncodeField fc
                , toEncodeField fd
                , toEncodeField fe
                , toEncodeField ff
                , toEncodeField fg
                , toEncodeField fh
                , toEncodeField fi
                , toEncodeField fj
                , toEncodeField fk
                ]
        , decoder =
            D.map11
                (\( a, la ) ( b, lb ) ( c, lc ) ( d, ld ) ( e, le ) ( f, lf ) ( g, lg ) ( h, lh ) ( i, li ) ( j, lj ) ( k, lk ) ->
                    ( init a b c d e f g h i j k
                    , List.concat [ la, lb, lc, ld, le, lf, lg, lh, li, lj, lk ]
                    )
                )
                (toDecoderField fa)
                (toDecoderField fb)
                (toDecoderField fc)
                (toDecoderField fd)
                (toDecoderField fe)
                (toDecoderField ff)
                (toDecoderField fg)
                (toDecoderField fh)
                (toDecoderField fi)
                (toDecoderField fj)
                (toDecoderField fk)
        , docs =
            DocsObject
                { name = name
                , description = description
                , keys =
                    [ toDocsField fa
                    , toDocsField fb
                    , toDocsField fc
                    , toDocsField fd
                    , toDocsField fe
                    , toDocsField ff
                    , toDocsField fg
                    , toDocsField fh
                    , toDocsField fi
                    , toDocsField fj
                    , toDocsField fk
                    ]
                }
        }


{-| Define an object with 12 keys
-}
object12 :
    Descriptive { init : a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> object }
    -> Field a object
    -> Field b object
    -> Field c object
    -> Field d object
    -> Field e object
    -> Field f object
    -> Field g object
    -> Field h object
    -> Field i object
    -> Field j object
    -> Field k object
    -> Field l object
    -> Coder object
object12 { name, description, init } fa fb fc fd fe ff fg fh fi fj fk fl =
    Coder
        { encoder =
            objectEncoder
                [ toEncodeField fa
                , toEncodeField fb
                , toEncodeField fc
                , toEncodeField fd
                , toEncodeField fe
                , toEncodeField ff
                , toEncodeField fg
                , toEncodeField fh
                , toEncodeField fi
                , toEncodeField fj
                , toEncodeField fk
                , toEncodeField fl
                ]
        , decoder =
            D.map12
                (\( a, la ) ( b, lb ) ( c, lc ) ( d, ld ) ( e, le ) ( f, lf ) ( g, lg ) ( h, lh ) ( i, li ) ( j, lj ) ( k, lk ) ( l, ll ) ->
                    ( init a b c d e f g h i j k l
                    , List.concat [ la, lb, lc, ld, le, lf, lg, lh, li, lj, lk, ll ]
                    )
                )
                (toDecoderField fa)
                (toDecoderField fb)
                (toDecoderField fc)
                (toDecoderField fd)
                (toDecoderField fe)
                (toDecoderField ff)
                (toDecoderField fg)
                (toDecoderField fh)
                (toDecoderField fi)
                (toDecoderField fj)
                (toDecoderField fk)
                (toDecoderField fl)
        , docs =
            DocsObject
                { name = name
                , description = description
                , keys =
                    [ toDocsField fa
                    , toDocsField fb
                    , toDocsField fc
                    , toDocsField fd
                    , toDocsField fe
                    , toDocsField ff
                    , toDocsField fg
                    , toDocsField fh
                    , toDocsField fi
                    , toDocsField fj
                    , toDocsField fk
                    , toDocsField fl
                    ]
                }
        }


{-| Define an object with 13 keys
-}
object13 :
    Descriptive { init : a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> object }
    -> Field a object
    -> Field b object
    -> Field c object
    -> Field d object
    -> Field e object
    -> Field f object
    -> Field g object
    -> Field h object
    -> Field i object
    -> Field j object
    -> Field k object
    -> Field l object
    -> Field m object
    -> Coder object
object13 { name, description, init } fa fb fc fd fe ff fg fh fi fj fk fl fm =
    Coder
        { encoder =
            objectEncoder
                [ toEncodeField fa
                , toEncodeField fb
                , toEncodeField fc
                , toEncodeField fd
                , toEncodeField fe
                , toEncodeField ff
                , toEncodeField fg
                , toEncodeField fh
                , toEncodeField fi
                , toEncodeField fj
                , toEncodeField fk
                , toEncodeField fl
                , toEncodeField fm
                ]
        , decoder =
            D.map13
                (\( a, la ) ( b, lb ) ( c, lc ) ( d, ld ) ( e, le ) ( f, lf ) ( g, lg ) ( h, lh ) ( i, li ) ( j, lj ) ( k, lk ) ( l, ll ) ( m, lm ) ->
                    ( init a b c d e f g h i j k l m
                    , List.concat [ la, lb, lc, ld, le, lf, lg, lh, li, lj, lk, ll, lm ]
                    )
                )
                (toDecoderField fa)
                (toDecoderField fb)
                (toDecoderField fc)
                (toDecoderField fd)
                (toDecoderField fe)
                (toDecoderField ff)
                (toDecoderField fg)
                (toDecoderField fh)
                (toDecoderField fi)
                (toDecoderField fj)
                (toDecoderField fk)
                (toDecoderField fl)
                (toDecoderField fm)
        , docs =
            DocsObject
                { name = name
                , description = description
                , keys =
                    [ toDocsField fa
                    , toDocsField fb
                    , toDocsField fc
                    , toDocsField fd
                    , toDocsField fe
                    , toDocsField ff
                    , toDocsField fg
                    , toDocsField fh
                    , toDocsField fi
                    , toDocsField fj
                    , toDocsField fk
                    , toDocsField fl
                    , toDocsField fm
                    ]
                }
        }


{-| Define a parser that converts a string into a custom Elm type.
-}
parser : { name : String, p : P.Parser ( a, List Log ), toString : a -> String } -> Coder a
parser { name, p, toString } =
    Coder
        { encoder = toString >> E.string
        , decoder =
            D.string
                |> D.andThen
                    (\v ->
                        case P.run p v of
                            Err _ ->
                                D.fail ("Failed to parse " ++ name ++ "!")

                            Ok o ->
                                D.succeed o
                    )
        , docs = DocsParser name
        }


{-| Define a set.
-}
set : Coder comparable -> Coder (Set comparable)
set (Coder data) =
    Coder
        { encoder = E.set data.encoder
        , decoder =
            data.decoder
                |> D.list
                |> D.map
                    (\items ->
                        ( items
                            |> List.map Tuple.first
                            |> Set.fromList
                        , items
                            |> List.concatMap Tuple.second
                        )
                    )
        , docs = DocsSet data.docs
        }


{-| Define a slow dict from the `elm/core` library.
-}
slowDict : Coder value -> Coder (SlowDict.Dict String value)
slowDict (Coder data) =
    Coder
        { encoder = E.dict identity data.encoder
        , decoder =
            data.decoder
                |> D.keyValuePairs
                |> D.map
                    (\items ->
                        ( items
                            |> List.map (Tuple.mapSecond Tuple.first)
                            |> SlowDict.fromList
                        , items
                            |> List.map Tuple.second
                            |> List.concatMap Tuple.second
                        )
                    )
        , docs = DocsDict data.docs
        }


{-| Define a string value.
-}
string : Coder String
string =
    Coder
        { encoder = E.string
        , decoder = D.map empty D.string
        , docs = DocsString
        }


{-| Succeed a decoder.
-}
succeed : a -> List Log -> DecodeResult a
succeed x logs =
    Success ( x, logs )


{-| Turn a Field type into a usable JSON decoder
-}
toDecoderField : Field a object -> D.Decoder ( a, List Log )
toDecoderField (Field data) =
    data.decoder


{-| Turn a Field type into a descriptive field documentation
-}
toDocsField : Field a object -> { field : String, description : List String, required : RequiredField, content : Docs }
toDocsField x =
    case x of
        Field { fieldName, description, docs, requiredness } ->
            { field = fieldName
            , description = description
            , required = requiredness
            , content = docs
            }


{-| Turn a Field type into a usable object for a maybeObject type
-}
toEncodeField : Field a object -> ( String, object -> Maybe E.Value )
toEncodeField (Field data) =
    ( data.fieldName, data.toField >> data.encoder )


{-| Completely ignore whatever needs to be encoded, and simply return a unit
value.
-}
unit : Coder ()
unit =
    Coder
        { encoder = \() -> E.object []
        , decoder = D.succeed ( (), [] )
        , docs = DocsUnit
        }


{-| Do not do anything useful with a JSON value, just bring it to Elm as a
JavaScript value.
-}
value : Coder Value
value =
    Coder
        { encoder = identity
        , decoder = D.map (\v -> ( v, [] )) D.value
        , docs = DocsValue
        }
