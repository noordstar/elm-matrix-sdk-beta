module Internal.Tools.Json exposing (..)

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

-}

import Dict as SlowDict
import FastDict
import Internal.Config.Log exposing (Log, log)
import Internal.Tools.DecodeExtra as D
import Internal.Tools.EncodeExtra as E
import Json.Decode as D
import Json.Encode as E


type Field a object
    = Field
        { fieldName : String
        , description : List String
        , encoder : a -> Maybe E.Value
        , decoder : D.Decoder ( a, List Log )
        , docs : JSONDocs
        , toField : object -> a
        , requiredness : RequiredField
        }


type JSONCoder a
    = JSONCoder
        { encoder : a -> E.Value
        , decoder : D.Decoder ( a, List Log )
        , docs : JSONDocs
        }


type JSONDocs
    = DocsBool
    | DocsDict JSONDocs
    | DocsFloat
    | DocsInt
    | DocsList JSONDocs
    | DocsObject
        { name : String
        , description : List String
        , keys :
            List
                { field : String
                , description : List String
                , required : RequiredField
                , content : JSONDocs
                }
        }
    | DocsOptional JSONDocs
    | DocsString


type RequiredField
    = RequiredField
    | OptionalField
    | OptionalFieldWithDefault String


bool : JSONCoder Bool
bool =
    JSONCoder
        { encoder = E.bool
        , decoder = D.map empty D.bool
        , docs = DocsBool
        }


{-| Get a JSON coder's decode value
-}
decode : JSONCoder a -> D.Decoder ( a, List Log )
decode (JSONCoder data) =
    data.decoder


{-| Create a tuple with no logs
-}
empty : a -> ( a, List Log )
empty x =
    ( x, [] )


{-| Get a JSON coder's encode value
-}
encode : JSONCoder a -> (a -> E.Value)
encode (JSONCoder data) =
    data.encoder


{-| Define a fast dict. The dict can only have strings as keys.
-}
fastDict : JSONCoder value -> JSONCoder (FastDict.Dict String value)
fastDict (JSONCoder value) =
    JSONCoder
        { encoder = FastDict.toCoreDict >> E.dict identity value.encoder
        , decoder =
            value.decoder
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
        , docs = DocsDict value.docs
        }


{-| Create a new field
-}
field :
    { required : { fieldName : String, toField : object -> a, description : List String, coder : JSONCoder a } -> Field a object
    , optional :
        { value : { fieldName : String, toField : object -> Maybe a, description : List String, coder : JSONCoder a } -> Field (Maybe a) object
        , withDefault : { fieldName : String, toField : object -> a, description : List String, coder : JSONCoder a, default : ( a, List Log ), defaultToString : a -> String } -> Field a object
        }
    }
field =
    { required =
        \{ fieldName, toField, description, coder } ->
            case coder of
                JSONCoder { encoder, decoder, docs } ->
                    Field
                        { fieldName = fieldName
                        , toField = toField
                        , description = description
                        , encoder = encoder >> Maybe.Just
                        , decoder = decoder
                        , docs = docs
                        , requiredness = RequiredField
                        }
    , optional =
        { value =
            \{ fieldName, toField, description, coder } ->
                case coder of
                    JSONCoder { encoder, decoder, docs } ->
                        Field
                            { fieldName = fieldName
                            , toField = toField
                            , description = description
                            , encoder = Maybe.map encoder
                            , decoder =
                                decoder
                                    |> D.opField fieldName
                                    |> D.map
                                        (\value ->
                                            case value of
                                                Just ( v, l ) ->
                                                    ( Just v, l )

                                                Nothing ->
                                                    ( Nothing, [] )
                                        )
                            , docs = docs
                            , requiredness = OptionalField
                            }
        , withDefault =
            \{ fieldName, toField, description, coder, default, defaultToString } ->
                case coder of
                    JSONCoder { encoder, decoder, docs } ->
                        Field
                            { fieldName = fieldName
                            , toField = toField
                            , description = description
                            , encoder = encoder >> Maybe.Just
                            , decoder = D.opFieldWithDefault fieldName default decoder
                            , docs = docs
                            , requiredness =
                                default
                                    |> Tuple.first
                                    |> defaultToString
                                    |> OptionalFieldWithDefault
                            }
        }
    }


{-| Define a float.
-}
float : JSONCoder Float
float =
    JSONCoder
        { encoder = E.float
        , decoder = D.map empty D.float
        , docs = DocsFloat
        }


{-| Define an int.
-}
int : JSONCoder Int
int =
    JSONCoder
        { encoder = E.int
        , decoder = D.map empty D.int
        , docs = DocsInt
        }


{-| Define a list.
-}
list : JSONCoder a -> JSONCoder (List a)
list (JSONCoder old) =
    JSONCoder
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


maybe : JSONCoder a -> JSONCoder (Maybe a)
maybe (JSONCoder old) =
    JSONCoder
        { encoder = Maybe.map old.encoder >> Maybe.withDefault E.null
        , decoder =
            old.decoder
                |> D.nullable
                |> D.map
                    (\value ->
                        case value of
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


{-| Define an object with 2 keys
-}
object2 :
    { name : String, description : List String, init : a -> b -> object }
    -> Field a object
    -> Field b object
    -> JSONCoder object
object2 { name, description, init } fa fb =
    JSONCoder
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
    { name : String, description : List String, init : a -> b -> c -> object }
    -> Field a object
    -> Field b object
    -> Field c object
    -> JSONCoder object
object3 { name, description, init } fa fb fc =
    JSONCoder
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
    { name : String, description : List String, init : a -> b -> c -> d -> object }
    -> Field a object
    -> Field b object
    -> Field c object
    -> Field d object
    -> JSONCoder object
object4 { name, description, init } fa fb fc fd =
    JSONCoder
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
    { name : String, description : List String, init : a -> b -> c -> d -> e -> object }
    -> Field a object
    -> Field b object
    -> Field c object
    -> Field d object
    -> Field e object
    -> JSONCoder object
object5 { name, description, init } fa fb fc fd fe =
    JSONCoder
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
    { name : String, description : List String, init : a -> b -> c -> d -> e -> f -> object }
    -> Field a object
    -> Field b object
    -> Field c object
    -> Field d object
    -> Field e object
    -> Field f object
    -> JSONCoder object
object6 { name, description, init } fa fb fc fd fe ff =
    JSONCoder
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
    { name : String, description : List String, init : a -> b -> c -> d -> e -> f -> g -> object }
    -> Field a object
    -> Field b object
    -> Field c object
    -> Field d object
    -> Field e object
    -> Field f object
    -> Field g object
    -> JSONCoder object
object7 { name, description, init } fa fb fc fd fe ff fg =
    JSONCoder
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
    { name : String, description : List String, init : a -> b -> c -> d -> e -> f -> g -> h -> object }
    -> Field a object
    -> Field b object
    -> Field c object
    -> Field d object
    -> Field e object
    -> Field f object
    -> Field g object
    -> Field h object
    -> JSONCoder object
object8 { name, description, init } fa fb fc fd fe ff fg fh =
    JSONCoder
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
    { name : String, description : List String, init : a -> b -> c -> d -> e -> f -> g -> h -> i -> object }
    -> Field a object
    -> Field b object
    -> Field c object
    -> Field d object
    -> Field e object
    -> Field f object
    -> Field g object
    -> Field h object
    -> Field i object
    -> JSONCoder object
object9 { name, description, init } fa fb fc fd fe ff fg fh fi =
    JSONCoder
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
    { name : String, description : List String, init : a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> object }
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
    -> JSONCoder object
object10 { name, description, init } fa fb fc fd fe ff fg fh fi fj =
    JSONCoder
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
    { name : String, description : List String, init : a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> object }
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
    -> JSONCoder object
object11 { name, description, init } fa fb fc fd fe ff fg fh fi fj fk =
    JSONCoder
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


{-| Define a slow dict from the elm/core library.
-}
slowDict : JSONCoder value -> JSONCoder (SlowDict.Dict String value)
slowDict (JSONCoder data) =
    JSONCoder
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


{-| Define a string.
-}
string : JSONCoder String
string =
    JSONCoder
        { encoder = E.string
        , decoder = D.map empty D.string
        , docs = DocsString
        }


{-| Turn a Field type into a usable JSON decoder
-}
toDecoderField : Field a object -> D.Decoder ( a, List Log )
toDecoderField (Field data) =
    data.decoder


{-| Turn a Field type into a descriptive field documentation
-}
toDocsField : Field a object -> { field : String, description : List String, required : RequiredField, content : JSONDocs }
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
