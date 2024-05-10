module Internal.Tools.Hashdict exposing
    ( Hashdict
    , empty, singleton, insert, remove, removeKey
    , isEmpty, member, memberKey, get, size, isEqual
    , keys, values, toList, fromList
    , rehash, union, map, update
    , coder, encode, decoder, softDecoder
    )

{-| This module abstracts the `Dict` type with one function that assigns a
unique identifier for each value based on a function that assigns each value.

This allows you to store values based on an externally defined identifier.


## Dictionaries

@docs Hashdict


## Build

@docs empty, singleton, insert, remove, removeKey


## Query

@docs isEmpty, member, memberKey, get, size, isEqual


## Lists

@docs keys, values, toList, fromList


## Transform

@docs rehash, union, map, update


## JSON coders

@docs coder, encode, decoder, softDecoder

-}

import FastDict as Dict exposing (Dict)
import Internal.Config.Log as Log
import Internal.Config.Text as Text
import Internal.Tools.Json as Json


{-| A dictionary of keys and values where each key is defined by its value. For
example, this can be useful when every user is identifiable by their username:

    import Hashdict exposing (Hashdict)

    users : Hashdict User
    users =
        Hashdict.fromList .name
            [ User "Alice" 28 1.65
            , User "Bob" 19 1.82
            , User "Chuck" 33 1.75
            ]

    type alias User =
        { name : String
        , age : Int
        , height : Float
        }

In the example listed above, the users are stored by their username, which means
that all you need to know is the value "Alice" to retrieve all the information
about them. Additionally, you do not need to specify a key to insert the values.

-}
type Hashdict a
    = Hashdict
        { hash : a -> String
        , values : Dict String a
        }


{-| Define how Hashdict can be encoded to and decoded from a JSON object.
-}
coder : (a -> String) -> Json.Coder a -> Json.Coder (Hashdict a)
coder f c1 =
    Json.andThen
        { name = Text.docs.hashdict.name
        , description = Text.docs.hashdict.description
        , forth =
            -- TODO: Implement fastDictWithFilter function
            \items ->
                case List.filter (\( k, v ) -> f v /= k) (Dict.toList items) of
                    [] ->
                        { hash = f, values = items }
                            |> Hashdict
                            |> Json.succeed
                            |> (|>) []

                    wrongHashes ->
                        wrongHashes
                            |> List.map Tuple.first
                            |> List.map ((++) "Invalid hash")
                            |> List.map Log.log.error
                            |> Json.fail Text.invalidHashInHashdict
        , back = \(Hashdict h) -> h.values
        , failure =
            Text.failures.hashdict
        }
        (Json.fastDict c1)


{-| Decode a hashdict from a JSON value. To create a hashdict, you are expected
to insert a hash function. If the hash function doesn't properly hash the values
as expected, the decoder will fail to decode the hashdict.
-}
decoder : (a -> String) -> Json.Coder a -> Json.Decoder (Hashdict a)
decoder f c1 =
    Json.decode (coder f c1)


{-| Create an empty hashdict.
-}
empty : (a -> String) -> Hashdict a
empty hash =
    Hashdict { hash = hash, values = Dict.empty }


{-| Encode a Hashdict into a JSON value. Keep in mind that an Elm function
cannot be universally converted to JSON, so it is up to you to preserve that
hash function!
-}
encode : Json.Coder a -> Json.Encoder (Hashdict a)
encode c1 (Hashdict h) =
    Json.encode (coder h.hash c1) (Hashdict h)


{-| Convert an association list into a hashdict.
-}
fromList : (a -> String) -> List a -> Hashdict a
fromList hash xs =
    Hashdict
        { hash = hash
        , values =
            xs
                |> List.map (\x -> ( hash x, x ))
                |> Dict.fromList
        }


{-| Get the value associated with a hash. If the hash is not found, return
`Nothing`. This is useful when you are not sure if a hash will be in the
hashdict.
-}
get : String -> Hashdict a -> Maybe a
get k (Hashdict h) =
    Dict.get k h.values


{-| Insert a value into a hashdict. The key is automatically generated by the
hash function. If the function generates a collision, it replaces the existing
value in the hashdict.
-}
insert : a -> Hashdict a -> Hashdict a
insert v (Hashdict h) =
    Hashdict { h | values = Dict.insert (h.hash v) v h.values }


{-| Since the Hashdict contains a hash function, the == operator does not work
simply. Instead, you should use the isEqual operator.
-}
isEqual : Hashdict a -> Hashdict a -> Bool
isEqual h1 h2 =
    toList h1 == toList h2


{-| Determine if a hashdict is empty.
-}
isEmpty : Hashdict a -> Bool
isEmpty (Hashdict h) =
    Dict.isEmpty h.values


{-| Get all of the hashes in a hashdict, sorted from lowest to highest.
-}
keys : Hashdict a -> List String
keys (Hashdict h) =
    Dict.keys h.values


{-| Map a value on a given key. If the outcome of the function changes the hash,
the operation does nothing.
-}
map : String -> (a -> a) -> Hashdict a -> Hashdict a
map key f (Hashdict h) =
    Hashdict
        { h
            | values =
                Dict.update
                    key
                    (Maybe.map
                        (\value ->
                            let
                                newValue : a
                                newValue =
                                    f value
                            in
                            if h.hash newValue == h.hash value then
                                newValue

                            else
                                value
                        )
                    )
                    h.values
        }


{-| Determine if a value's hash is in a hashdict.
-}
member : a -> Hashdict a -> Bool
member value (Hashdict h) =
    Dict.member (h.hash value) h.values


{-| Determine if a hash is in a hashdict.
-}
memberKey : String -> Hashdict a -> Bool
memberKey key (Hashdict h) =
    Dict.member key h.values


{-| Remap a hashdict using a new hashing algorithm.
-}
rehash : (a -> String) -> Hashdict a -> Hashdict a
rehash f (Hashdict h) =
    Hashdict
        { hash = f
        , values =
            h.values
                |> Dict.values
                |> List.map (\v -> ( f v, v ))
                |> Dict.fromList
        }


{-| Remove a value from a hashdict. If the value's hash is found, the key-value
pair is removed. If the value's hash is not found, no changes are made.

    hdict |> Hashdict.remove (User "Alice" 19 1.82)

-}
remove : a -> Hashdict a -> Hashdict a
remove v (Hashdict h) =
    Hashdict { h | values = Dict.remove (h.hash v) h.values }


{-| Remove a key from a hashdict. If the key is not found, no changes are made.

    hdict |> Hashdict.removeKey "Alice"

-}
removeKey : String -> Hashdict a -> Hashdict a
removeKey k (Hashdict h) =
    Hashdict { h | values = Dict.remove k h.values }


{-| Create a hashdict with a single key-value pair.
-}
singleton : (a -> String) -> a -> Hashdict a
singleton f v =
    empty f |> insert v


{-| Determine the number of values in a hashdict.
-}
size : Hashdict a -> Int
size (Hashdict h) =
    Dict.size h.values


{-| Decode a hashdict from a JSON value. If you cannot deduce the originally
used hash function, (or if you simply do not care) you can use this function to
decode and rehash the Hashdict using your new hash function.
-}
softDecoder : (a -> String) -> Json.Coder a -> Json.Decoder (Hashdict a)
softDecoder f c1 =
    c1
        |> Json.fastDict
        |> Json.map
            { name = Text.docs.hashdict.name
            , description = Text.docs.hashdict.description
            , forth =
                \items ->
                    Hashdict { hash = f, values = items }
                        |> rehash f
            , back = \(Hashdict h) -> h.values
            }
        |> Json.decode


{-| Convert a hashdict into an association list of key-value pairs, sorted by
keys.
-}
toList : Hashdict a -> List ( String, a )
toList (Hashdict h) =
    Dict.toList h.values


{-| Combine two hashdicts under the hash function of the first. If there is a
collision, preference is given to the first hashdict.
-}
union : Hashdict a -> Hashdict a -> Hashdict a
union (Hashdict h1) hd2 =
    case rehash h1.hash hd2 of
        Hashdict h2 ->
            Hashdict
                { hash = h1.hash
                , values = Dict.union h1.values h2.values
                }


{-| Update a dict to maybe contain a value (or not). If the output does not
have the originally expected key, it is not updated.
-}
update : String -> (Maybe a -> Maybe a) -> Hashdict a -> Hashdict a
update key f ((Hashdict h) as hd) =
    case f (get key hd) of
        Just v ->
            if h.hash v == key then
                insert v hd

            else
                hd

        Nothing ->
            removeKey key hd


{-| Get all values stored in the hashdict, in the order of their keys.
-}
values : Hashdict a -> List a
values (Hashdict h) =
    Dict.values h.values
