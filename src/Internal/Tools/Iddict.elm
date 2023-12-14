module Internal.Tools.Iddict exposing (Iddict, empty, get, insert, isEmpty, keys, map, member, remove, singleton, size, values)
{-| The id-dict is a data type that lets us store values in a dictionary using
unique identifiers. This can be used as a dictionary where the keys do not
matter.

The benefit of the iddict is that it generates the keys FOR you. This way, you
do not need to generate identifiers yourself.

## Id-dict

@docs Iddict

## Build

@docs empty, singleton, insert, map, remove

## Query

@docs isEmpty, member, get, size

## Lists

@docs keys, values
-}

import FastDict as Dict exposing (Dict)

{-| The Iddict data type.
-}
type Iddict a
    = Iddict 
        { cursor : Int
        , dict : Dict Int a
        }

{-| Create an empty id-dict.
-}
empty : Iddict a
empty =
    Iddict
        { cursor = 0
        , dict = Dict.empty
        }

{-| Get a value from the id-dict using its key.
-}
get : Int -> Iddict a -> Maybe a
get k (Iddict { dict }) =
    Dict.get k dict

{-| Insert a new value into the id-dict. Given that the id-dict generates its
key, the function returns both the updated id-dict as the newly generated key.

    x = empty |> insert "hello" -- ( 0, <Iddict with value "hello"> )

    case x of
        ( _, iddict ) ->
            get 0 iddict -- Just "hello"
-}
insert : a -> Iddict a -> (Int, Iddict a)
insert v (Iddict d) =
    ( d.cursor
    , Iddict { cursor = d.cursor + 1, dict = Dict.insert d.cursor v d.dict }
    )

{-| Determine if an id-dict is empty.
-}
isEmpty : Iddict a -> Bool
isEmpty (Iddict d) =
    Dict.isEmpty d.dict

{-| Get all of the keys from the id-dict, sorted from lowest to highest.
-}
keys : Iddict a -> List Int
keys (Iddict { dict }) =
    Dict.keys dict

{-| Map an existing value at a given key, if it exists. If it does not exist,
the operation does nothing.
-}
map : Int -> (a -> a) -> Iddict a -> Iddict a
map k f (Iddict d) =
    Iddict { d | dict = Dict.update k (Maybe.map f) d.dict }

{-| Determine if a key is in an id-dict.
-}
member : Int -> Iddict a -> Bool
member k (Iddict d) =
    k < d.cursor && Dict.member k d.dict

{-| Remove a key-value pair from the id-dict. If the key is not found, no 
changes are made.
-}
remove : Int -> Iddict a -> Iddict a
remove k (Iddict d) =
    Iddict { d | dict = Dict.remove k d.dict }

{-| Create an id-dict with a single value.
-}
singleton : a -> (Int, Iddict a)
singleton v = insert v empty

{-| Determine the number of key-value pairs in the id-dict.
-}
size : Iddict a -> Int
size (Iddict d) =
    Dict.size d.dict

{-| Get all of the values from an id-dict, in the order of their keys.
-}
values : Iddict a -> List a
values (Iddict { dict }) =
    Dict.values dict
