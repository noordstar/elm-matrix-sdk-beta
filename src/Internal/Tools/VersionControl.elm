module Internal.Tools.VersionControl exposing
    ( VersionControl, withBottomLayer
    , sameForVersion, MiddleLayer, addMiddleLayer
    , isSupported, toDict, fromVersion, mostRecentFromVersionList, fromVersionList
    )

{-|


# Version Control module

This module helps you maintain different functions based on their version.

Not every Matrix homeserver is the same. Some keep up with the latest Matrix
specifications, while others stay behind because they have to support legacy
projects who do not support new API endpoints (yet). The Elm SDK aims to support
as many homeserver versions as possible - at the same time.

Support for legacy versions can be difficult! The Elm SDK expects one way of
getting information, and translating every Matrix spec(ification) version to it
can take time. But what if a new Matrix spec version adds a new feature? Do we
need to re-translate every single version to accomodate any future updates?

The VersionControl helps define different API rules for different spec versions
in an easy way. The VersionControl module puts all the versions in a linear
timeline. (Because, you know, updates are usually newer versions of older
versions.) This way, you can define different behaviour while still having only
one input, one output.

The module can be best described as a layered version type.

    |----------------------------------------------|
    | VersionControl                               |
    |                     input            output  |
    |                       |                ^     |
    |---------------------- | -------------- | ----|
                            |                |
    |---------------------- | -------------- | ----|
    | MiddleLayer v3        |                |     |
    |                       [---> current ---]     |
    |                       |                |     |
    |                    downcast          upcast  |
    |                       |                ^     |
    |---------------------- | -------------- | ----|
                            |                |
    |---------------------- | -------------- | ----|
    | MiddleLayer v2        |                |     |
    |                       [---> current ---]     |
    |                       |                |     |
    |                    downcast          upcast  |
    |                       |                ^     |
    |---------------------- | -------------- | ----|
                            |                |
    |---------------------- | -------------- | ----|
    | BottomLayer v1        |                |     |
    |                       \---> current ---/     |
    |                                              |
    |----------------------------------------------|

This method means you only need to write one downcast, one current and one
upcast whenever you introduce a new version. In other words, you can instantly
update all functions without having to write every version!

The VersionControl keeps tracks the version order. This way, you can either get
the VersionControl type to render the function for the most recent supported
version, or you can choose for yourself which version you prefer to use.


## Building a VersionControl

To build a VersionControl type, one must start with the bottom layer and start
building up to newer versions with middle layers.


### Create

@docs VersionControl, withBottomLayer


### Expand

@docs sameForVersion, MiddleLayer, addMiddleLayer


## Getting functions

Once you've successfully built the VersionControl type, there's a variety of
ways in which you can find an appropriate function.

@docs isSupported, toDict, fromVersion, mostRecentFromVersionList, fromVersionList

-}

import Dict exposing (Dict)


{-| The VersionControl layer is the layer on top that keeps track of all the
available versions. It is usually defined with a bottom layer and a few layers
on top.
-}
type VersionControl input output
    = VersionControl
        { latestVersion : input -> output
        , order : List String
        , versions : Dict String (input -> output)
        }


{-| The middle layer is placed between a VersionControl and a BottomLayer to
support a new function for a new version. The abbreviations stand for the
following:

  - `cin` means **current in**. It is the Middle Layer's input.

  - `cout` means **current out**. It is the Middle Layer's output.

  - `din` means **downcast in**. It is the Bottom Layer's input.

  - `dout` means **downcast out**. It is the Bottom Layer's output.

As a result, we have the following model to explain the MiddleLayer:

    |----------------------------------------------|
    | VersionControl                               |
    |                     input            output  |
    |                       |                ^     |
    |---------------------- | -------------- | ----|
                           [cin]           [cout]
    |---------------------- | -------------- | ----|
    | MiddleLayer           |                |     |
    |                       [---> current ---]     |
    |                       |                |     |
    |                    downcast          upcast  |
    |                       |                ^     |
    |---------------------- | -------------- | ----|
                          [din]            [dout]
    |---------------------- | -------------- | ----|
    | BottomLayer           |                |     |
    |                       \---> current ---/     |
    |                                              |
    |----------------------------------------------|

To sew a MiddleLayer type, we need the `downcast` and `upcast` functions to
translate the `cin` and `cout` to meaningful values `din` and `dout` for the
BottomLayer function.

Usually, this means transforming the data. For example, say our BottomLayer
still has an old version where people had just one name, and our MiddleLayer
version has two fields: a first and last name.

    type alias NewUser =
        { firstName : String, lastName : String, age : Int }

    type alias OldUser =
        { name : String, age : Int }

An appropriate downcasting function could then something like the following:

    downcast : NewUser -> OldUser
    downcast user =
        { name = user.firstName ++ " " ++ user.lastName, age = user.age }

-}
type alias MiddleLayer cin cout din dout =
    { current : cin -> cout
    , downcast : cin -> din
    , upcast : dout -> cout
    , version : String
    }


{-| Add a MiddleLayer to the VersionControl, effectively updating all old
functions with a downcast and upcast to deal with the inputs and outputs of all
functions at the same time.

For example, using the `NewUser` and `OldUser` types, one could create the
following example to get the user's names:

    vc : VersionControl NewUser String
    vc =
        withBottomLayer
            { current = .name
            , version = "v1"
            }
            |> sameForVersion "v2"
            |> sameForVersion "v3"
            |> sameForVersion "v4"
            |> sameForVersion "v5"
            |> sameForVersion "v6"
            |> addMiddleLayer
                { downcast = \user -> { name = user.firstName ++ " " ++ user.lastName, age = user.age }
                , current = \user -> user.firstName ++ " " ++ user.lastName
                , upcast = identity
                , version = "v7"
                }

Effectively, even though versions `v1` through `v6` still require an `OldUser`
type as an input, all functions have now been updated to the new standard of
getting a `NewUser` as an input thanks to the `downcast` function.

-}
addMiddleLayer : MiddleLayer cin cout din dout -> VersionControl din dout -> VersionControl cin cout
addMiddleLayer { current, downcast, upcast, version } (VersionControl d) =
    VersionControl
        { latestVersion = current
        , order = version :: d.order
        , versions =
            d.versions
                |> Dict.map (\_ f -> downcast >> f >> upcast)
                |> Dict.insert version current
        }


{-| Get the function that corresponds with a given version. Returns `Nothing` if
the version has never been inserted into the VersionControl type.
-}
fromVersion : String -> VersionControl a b -> Maybe (a -> b)
fromVersion version (VersionControl { versions }) =
    Dict.get version versions


{-| Provided a list of versions, this function will provide a list of compatible versions to you in your preferred order.

If you just care about getting the most recent function, you will be better off using `mostRecentFromVersionList`,
but this function can help if you care about knowing which Matrix spec version you're using.

-}
fromVersionList : List String -> VersionControl a b -> List ( String, a -> b )
fromVersionList versionList vc =
    List.filterMap
        (\version ->
            vc
                |> fromVersion version
                |> Maybe.map (\f -> ( version, f ))
        )
        versionList


{-| Determine if a version is supported by the VersionControl.

    vc : VersionControl NewUser String
    vc =
        withBottomLayer
            { current = .name
            , version = "v1"
            }
            |> sameForVersion "v2"
            |> sameForVersion "v3"
            |> sameForVersion "v4"

    isSupported "v3" vc -- True
    isSupported "v9" vc -- False

-}
isSupported : String -> VersionControl a b -> Bool
isSupported version (VersionControl d) =
    Dict.member version d.versions


{-| Get the most recent event based on a list of versions. Returns `Nothing` if
the list is empty, or if none of the versions are supported.

    vc : VersionControl a b
    vc =
      withBottomLayer
        { current = foo
        , version = "v1"
        }
        |> sameForVersion "v2"
        |> sameForVersion "v3"
        |> sameForVersion "v4"
        |> sameForVersion "v5"
        |> sameForVersion "v6"

    -- This returns the function for v6 because that is the most recent version
    -- in the provided version list
    mostRecentFromVersionList [ "v5", "v3", "v7", "v6", "v8" ] vc

-}
mostRecentFromVersionList : List String -> VersionControl a b -> Maybe (a -> b)
mostRecentFromVersionList versionList ((VersionControl { order }) as vc) =
    order
        |> List.filter (\o -> List.member o versionList)
        |> List.filterMap (\v -> fromVersion v vc)
        |> List.head


{-| Not every version overhauls every interaction. For this reason, many version
functions are identical to their previous functions.

This function adds a new version to the VersionControl and tells it that the
version uses the same function as the previous version.

    vc : VersionControl User String
    vc =
        withBottomLayer
            { current = .name
            , version = "v1"
            }
            |> sameForVersion "v2"
            |> sameForVersion "v3"
            |> sameForVersion "v4"
            |> sameForVersion "v5"
            |> sameForVersion "v6"

The example above lists the function `.name` for versions `v1` through `v6`.

-}
sameForVersion : String -> VersionControl a b -> VersionControl a b
sameForVersion version (VersionControl data) =
    VersionControl
        { data
            | order = version :: data.order
            , versions = Dict.insert version data.latestVersion data.versions
        }


{-| Get a dict of all available functions.


    vc : VersionControl NewUser String
    vc =
        withBottomLayer
            { current = .name
            , version = "v1"
            }
            |> sameForVersion "v2"
            |> sameForVersion "v3"
            |> sameForVersion "v4"
            |> toDict

    -- Dict.fromList
    --     [ ( "v1", <internal> )
    --     , ( "v2", <internal> )
    --     , ( "v3", <internal> )
    --     , ( "v4", <internal> )
    --     ]

-}
toDict : VersionControl a b -> Dict String (a -> b)
toDict (VersionControl d) =
    d.versions


{-| You cannot create an empty VersionControl layer, you must always start with a BottomLayer
and then stack MiddleLayer types on top until you've reached the version that you're happy with.

    vc : VersionControl User String
    vc =
        withBottomLayer
            { current = .name
            , version = "v1"
            }

    type alias User =
        { name : String, age : Int }

-}
withBottomLayer : { current : input -> output, version : String } -> VersionControl input output
withBottomLayer { current, version } =
    VersionControl
        { latestVersion = current
        , order = List.singleton version
        , versions = Dict.singleton version current
        }
