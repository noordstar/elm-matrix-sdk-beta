module Internal.Api.Api exposing
    ( TaskChain, request
    , VersionControl, startWithVersion, startWithUnstableFeature, forVersion, sameForVersion, forUnstableFeature, versionChain
    )

{-|


# API

The API module is a front-end for implementing API endpoints according to spec.

This module is imported by various API endpoint implementations to keep the
implementation simple and understandable.

@docs TaskChain, request


## Spec versions

To respect spec versions, there is often a variety of ways to communicate with
the homeserver. For this reason, users can differentiate spec versions using
these functions.

@docs VersionControl, startWithVersion, startWithUnstableFeature, forVersion, sameForVersion, forUnstableFeature, versionChain

-}

import Internal.Api.Chain as C
import Internal.Api.Request as R
import Internal.Config.Log exposing (Log, log)
import Internal.Tools.Json as Json
import Internal.Values.Context as Context exposing (APIContext, Versions)
import Internal.Values.Vault as V
import Recursion
import Set


{-| A TaskChain helps create a chain of HTTP requests.
-}
type alias TaskChain ph1 ph2 =
    C.TaskChain R.Error V.VaultUpdate { ph1 | baseUrl : () } { ph2 | baseUrl : () }


{-| Make an HTTP request that adheres to the Matrix spec rules.
-}
request :
    { attributes : List (R.Attribute { ph1 | baseUrl : () })
    , coder : Json.Coder returnValue
    , contextChange : V.VaultUpdate -> (APIContext { ph1 | baseUrl : () } -> APIContext { ph2 | baseUrl : () })
    , method : String
    , path : List String
    , toUpdate : returnValue -> ( V.VaultUpdate, List Log )
    }
    -> TaskChain ph1 ph2
request data =
    R.toChain
        { logHttp =
            \r ->
                ( V.HttpRequest r
                , String.concat
                    -- TODO: Move this to Internal.Config.Text module
                    [ "Matrix HTTP: "
                    , r.method
                    , " "
                    , r.url
                    ]
                    |> log.info
                    |> List.singleton
                )
        , coder = data.coder
        , request =
            R.callAPI
                { method = data.method
                , path = data.path
                }
                |> R.withAttributes data.attributes
        , toContextChange = data.contextChange
        , toUpdate = data.toUpdate
        }


{-| This type allows different definitions for different spec versions,
allowing the Elm SDK to communicate differently to the server depending on
how up-to-date the server is.
-}
type VersionControl a ph1 ph2
    = VC
        { name : VersionType
        , chain : a -> TaskChain (WithV ph1) (WithV ph2)
        , prev : Maybe (VersionControl a ph1 ph2)
        }


type VersionType
    = SpecVersion String
    | UnstableFeature String


type alias WithV ph =
    { ph | versions : () }


{-| Start with a given spec version supporting a given API endpoint.
-}
startWithVersion : String -> (a -> TaskChain (WithV ph1) (WithV ph2)) -> VersionControl a ph1 ph2
startWithVersion name chain =
    VC
        { name = SpecVersion name
        , chain = chain
        , prev = Nothing
        }


{-| Start with a given unstable feature supporting a given API endpoint.
-}
startWithUnstableFeature : String -> (a -> TaskChain (WithV ph1) (WithV ph2)) -> VersionControl a ph1 ph2
startWithUnstableFeature name chain =
    VC
        { name = UnstableFeature name
        , chain = chain
        , prev = Nothing
        }


{-| Add a new unstable feature that supports a different version of the API endpoint.
-}
forUnstableFeature : String -> (a -> TaskChain (WithV ph1) (WithV ph2)) -> VersionControl a ph1 ph2 -> VersionControl a ph1 ph2
forUnstableFeature name chain prev =
    VC
        { name = UnstableFeature name
        , chain = chain
        , prev = Just prev
        }


{-| Add a new spec version that supports a different version of the API endpoint.
-}
forVersion : String -> (a -> TaskChain (WithV ph1) (WithV ph2)) -> VersionControl a ph1 ph2 -> VersionControl a ph1 ph2
forVersion name chain prev =
    VC
        { name = SpecVersion name
        , chain = chain
        , prev = Just prev
        }


{-| Add another spec version that has the API endpoint defined the same as the previous API endpoint.
-}
sameForVersion : String -> VersionControl a ph1 ph2 -> VersionControl a ph1 ph2
sameForVersion name (VC data) =
    VC
        { name = SpecVersion name
        , chain = data.chain
        , prev = Just (VC data)
        }


supportedVersion : Versions -> VersionType -> Bool
supportedVersion { versions, unstableFeatures } name =
    case name of
        SpecVersion n ->
            List.member n versions

        UnstableFeature n ->
            Set.member n unstableFeatures



-- NOTE: Interesting detail! For some reason, I cannot add the `context`
-- NOTE: variable to the top line of the defined input values!
-- NOTE: Maybe this is a bug?


{-| Once you are done, turn a VersionControl type into a Task Chain.
-}
versionChain : VersionControl a ph1 ph2 -> a -> TaskChain (WithV ph1) (WithV ph2)
versionChain vc input =
    \context ->
        case Context.getVersions context of
            versions ->
                Recursion.runRecursion
                    (\mvc ->
                        case mvc of
                            Nothing ->
                                Recursion.base (C.fail R.NoSupportedVersion context)

                            Just (VC data) ->
                                if supportedVersion versions data.name then
                                    Recursion.base (data.chain input context)

                                else
                                    Recursion.recurse data.prev
                    )
                    (Just vc)
