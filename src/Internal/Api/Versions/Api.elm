module Internal.Api.Versions.Api exposing (versions, Phantom)

{-|


# Versions

Ask the Matrix API which versions it supports.

@docs versions, Phantom

-}

import Dict
import Internal.Api.Api as A
import Internal.Tools.Json as Json
import Internal.Values.Context as Context exposing (Versions)
import Internal.Values.Envelope as E
import Set


{-| Task chain to ask which spec versions the Matrix API supports.
-}
versions : A.TaskChain (Phantom ph) (Phantom { ph | versions : () })
versions =
    A.request
        { attributes = []
        , coder = versionsCoder
        , contextChange = Context.setVersions
        , method = "GET"
        , path = [ "_matrix", "client", "versions" ]
        , toUpdate = \v -> ( E.SetVersions v, [] )
        }


{-| Context needed for asking the server's available spec versions
-}
type alias Phantom a =
    { a | baseUrl : () }


versionsCoder : Json.Coder Versions
versionsCoder =
    Json.object2
        { name = "Versions"
        , description =
            [ "Gets the versions of the specification supported by the server."
            , "Values will take the form vX.Y or rX.Y.Z in historical cases. See the Specification Versioning for more information."
            , "The server may additionally advertise experimental features it supports through unstable_features. These features should be namespaced and may optionally include version information within their name if desired. Features listed here are not for optionally toggling parts of the Matrix specification and should only be used to advertise support for a feature which has not yet landed in the spec. For example, a feature currently undergoing the proposal process may appear here and eventually be taken off this list once the feature lands in the spec and the server deems it reasonable to do so. Servers can choose to enable some features only for some users, so clients should include authentication in the request to get all the features available for the logged-in user. If no authentication is provided, the server should only return the features available to all users. Servers may wish to keep advertising features here after they’ve been released into the spec to give clients a chance to upgrade appropriately. Additionally, clients should avoid using unstable features in their stable releases."
            ]
        , init = Versions
        }
        (Json.field.required
            { fieldName = "versions"
            , toField = .versions
            , description =
                [ "The supported versions."
                ]
            , coder = Json.list Json.string
            }
        )
        (Json.field.optional.withDefault
            { fieldName = "unstable_features"
            , toField = .unstableFeatures
            , description =
                [ "Experimental features the server supports. Features not listed here, or the lack of this property all together, indicate that a feature is not supported."
                ]
            , coder =
                Json.bool
                    |> Json.slowDict
                    |> Json.map
                        { name = "Dict to set"
                        , description =
                            [ "Turn a dictionary of supported values into a set that contains only supported values"
                            ]
                        , back = Set.foldl (\k d -> Dict.insert k True d) Dict.empty
                        , forth =
                            Dict.foldl
                                (\k v s ->
                                    if v then
                                        Set.insert k s

                                    else
                                        s
                                )
                                Set.empty
                        }
            , default = ( Set.empty, [] )
            }
        )
