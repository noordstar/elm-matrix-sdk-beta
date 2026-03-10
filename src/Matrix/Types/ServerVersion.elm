module Matrix.Types.ServerVersion exposing
    ( ServerVersion, hasUnstableFeature, hasVersion
    , decoder
    )

{-| Homeserver versions and feature flags.

This usually comes from the `/versions` endpoint.


## ServerVersion

@docs ServerVersion, hasUnstableFeature, hasVersion


## JSON

@docs decoder

-}

import Json.Decode as D
import Set exposing (Set)


{-| Capability information returned by a homeserver.

`versions` lists stable Matrix spec versions the server supports.
`unstableFeatures` contains enabled feature identifiers from
`unstable_features`.

-}
type ServerVersion
    = ServerVersion
        { unstableFeatures : Set String
        , versions : List String
        }


{-| Decode server capability information from `/versions`.

    import Json.Decode as D
    import Matrix.Types.ServerVersion as ServerVersion

    D.decodeString ServerVersion.decoder "{ \"versions\": [\"v1.11\"], \"unstable_features\": {} }"
    -- Ok <serverVersion>

-}
decoder : D.Decoder ServerVersion
decoder =
    D.map2
        (\u v -> ServerVersion { unstableFeatures = u, versions = v })
        (D.field "unstable_features" (D.keyValuePairs D.bool)
            |> D.map (List.filter Tuple.second)
            |> D.map (List.map Tuple.first)
            |> D.map Set.fromList
        )
        (D.field "versions" <| D.list D.string)


{-| Check whether an unstable feature flag is enabled.

    ServerVersion.hasUnstableFeature "org.matrix.mscXXXX" server

-}
hasUnstableFeature : String -> ServerVersion -> Bool
hasUnstableFeature uf (ServerVersion { unstableFeatures }) =
    Set.member uf unstableFeatures


{-| Check whether a specific Matrix version is supported.

Example version strings include `"r0.6.1"` and `"v1.11"`.

    ServerVersion.hasVersion "v1.11" server

-}
hasVersion : String -> ServerVersion -> Bool
hasVersion v (ServerVersion { versions }) =
    List.member v versions
