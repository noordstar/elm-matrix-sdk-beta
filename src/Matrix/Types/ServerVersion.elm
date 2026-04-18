module Matrix.Types.ServerVersion exposing
    ( ServerVersion, naive, hasUnstableFeature, hasVersion
    , decoder
    )

{-| The `ServerVersion` describes the Matrix server's version and communication
flag. This allows the Matrix SDK to remain up-to-date with the latest versions
while remaining fully backwards compatible.

In general, you should download this from the `/versions` endpoint. See
[the fetchServerVersion function](Matrix.Api.Versions#fetchServerVersion).


## ServerVersion

@docs ServerVersion, naive, hasUnstableFeature, hasVersion


## JSON

@docs decoder

-}

import Json.Decode as D
import Set exposing (Set)


{-| Capability information returned by a homeserver.
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


{-| Naive type containing zero information about a Matrix server.

You can use this type if you cannot
[download the information](Matrix.Api.Versions#fetchServerVersion) or if the
Matrix server cannot tell you its capabilities for some reason. In general,
the Matrix SDK will communicate in the version that it thinks is the most
likely to be understood by the Matrix server.

-}
naive : ServerVersion
naive =
    ServerVersion { unstableFeatures = Set.empty, versions = [] }
