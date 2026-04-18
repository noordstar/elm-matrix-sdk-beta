module Matrix.Api.Internal.VersionedEndpoint exposing
    ( Match(..)
    , Variant
    , selectVariant
    )

{-| Helpers for choosing an endpoint variant from advertised server versions.
-}

import Matrix.Types.ServerVersion as ServerVersion exposing (ServerVersion)


{-| A compatibility rule for a versioned endpoint variant.
-}
type Match
    = StableVersion String
    | UnstableFeature String


{-| A single endpoint variant and the version rules that enable it.
-}
type alias Variant a =
    { matches : List Match
    , value : a
    }


{-| Pick the first compatible variant from a newest-first list.

If none of the variants are compatible, the newest variant is returned.

-}
selectVariant : ServerVersion -> List (Variant a) -> Maybe a
selectVariant serverVersion variants =
    case List.filter (supports serverVersion << .matches) variants of
        variant :: _ ->
            Just variant.value

        [] ->
            List.head variants |> Maybe.map .value


supports : ServerVersion -> List Match -> Bool
supports serverVersion =
    List.any
        (\match ->
            case match of
                StableVersion version ->
                    ServerVersion.hasVersion version serverVersion

                UnstableFeature feature ->
                    ServerVersion.hasUnstableFeature feature serverVersion
        )
