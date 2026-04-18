module Matrix.Api.Internal.GetEvent exposing
    ( EndpointProfile
    , latestProfile
    , selectProfile
    )

{-| Version-specific behavior for the client-server get-event endpoint.
-}

import Matrix.Api.Internal.VersionedEndpoint as VersionedEndpoint exposing (Match(..), Variant)
import Matrix.Types.ClientEvent as ClientEvent
import Matrix.Types.ServerVersion exposing (ServerVersion)


{-| The parts of get-event that can vary across spec versions.
-}
type alias EndpointProfile =
    { clientPathVersion : String
    , decoderProfile : ClientEvent.DecoderProfile
    }


latestProfile : EndpointProfile
latestProfile =
    { clientPathVersion = "v3"
    , decoderProfile = ClientEvent.latestDecoderProfile
    }


selectProfile : ServerVersion -> EndpointProfile
selectProfile serverVersion =
    VersionedEndpoint.selectVariant serverVersion variants
        |> Maybe.withDefault latestProfile


variants : List (Variant EndpointProfile)
variants =
    [ { matches =
            [ StableVersion "v1.11"
            , StableVersion "v1.12"
            , StableVersion "v1.13"
            , StableVersion "v1.14"
            , StableVersion "v1.15"
            , StableVersion "v1.16"
            , StableVersion "v1.17"
            , StableVersion "v1.18"
            ]
      , value = latestProfile
      }
    , { matches =
            [ StableVersion "v1.1"
            , StableVersion "v1.2"
            , StableVersion "v1.3"
            , StableVersion "v1.4"
            , StableVersion "v1.5"
            , StableVersion "v1.6"
            , StableVersion "v1.7"
            , StableVersion "v1.8"
            , StableVersion "v1.9"
            , StableVersion "v1.10"
            ]
      , value =
            { clientPathVersion = "v3"
            , decoderProfile = { includeMembership = False }
            }
      }
    , { matches =
            [ StableVersion "r0.5.0"
            , StableVersion "r0.6.0"
            , StableVersion "r0.6.1"
            ]
      , value =
            { clientPathVersion = "r0"
            , decoderProfile = { includeMembership = False }
            }
      }
    ]
