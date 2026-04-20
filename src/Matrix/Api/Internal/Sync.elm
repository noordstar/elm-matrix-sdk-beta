module Matrix.Api.Internal.Sync exposing
    ( DecoderProfile
    , PrevContentLocation(..)
    , latestDecoderProfile
    , selectDecoderProfile
    )

{-| Version-specific behavior for the client-server sync endpoint.
-}

import Matrix.Api.Internal.VersionedEndpoint as VersionedEndpoint exposing (Match(..), Variant)
import Matrix.Types.ServerVersion exposing (ServerVersion)


type PrevContentLocation
    = PrevContentTopLevel
    | PrevContentUnsigned


type alias DecoderProfile =
    { requiresFallbackKeyTypes : Bool
    , supportsUnreadThreadNotifications : Bool
    , supportsUnsignedMembership : Bool
    , prevContentLocation : PrevContentLocation
    }


latestDecoderProfile : DecoderProfile
latestDecoderProfile =
    { requiresFallbackKeyTypes = True
    , supportsUnreadThreadNotifications = True
    , supportsUnsignedMembership = True
    , prevContentLocation = PrevContentUnsigned
    }


selectDecoderProfile : ServerVersion -> DecoderProfile
selectDecoderProfile serverVersion =
    VersionedEndpoint.selectVariant serverVersion variants
        |> Maybe.withDefault latestDecoderProfile


variants : List (Variant DecoderProfile)
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
      , value = latestDecoderProfile
      }
    , { matches =
            [ StableVersion "v1.4"
            , StableVersion "v1.5"
            , StableVersion "v1.6"
            , StableVersion "v1.7"
            , StableVersion "v1.8"
            , StableVersion "v1.9"
            , StableVersion "v1.10"
            ]
      , value =
            { requiresFallbackKeyTypes = True
            , supportsUnreadThreadNotifications = True
            , supportsUnsignedMembership = False
            , prevContentLocation = PrevContentUnsigned
            }
      }
    , { matches =
            [ StableVersion "v1.2"
            , StableVersion "v1.3"
            ]
      , value =
            { requiresFallbackKeyTypes = True
            , supportsUnreadThreadNotifications = False
            , supportsUnsignedMembership = False
            , prevContentLocation = PrevContentUnsigned
            }
      }
    , { matches = [ StableVersion "v1.1" ]
      , value =
            { requiresFallbackKeyTypes = False
            , supportsUnreadThreadNotifications = False
            , supportsUnsignedMembership = False
            , prevContentLocation = PrevContentTopLevel
            }
      }
    ]
