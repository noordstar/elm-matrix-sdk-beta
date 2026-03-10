module Matrix.Types.DiscoveryInformation exposing
    ( DiscoveryInformation, get, homeserver, identityServer
    , decoder
    )

{-| Discovery information.

This is typically decoded from `/.well-known/matrix/client`.


## DiscoveryInformation

@docs DiscoveryInformation, get, homeserver, identityServer


## JSON

@docs decoder

-}

import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Matrix.Types.HomeserverUrl as HomeserverUrl exposing (HomeserverUrl)


{-| Matrix client discovery data.

`homeserver` is the primary server your client should talk to.
`identityServer` is optional and may be absent.
`keys` keeps all raw discovery keys so you can access extra vendor fields.

-}
type DiscoveryInformation
    = DiscoveryInformation
        { homeserver : HomeserverUrl
        , identityServer : Maybe HomeserverUrl
        , keys : Dict String E.Value
        }


{-| Decode discovery information from JSON.

    import Json.Decode as D
    import Matrix.Types.DiscoveryInformation as Discovery

    D.decodeString Discovery.decoder "{ \"m.homeserver\": { \"base_url\": \"https://example.org\" } }"
    -- Ok <discovery>

Note: `identityServer` is optional, so it may be `Nothing`.

-}
decoder : D.Decoder DiscoveryInformation
decoder =
    D.map3
        (\home ident keys ->
            DiscoveryInformation
                { homeserver = home
                , identityServer = ident
                , keys = keys
                }
        )
        (D.at [ "m.homeserver", "base_url" ] HomeserverUrl.decoder)
        (D.at [ "m.identity_server", "base_url" ] HomeserverUrl.decoder |> D.maybe)
        (D.dict D.value)


{-| Read a raw key from the original discovery object.

This lets you access vendor-specific fields without losing the standard ones.

-}
get : String -> DiscoveryInformation -> Maybe E.Value
get key (DiscoveryInformation data) =
    Dict.get key data.keys


{-| Get the discovered homeserver URL.

    homeserver discovery

-}
homeserver : DiscoveryInformation -> HomeserverUrl
homeserver (DiscoveryInformation data) =
    data.homeserver


{-| Get the discovered identity server URL, if present.

    identityServer discovery
    -- Maybe HomeserverUrl

-}
identityServer : DiscoveryInformation -> Maybe HomeserverUrl
identityServer (DiscoveryInformation data) =
    data.identityServer
