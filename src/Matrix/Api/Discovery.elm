module Matrix.Api.Discovery exposing
    ( discoverHomeserver, getSupportInfo
    , SupportInfo, SupportContact
    , supportContactDecoder, supportInfoDecoder
    )

{-| Discovery endpoints.

Use these functions to turn a human domain into a homeserver URL and to fetch
support information for that domain.


## Discovery

@docs discoverHomeserver, getSupportInfo


## Support Types

@docs SupportInfo, SupportContact


## JSON

@docs supportContactDecoder, supportInfoDecoder

-}

import Internal.Tools.DecodeExtra as DE
import Json.Decode as D
import Matrix.Http exposing (Error)
import Matrix.Types.DiscoveryInformation as DiscoveryInformation exposing (DiscoveryInformation)
import Matrix.Types.HomeserverUrl as HomeserverUrl
import Matrix.Types.UserId as UserId exposing (UserId)
import Url exposing (Url)


{-| Discover the homeserver for a given domain.

For example, if you pass `https://matrix.org`, this endpoint tells you which
homeserver URL the domain uses.

    import Matrix.Api.Discovery as Discovery

    Discovery.discoverHomeserver
        { url = Url.fromString "https://matrix.org" |> Maybe.withDefault dummy
        , toMsg = GotDiscovery
        }

-}
discoverHomeserver :
    { url : Url
    , toMsg : Result Error DiscoveryInformation -> msg
    }
    -> Cmd msg
discoverHomeserver data =
    Matrix.Http.get
        { accessToken = Nothing
        , baseUrl = domainRoot data.url
        , path = [ ".well-known", "matrix", "client" ]
        , expect =
            Matrix.Http.expectJson data.toMsg DiscoveryInformation.decoder
        }


{-| The root of a domain. The `.well-known` files always live at the root,
even when the URL you were given has a path.
-}
domainRoot : Url -> HomeserverUrl.HomeserverUrl
domainRoot url =
    HomeserverUrl.fromUrl { url | path = "/" }


{-| Fetch support information for a domain.

This endpoint lives on the domain itself, not on the homeserver URL, so you can
use it before login.

    import Matrix.Api.Discovery as Discovery

    Discovery.getSupportInfo
        { url = Url.fromString "https://example.org" |> Maybe.withDefault dummy
        , toMsg = GotSupport
        }

-}
getSupportInfo :
    { url : Url
    , toMsg : Result Error SupportInfo -> msg
    }
    -> Cmd msg
getSupportInfo data =
    Matrix.Http.get
        { accessToken = Nothing
        , baseUrl = domainRoot data.url
        , path = [ ".well-known", "matrix", "support" ]
        , expect = Matrix.Http.expectJson data.toMsg supportInfoDecoder
        }


{-| Decode a support contact.

Exposed for customization if you want to decode only parts of the response.

-}
supportContactDecoder : D.Decoder SupportContact
supportContactDecoder =
    D.map3 SupportContact
        (DE.opField "email_address" D.string)
        (DE.opField "matrix_id" UserId.decoder)
        (D.field "role" D.string)


{-| Decode a support response payload.
-}
supportInfoDecoder : D.Decoder SupportInfo
supportInfoDecoder =
    D.map2 SupportInfo
        (D.list supportContactDecoder
            |> DE.opFieldWithDefault "contacts" []
        )
        (D.string
            |> D.andThen
                (\s ->
                    case Url.fromString s of
                        Just url ->
                            D.succeed url

                        Nothing ->
                            D.fail "Could not decode support_page URL"
                )
            |> DE.opField "support_page"
        )


{-| Response payload for `getSupportInfo`.

`contacts` lists support contacts; `supportPage` is an optional URL to a human
readable support page.

-}
type alias SupportInfo =
    { contacts : List SupportContact
    , supportPage : Maybe Url
    }


{-| A support contact returned by `getSupportInfo`.

The `matrixUser` field may be absent if the server only provides email.

-}
type alias SupportContact =
    { emailAddress : Maybe String
    , matrixUser : Maybe UserId
    , role : String
    }
