module Matrix.Api.Authorization exposing
    ( fetchAuthorizationMetadata, AuthorizationServerMetadata
    , authorizationServerMetadataDecoder
    )

{-| Authorization metadata.

This is the OpenID metadata published by a Matrix homeserver.


## Authorization

@docs fetchAuthorizationMetadata, AuthorizationServerMetadata


## JSON

@docs authorizationServerMetadataDecoder

-}

import Internal.Tools.DecodeExtra as DE
import Json.Decode as D
import Matrix.Http exposing (Error)
import Matrix.Types.HomeserverUrl as HomeserverUrl exposing (HomeserverUrl)
import Url exposing (Url)


{-| Fetch OpenID authorization server metadata.

    import Matrix.Api.Authorization as Authorization

    Authorization.fetchAuthorizationMetadata
        { baseUrl = homeserver
        , toMsg = GotMetadata
        }

-}
fetchAuthorizationMetadata :
    { baseUrl : HomeserverUrl
    , toMsg : Result Error AuthorizationServerMetadata -> msg
    }
    -> Cmd msg
fetchAuthorizationMetadata data =
    Matrix.Http.get
        { accessToken = Nothing
        , baseUrl = data.baseUrl
        , path = [ "_matrix", "client", "v1", "auth_metadata" ]
        , expect = Matrix.Http.expectJson data.toMsg authorizationServerMetadataDecoder
        }


{-| Decode authorization server metadata.
-}
authorizationServerMetadataDecoder : D.Decoder AuthorizationServerMetadata
authorizationServerMetadataDecoder =
    DE.map10 AuthorizationServerMetadata
        (D.field "authorization_endpoint" Matrix.Http.urlDecoder)
        (D.field "code_challenge_methods_supported" <| D.list D.string)
        (D.field "grant_types_supported" <| D.list D.string)
        (D.field "issuer" Matrix.Http.urlDecoder)
        (DE.opField "prompt_values_supported" <| D.list D.string)
        (D.field "registration_endpoint" Matrix.Http.urlDecoder)
        (D.field "response_modes_supported" <| D.list D.string)
        (D.field "response_types_supported" <| D.list D.string)
        (D.field "revocation_endpoint" Matrix.Http.urlDecoder)
        (D.field "token_endpoint" Matrix.Http.urlDecoder)


{-| OpenID authorization server metadata.

This is returned by the `/_matrix/client/v1/auth_metadata` endpoint.

-}
type alias AuthorizationServerMetadata =
    { authorizationEndpoint : Url
    , codeChallengeMethodsSupported : List String
    , grantTypesSupported : List String
    , issuer : Url
    , promptValuesSupported : Maybe (List String)
    , registrationEndpoint : Url
    , responseModesSupported : List String
    , responseTypesSupported : List String
    , revocationEndpoint : Url
    , tokenEndpoint : Url
    }
