module Matrix.Api.Tokens exposing
    ( refreshTokens, RefreshTokensResponse
    , refreshTokensResponseDecoder
    )

{-| Token refresh endpoints.

Use these when you have a refresh token and want a new access token.


## Tokens

@docs refreshTokens, RefreshTokensResponse


## JSON

@docs refreshTokensResponseDecoder

-}

import Duration exposing (Duration)
import Internal.Tools.DecodeExtra as DE
import Json.Decode as D
import Json.Encode as E
import Matrix.Http exposing (Error)
import Matrix.Types.AccessToken as AccessToken exposing (AccessToken)
import Matrix.Types.HomeserverUrl as HomeserverUrl exposing (HomeserverUrl)
import Matrix.Types.RefreshToken as RefreshToken exposing (RefreshToken)


{-| Exchange a refresh token for a new access token.

    import Matrix.Api.Tokens as Tokens

    Tokens.refreshTokens
        { baseUrl = homeserver
        , refreshToken = token
        , toMsg = GotTokens
        }

-}
refreshTokens :
    { baseUrl : HomeserverUrl
    , refreshToken : RefreshToken
    , toMsg : Result Error RefreshTokensResponse -> msg
    }
    -> Cmd msg
refreshTokens data =
    Matrix.Http.post
        { accessToken = Nothing
        , baseUrl = data.baseUrl
        , path = [ "_matrix", "client", "v3", "refresh" ]
        , body =
            E.object
                [ ( "refresh_token", RefreshToken.encode data.refreshToken ) ]
        , expect =
            Matrix.Http.expectJson data.toMsg refreshTokensResponseDecoder
        }


{-| Decode a refresh-token response.
-}
refreshTokensResponseDecoder : D.Decoder RefreshTokensResponse
refreshTokensResponseDecoder =
    D.map3 RefreshTokensResponse
        (D.field "access_token" AccessToken.decoder)
        (DE.opField "expires_in_ms" (D.map (toFloat >> Duration.milliseconds) D.int))
        (DE.opField "refresh_token" RefreshToken.decoder)


{-| Response returned when refreshing an access token.
-}
type alias RefreshTokensResponse =
    { accessToken : AccessToken
    , expiresIn : Maybe Duration
    , refreshToken : Maybe RefreshToken
    }
