module Matrix.Api.Login exposing
    ( listLoginMethods, loginWithPassword, loginWithToken, login
    , LoginFlow, LoginResponse, LoginIdentifier(..), encodeLoginIdentifier
    , loginFlowDecoder, loginResponseDecoder
    )

{-| Login endpoints.

These functions guide you from available login flows to an authenticated
session. The simpler helpers come first; use `login` only when you need full
control.


## Login

@docs listLoginMethods, loginWithPassword, loginWithToken, login


## Types

@docs LoginFlow, LoginResponse, LoginIdentifier, encodeLoginIdentifier


## JSON

@docs loginFlowDecoder, loginResponseDecoder

-}

import Duration exposing (Duration)
import Internal.Tools.DecodeExtra as DE
import Internal.Tools.EncodeExtra as EE
import Json.Decode as D
import Json.Encode as E
import Matrix.Http exposing (Error)
import Matrix.Types.AccessToken as AccessToken exposing (AccessToken)
import Matrix.Types.DeviceId as DeviceId exposing (DeviceId)
import Matrix.Types.DiscoveryInformation as DiscoveryInformation exposing (DiscoveryInformation)
import Matrix.Types.HomeserverUrl as HomeserverUrl exposing (HomeserverUrl)
import Matrix.Types.RefreshToken as RefreshToken exposing (RefreshToken)
import Matrix.Types.UserId as UserId exposing (UserId)


{-| Fetch the list of supported login flows.

    import Matrix.Api.Login as Login

    Login.listLoginMethods
        { baseUrl = homeserver
        , toMsg = GotFlows
        }

-}
listLoginMethods :
    { baseUrl : HomeserverUrl
    , toMsg : Result Error (List LoginFlow) -> msg
    }
    -> Cmd msg
listLoginMethods data =
    Matrix.Http.get
        { accessToken = Nothing
        , baseUrl = data.baseUrl
        , path = [ "_matrix", "client", "v3", "login" ]
        , expect =
            D.list loginFlowDecoder
                |> D.field "flows"
                |> Matrix.Http.expectJson data.toMsg
        }


{-| Log in with a password-based flow.

This is a thin wrapper around `login` that sets `type_` to
`"m.login.password"`.

    import Matrix.Api.Login as Login

    Login.loginWithPassword
        { baseUrl = homeserver
        , deviceId = Nothing
        , identifier = MatrixUser { username = "alice" }
        , initialDeviceDisplayName = Just "Elm Client"
        , password = "secret"
        , supportsRefreshTokens = Just True
        , toMsg = GotLogin
        }

-}
loginWithPassword :
    { baseUrl : HomeserverUrl
    , deviceId : Maybe DeviceId
    , identifier : LoginIdentifier
    , initialDeviceDisplayName : Maybe String
    , password : String
    , supportsRefreshTokens : Maybe Bool
    , toMsg : Result Error LoginResponse -> msg
    }
    -> Cmd msg
loginWithPassword data =
    login
        { baseUrl = data.baseUrl
        , deviceId = data.deviceId
        , identifier = data.identifier
        , initialDeviceDisplayName = data.initialDeviceDisplayName
        , password = Just data.password
        , supportsRefreshTokens = data.supportsRefreshTokens
        , token = Nothing
        , type_ = "m.login.password"
        , toMsg = data.toMsg
        }


{-| Log in with a token-based flow.

This is a thin wrapper around `login` that sets `type_` to
`"m.login.token"`.

    import Matrix.Api.Login as Login

    Login.loginWithToken
        { baseUrl = homeserver
        , deviceId = Nothing
        , identifier = MatrixUser { username = "alice" }
        , initialDeviceDisplayName = Nothing
        , supportsRefreshTokens = Just True
        , token = "token"
        , toMsg = GotLogin
        }

-}
loginWithToken :
    { baseUrl : HomeserverUrl
    , deviceId : Maybe DeviceId
    , identifier : LoginIdentifier
    , initialDeviceDisplayName : Maybe String
    , supportsRefreshTokens : Maybe Bool
    , token : String
    , toMsg : Result Error LoginResponse -> msg
    }
    -> Cmd msg
loginWithToken data =
    login
        { baseUrl = data.baseUrl
        , deviceId = data.deviceId
        , identifier = data.identifier
        , initialDeviceDisplayName = data.initialDeviceDisplayName
        , password = Nothing
        , supportsRefreshTokens = data.supportsRefreshTokens
        , token = Just data.token
        , type_ = "m.login.token"
        , toMsg = data.toMsg
        }


{-| Perform a login request with a fully custom payload.

Use this when you need full control over the login `type_` and fields.

    import Matrix.Api.Login as Login

    Login.login
        { baseUrl = homeserver
        , deviceId = Nothing
        , identifier = MatrixUser { username = "alice" }
        , initialDeviceDisplayName = Nothing
        , password = Just "secret"
        , supportsRefreshTokens = Just True
        , token = Nothing
        , type_ = "m.login.password"
        , toMsg = GotLogin
        }

-}
login :
    { baseUrl : HomeserverUrl
    , deviceId : Maybe DeviceId
    , identifier : LoginIdentifier
    , initialDeviceDisplayName : Maybe String
    , password : Maybe String
    , supportsRefreshTokens : Maybe Bool
    , token : Maybe String
    , type_ : String
    , toMsg : Result Error LoginResponse -> msg
    }
    -> Cmd msg
login data =
    Matrix.Http.post
        { accessToken = Nothing
        , baseUrl = data.baseUrl
        , path = [ "_matrix", "client", "v3", "login" ]
        , body =
            EE.maybeObject
                [ ( "device_id", Maybe.map DeviceId.encode data.deviceId )
                , ( "identifier", Just <| encodeLoginIdentifier data.identifier )
                , ( "initial_device_display_name", Maybe.map E.string data.initialDeviceDisplayName )
                , ( "password", Maybe.map E.string data.password )
                , ( "refresh_token", Maybe.map E.bool data.supportsRefreshTokens )
                , ( "token", Maybe.map E.string data.token )
                , ( "type", Just <| E.string data.type_ )
                ]
        , expect = Matrix.Http.expectJson data.toMsg loginResponseDecoder
        }


{-| Decode a login flow returned by `/login`.
-}
loginFlowDecoder : D.Decoder LoginFlow
loginFlowDecoder =
    D.map2 LoginFlow (DE.opField "get_login_token" D.bool) (D.field "type" D.string)


{-| Decode a login response.
-}
loginResponseDecoder : D.Decoder LoginResponse
loginResponseDecoder =
    D.map6 LoginResponse
        (D.field "access_token" AccessToken.decoder)
        (D.field "device_id" DeviceId.decoder)
        (DE.opField "expires_in_ms" (D.map (toFloat >> Duration.milliseconds) D.int))
        (DE.opField "refresh_token" RefreshToken.decoder)
        (D.field "user_id" UserId.decoder)
        (DE.opField "well_known" DiscoveryInformation.decoder)


{-| A login flow advertised by the server.

You can use these flows to choose between password, SSO, token logins, and
more.

-}
type alias LoginFlow =
    { supportsGetLoginToken : Maybe Bool
    , type_ : String
    }


{-| Response returned by a successful login.

It includes the access token, device ID, and the authenticated user ID.

-}
type alias LoginResponse =
    { accessToken : AccessToken
    , deviceId : DeviceId
    , expiresIn : Maybe Duration
    , refreshToken : Maybe RefreshToken
    , userId : UserId
    , wellKnown : Maybe DiscoveryInformation
    }


{-| Identify which account should log in.

Most clients use `MatrixUser`, but you can also log in via third-party or phone
identifiers.

-}
type LoginIdentifier
    = MatrixUser { username : String }
    | ThirdParty { medium : String, address : String }
    | PhoneNumber { country : String, phoneNumber : String }
    | OtherIdentifier E.Value


{-| Encode a login identifier as JSON.
-}
encodeLoginIdentifier : LoginIdentifier -> E.Value
encodeLoginIdentifier i =
    case i of
        MatrixUser { username } ->
            E.object
                [ ( "type", E.string "m.id.user" )
                , ( "user", E.string username )
                ]

        ThirdParty { medium, address } ->
            E.object
                [ ( "type", E.string "m.id.thirdparty" )
                , ( "medium", E.string medium )
                , ( "address", E.string address )
                ]

        PhoneNumber { country, phoneNumber } ->
            E.object
                [ ( "type", E.string "m.id.phone" )
                , ( "country", E.string country )
                , ( "phone", E.string phoneNumber )
                ]

        OtherIdentifier v ->
            v
