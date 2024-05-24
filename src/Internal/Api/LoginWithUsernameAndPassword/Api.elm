module Internal.Api.LoginWithUsernameAndPassword.Api exposing (Phantom, loginWithUsernameAndPassword)

{-|


# Login

This module allows the user to log in using a username and password.

@docs Phantom, loginWithUsernameAndPassword

-}

import Internal.Api.Api as A
import Internal.Api.Request as R
import Internal.Config.Leaks as L
import Internal.Tools.Json as Json
import Internal.Values.Context as Context
import Internal.Values.Envelope as E
import Internal.Values.User as User exposing (User)
import Internal.Values.Vault as V
import Json.Encode as E


loginWithUsernameAndPassword : LoginWithUsernameAndPasswordInput -> A.TaskChain (Phantom a) (Phantom { a | accessToken : () })
loginWithUsernameAndPassword =
    A.startWithVersion "r0.0.0" loginWithUsernameAndPasswordV1
        |> A.sameForVersion "r0.0.1"
        |> A.sameForVersion "r0.1.0"
        |> A.sameForVersion "r0.2.0"
        |> A.forVersion "r0.3.0" loginWithUsernameAndPasswordV2
        |> A.forVersion "r0.4.0" loginWithUsernameAndPasswordV3
        |> A.forVersion "r0.5.0" loginWithUsernameAndPasswordV4
        |> A.sameForVersion "r0.6.0"
        |> A.sameForVersion "r0.6.1"
        |> A.forVersion "v1.1" loginWithUsernameAndPasswordV5
        |> A.sameForVersion "v1.2"
        |> A.forVersion "v1.3" loginWithUsernameAndPasswordV6
        |> A.forVersion "v1.4" loginWithUsernameAndPasswordV7
        |> A.sameForVersion "v1.5"
        |> A.sameForVersion "v1.6"
        |> A.sameForVersion "v1.7"
        |> A.sameForVersion "v1.8"
        |> A.sameForVersion "v1.9"
        |> A.sameForVersion "v1.10"
        |> A.versionChain


type alias Phantom a =
    { a | baseUrl : (), versions : () }


type alias LoginWithUsernameAndPasswordInput =
    { deviceId : Maybe String
    , enableRefreshToken : Maybe Bool
    , initialDeviceDisplayName : Maybe String
    , password : String
    , username : String
    }


type alias LoginWithUsernameAndPasswordInputV1 a =
    { a
        | password : String
        , username : String
    }


type alias LoginWithUsernameAndPasswordInputV2 a =
    { a
        | deviceId : Maybe String
        , initialDeviceDisplayName : Maybe String
        , password : String
        , username : String
    }


type alias LoginWithUsernameAndPasswordInputV3 a =
    { a
        | deviceId : Maybe String
        , enableRefreshToken : Maybe Bool
        , initialDeviceDisplayName : Maybe String
        , password : String
        , username : String
    }


type alias LoginWithUsernameAndPasswordOutputV1 =
    { accessToken : String -- Even though it is not required, we do not want it to be omitted.
    , homeserver : String
    , refreshToken : Maybe String
    , user : Maybe User
    }


type alias LoginWithUsernameAndPasswordOutputV2 =
    { accessToken : String -- Even though it is not required, we do not want it to be omitted.
    , deviceId : Maybe String
    , homeserver : String
    , user : Maybe User
    }


type alias LoginWithUsernameAndPasswordOutputV3 =
    { accessToken : String -- Even though it is not required, we do not want it to be omitted.
    , deviceId : Maybe String
    , homeserver : Maybe String
    , user : Maybe User
    }


type alias LoginWithUsernameAndPasswordOutputV4 =
    { accessToken : String -- Even though it is not required, we do not want it to be omitted.
    , deviceId : Maybe String
    , homeserver : Maybe String
    , user : Maybe User
    , wellKnown : Maybe DiscoveryInformationV1
    }


type alias LoginWithUsernameAndPasswordOutputV5 =
    { accessToken : String -- Even though it is not required, we do not want it to be omitted.
    , deviceId : Maybe String
    , expiresInMs : Maybe Int
    , homeserver : Maybe String
    , refreshToken : Maybe String
    , user : Maybe User
    , wellKnown : Maybe DiscoveryInformationV1
    }


type alias LoginWithUsernameAndPasswordOutputV6 =
    { accessToken : String
    , deviceId : String
    , expiresInMs : Maybe Int
    , homeserver : Maybe String
    , refreshToken : Maybe String
    , user : User
    , wellKnown : Maybe DiscoveryInformationV1
    }


type alias DiscoveryInformationV1 =
    { homeserver : HomeserverInformation
    , identityServer : Maybe IdentityServerInformation
    }


type alias HomeserverInformation =
    { baseUrl : String }


type alias IdentityServerInformation =
    { baseUrl : String }


type alias PhantomV1 a =
    { a | baseUrl : (), now : () }


loginWithUsernameAndPasswordV1 : LoginWithUsernameAndPasswordInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 { a | accessToken : () })
loginWithUsernameAndPasswordV1 { username, password } context =
    A.request
        { attributes =
            [ R.bodyString "password" password
            , R.bodyString "type" "m.login.password"
            , R.bodyString "user" username
            , R.onStatusCode 400 "M_UNKNOWN"
            , R.onStatusCode 403 "M_FORBIDDEN"
            ]
        , coder = coderV1
        , method = "POST"
        , path = [ "_matrix", "client", "r0", "login" ]
        , contextChange =
            \out -> Context.setAccessToken out.accessToken
        , toUpdate =
            \out ->
                ( E.More
                    [ E.SetAccessToken
                        { created = Context.getNow context
                        , expiryMs = Nothing
                        , lastUsed = Context.getNow context
                        , refresh = out.refreshToken
                        , value = out.accessToken
                        }
                    , out.user
                        |> Maybe.map (V.SetUser >> E.ContentUpdate)
                        |> E.Optional
                    ]
                , []
                )
        }
        context


loginWithUsernameAndPasswordV2 : LoginWithUsernameAndPasswordInputV2 i -> A.TaskChain (PhantomV1 a) (PhantomV1 { a | accessToken : () })
loginWithUsernameAndPasswordV2 { deviceId, initialDeviceDisplayName, username, password } context =
    A.request
        { attributes =
            [ R.bodyOpString "device_id" deviceId
            , R.bodyOpString "initial_device_display_name" initialDeviceDisplayName
            , R.bodyString "password" password
            , R.bodyString "type" "m.login.password"
            , R.bodyString "user" username
            , R.onStatusCode 400 "M_UNKNOWN"
            , R.onStatusCode 403 "M_FORBIDDEN"
            , R.onStatusCode 429 "string" -- Yup. That's what it says.
            ]
        , coder = coderV2
        , method = "POST"
        , path = [ "_matrix", "client", "r0", "login" ]
        , contextChange =
            \out -> Context.setAccessToken out.accessToken
        , toUpdate =
            \out ->
                ( E.More
                    [ E.SetAccessToken
                        { created = Context.getNow context
                        , expiryMs = Nothing
                        , lastUsed = Context.getNow context
                        , refresh = Nothing
                        , value = out.accessToken
                        }
                    , out.user
                        |> Maybe.map (V.SetUser >> E.ContentUpdate)
                        |> E.Optional
                    , out.deviceId
                        |> Maybe.map E.SetDeviceId
                        |> E.Optional
                    ]
                , []
                )
        }
        context


loginWithUsernameAndPasswordV3 : LoginWithUsernameAndPasswordInputV2 i -> A.TaskChain (PhantomV1 a) (PhantomV1 { a | accessToken : () })
loginWithUsernameAndPasswordV3 { deviceId, initialDeviceDisplayName, username, password } context =
    A.request
        { attributes =
            [ R.bodyOpString "address" Nothing
            , R.bodyOpString "device_id" deviceId
            , R.bodyValue "identifier"
                (E.object
                    [ ( "type", E.string "m.id.user" )
                    , ( "user", E.string username )
                    ]
                )
            , R.bodyOpString "initial_device_display_name" initialDeviceDisplayName
            , R.bodyString "password" password
            , R.bodyString "type" "m.login.password"
            , R.bodyString "user" username
            , R.onStatusCode 400 "M_UNKNOWN"
            , R.onStatusCode 403 "M_FORBIDDEN"
            , R.onStatusCode 429 "M_LIMIT_EXCEEDED"
            ]
        , coder = coderV3
        , method = "POST"
        , path = [ "_matrix", "client", "r0", "login" ]
        , contextChange =
            \out -> Context.setAccessToken out.accessToken
        , toUpdate =
            \out ->
                ( E.More
                    [ E.SetAccessToken
                        { created = Context.getNow context
                        , expiryMs = Nothing
                        , lastUsed = Context.getNow context
                        , refresh = Nothing
                        , value = out.accessToken
                        }
                    , out.user
                        |> Maybe.map (V.SetUser >> E.ContentUpdate)
                        |> E.Optional
                    , out.deviceId
                        |> Maybe.map E.SetDeviceId
                        |> E.Optional
                    ]
                , []
                )
        }
        context


loginWithUsernameAndPasswordV4 : LoginWithUsernameAndPasswordInputV2 i -> A.TaskChain (PhantomV1 a) (PhantomV1 { a | accessToken : () })
loginWithUsernameAndPasswordV4 { deviceId, initialDeviceDisplayName, username, password } context =
    A.request
        { attributes =
            [ R.bodyOpString "address" Nothing
            , R.bodyOpString "device_id" deviceId
            , R.bodyValue "identifier"
                (E.object
                    [ ( "type", E.string "m.id.user" )
                    , ( "user", E.string username )
                    ]
                )
            , R.bodyOpString "initial_device_display_name" initialDeviceDisplayName
            , R.bodyString "password" password
            , R.bodyString "type" "m.login.password"
            , R.bodyString "user" username
            , R.onStatusCode 400 "M_UNKNOWN"
            , R.onStatusCode 403 "M_FORBIDDEN"
            , R.onStatusCode 429 "M_LIMIT_EXCEEDED"
            ]
        , coder = coderV4
        , method = "POST"
        , path = [ "_matrix", "client", "r0", "login" ]
        , contextChange =
            \out -> Context.setAccessToken out.accessToken
        , toUpdate =
            \out ->
                ( E.More
                    [ E.SetAccessToken
                        { created = Context.getNow context
                        , expiryMs = Nothing
                        , lastUsed = Context.getNow context
                        , refresh = Nothing
                        , value = out.accessToken
                        }
                    , out.user
                        |> Maybe.map (V.SetUser >> E.ContentUpdate)
                        |> E.Optional
                    , out.wellKnown
                        |> Maybe.map (.homeserver >> .baseUrl)
                        |> Maybe.map E.SetBaseUrl
                        |> E.Optional
                    , out.deviceId
                        |> Maybe.map E.SetDeviceId
                        |> E.Optional
                    ]
                , []
                )
        }
        context


loginWithUsernameAndPasswordV5 : LoginWithUsernameAndPasswordInputV2 i -> A.TaskChain (PhantomV1 a) (PhantomV1 { a | accessToken : () })
loginWithUsernameAndPasswordV5 { deviceId, initialDeviceDisplayName, username, password } context =
    A.request
        { attributes =
            [ R.bodyOpString "address" Nothing
            , R.bodyOpString "device_id" deviceId
            , R.bodyValue "identifier"
                (E.object
                    [ ( "type", E.string "m.id.user" )
                    , ( "user", E.string username )
                    ]
                )
            , R.bodyOpString "initial_device_display_name" initialDeviceDisplayName
            , R.bodyString "password" password
            , R.bodyString "type" "m.login.password"
            , R.bodyString "user" username
            , R.onStatusCode 400 "M_UNKNOWN"
            , R.onStatusCode 403 "M_FORBIDDEN"
            , R.onStatusCode 429 "M_LIMIT_EXCEEDED"
            ]
        , coder = coderV4
        , method = "POST"
        , path = [ "_matrix", "client", "v3", "login" ]
        , contextChange =
            \out -> Context.setAccessToken out.accessToken
        , toUpdate =
            \out ->
                ( E.More
                    [ E.SetAccessToken
                        { created = Context.getNow context
                        , expiryMs = Nothing
                        , lastUsed = Context.getNow context
                        , refresh = Nothing
                        , value = out.accessToken
                        }
                    , out.user
                        |> Maybe.map (V.SetUser >> E.ContentUpdate)
                        |> E.Optional
                    , out.wellKnown
                        |> Maybe.map (.homeserver >> .baseUrl)
                        |> Maybe.map E.SetBaseUrl
                        |> E.Optional
                    , out.deviceId
                        |> Maybe.map E.SetDeviceId
                        |> E.Optional
                    ]
                , []
                )
        }
        context


loginWithUsernameAndPasswordV6 : LoginWithUsernameAndPasswordInputV3 i -> A.TaskChain (PhantomV1 a) (PhantomV1 { a | accessToken : () })
loginWithUsernameAndPasswordV6 { deviceId, enableRefreshToken, initialDeviceDisplayName, username, password } context =
    A.request
        { attributes =
            [ R.bodyOpString "address" Nothing
            , R.bodyOpString "device_id" deviceId
            , R.bodyValue "identifier"
                (E.object
                    [ ( "type", E.string "m.id.user" )
                    , ( "user", E.string username )
                    ]
                )
            , R.bodyOpString "initial_device_display_name" initialDeviceDisplayName
            , R.bodyString "password" password
            , R.bodyOpBool "refresh_token" enableRefreshToken
            , R.bodyString "type" "m.login.password"
            , R.bodyString "user" username
            , R.onStatusCode 400 "M_UNKNOWN"
            , R.onStatusCode 403 "M_FORBIDDEN"
            , R.onStatusCode 429 "M_LIMIT_EXCEEDED"
            ]
        , coder = coderV5
        , method = "POST"
        , path = [ "_matrix", "client", "v3", "login" ]
        , contextChange =
            \out -> Context.setAccessToken out.accessToken
        , toUpdate =
            \out ->
                ( E.More
                    [ E.SetAccessToken
                        { created = Context.getNow context
                        , expiryMs = out.expiresInMs
                        , lastUsed = Context.getNow context
                        , refresh = out.refreshToken
                        , value = out.accessToken
                        }
                    , out.user
                        |> Maybe.map (V.SetUser >> E.ContentUpdate)
                        |> E.Optional
                    , out.wellKnown
                        |> Maybe.map (.homeserver >> .baseUrl)
                        |> Maybe.map E.SetBaseUrl
                        |> E.Optional
                    , out.deviceId
                        |> Maybe.map E.SetDeviceId
                        |> E.Optional
                    ]
                , []
                )
        }
        context


loginWithUsernameAndPasswordV7 : LoginWithUsernameAndPasswordInputV3 i -> A.TaskChain (PhantomV1 a) (PhantomV1 { a | accessToken : () })
loginWithUsernameAndPasswordV7 { deviceId, enableRefreshToken, initialDeviceDisplayName, username, password } context =
    A.request
        { attributes =
            [ R.bodyOpString "address" Nothing
            , R.bodyOpString "device_id" deviceId
            , R.bodyValue "identifier"
                (E.object
                    [ ( "type", E.string "m.id.user" )
                    , ( "user", E.string username )
                    ]
                )
            , R.bodyOpString "initial_device_display_name" initialDeviceDisplayName
            , R.bodyString "password" password
            , R.bodyOpBool "refresh_token" enableRefreshToken
            , R.bodyString "type" "m.login.password"
            , R.bodyString "user" username
            , R.onStatusCode 400 "M_UNKNOWN"
            , R.onStatusCode 403 "M_FORBIDDEN"
            , R.onStatusCode 429 "M_LIMIT_EXCEEDED"
            ]
        , coder = coderV6
        , method = "POST"
        , path = [ "_matrix", "client", "v3", "login" ]
        , contextChange =
            \out -> Context.setAccessToken out.accessToken
        , toUpdate =
            \out ->
                ( E.More
                    [ E.SetAccessToken
                        { created = Context.getNow context
                        , expiryMs = out.expiresInMs
                        , lastUsed = Context.getNow context
                        , refresh = out.refreshToken
                        , value = out.accessToken
                        }
                    , E.ContentUpdate (V.SetUser out.user)
                    , out.wellKnown
                        |> Maybe.map (.homeserver >> .baseUrl)
                        |> Maybe.map E.SetBaseUrl
                        |> E.Optional
                    , E.SetDeviceId out.deviceId
                    ]
                , []
                )
        }
        context


coderV1 : Json.Coder LoginWithUsernameAndPasswordOutputV1
coderV1 =
    Json.object4
        { name = "Login Response"
        , description =
            [ "Authenticates the user by password, and issues an access token they can use to authorize themself in subsequent requests."
            , "https://spec.matrix.org/legacy/r0.0.0/client_server.html#post-matrix-client-r0-login"
            ]
        , init = LoginWithUsernameAndPasswordOutputV1
        }
        (Json.field.required
            { fieldName = "access_token"
            , toField = .accessToken
            , description =
                [ "An access token for the account. This access token can then be used to authorize other requests. The access token may expire at some point, and if so, it SHOULD come with a refresh_token. There is no specific error message to indicate that a request has failed because an access token has expired; instead, if a client has reason to believe its access token is valid, and it receives an auth error, they should attempt to refresh for a new token on failure, and retry the request with the new token."
                ]
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "home_server"
            , toField = .homeserver
            , description =
                [ "The hostname of the homeserver on which the account has been registered."
                ]
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "refresh_token"
            , toField = .refreshToken
            , description =
                [ "A refresh_token may be exchanged for a new access_token using the /tokenrefresh API endpoint."
                ]
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "user_id"
            , toField = .user
            , description =
                [ "The fully-qualified Matrix ID that has been registered."
                ]
            , coder = User.coder
            }
        )


coderV2 : Json.Coder LoginWithUsernameAndPasswordOutputV2
coderV2 =
    Json.object4
        { name = "Login Response"
        , description =
            [ "Authenticates the user by password, and issues an access token they can use to authorize themself in subsequent requests."
            , "https://spec.matrix.org/legacy/client_server/r0.3.0.html#post-matrix-client-r0-login"
            ]
        , init = LoginWithUsernameAndPasswordOutputV2
        }
        (Json.field.required
            { fieldName = "access_token"
            , toField = .accessToken
            , description =
                [ "An access token for the account. This access token can then be used to authorize other requests."
                ]
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "device_id"
            , toField = .deviceId
            , description =
                [ "ID of the logged-in device. Will be the same as the corresponding parameter in the request, if one was specified."
                ]
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "home_server"
            , toField = .homeserver
            , description =
                [ "The hostname of the homeserver on which the account has been registered."
                ]
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "user_id"
            , toField = .user
            , description =
                [ "The fully-qualified Matrix ID that has been registered."
                ]
            , coder = User.coder
            }
        )


coderV3 : Json.Coder LoginWithUsernameAndPasswordOutputV3
coderV3 =
    Json.object4
        { name = "Login Response"
        , description =
            [ "Authenticates the user by password, and issues an access token they can use to authorize themself in subsequent requests."
            , "https://spec.matrix.org/legacy/client_server/r0.3.0.html#post-matrix-client-r0-login"
            ]
        , init = LoginWithUsernameAndPasswordOutputV3
        }
        (Json.field.required
            { fieldName = "access_token"
            , toField = .accessToken
            , description =
                [ "An access token for the account. This access token can then be used to authorize other requests."
                ]
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "device_id"
            , toField = .deviceId
            , description =
                [ "ID of the logged-in device. Will be the same as the corresponding parameter in the request, if one was specified."
                ]
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "home_server"
            , toField = .homeserver
            , description =
                [ "The hostname of the homeserver on which the account has been registered."
                ]
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "user_id"
            , toField = .user
            , description =
                [ "The fully-qualified Matrix ID that has been registered."
                ]
            , coder = User.coder
            }
        )


coderV4 : Json.Coder LoginWithUsernameAndPasswordOutputV4
coderV4 =
    Json.object5
        { name = "Login Response"
        , description =
            [ "Authenticates the user by password, and issues an access token they can use to authorize themself in subsequent requests."
            , "https://spec.matrix.org/legacy/client_server/r0.5.0.html#post-matrix-client-r0-login"
            ]
        , init = LoginWithUsernameAndPasswordOutputV4
        }
        (Json.field.required
            { fieldName = "access_token"
            , toField = .accessToken
            , description =
                [ "An access token for the account. This access token can then be used to authorize other requests."
                ]
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "device_id"
            , toField = .deviceId
            , description =
                [ "ID of the logged-in device. Will be the same as the corresponding parameter in the request, if one was specified."
                ]
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "home_server"
            , toField = .homeserver
            , description =
                [ "The hostname of the homeserver on which the account has been registered."
                ]
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "user_id"
            , toField = .user
            , description =
                [ "The fully-qualified Matrix ID that has been registered."
                ]
            , coder = User.coder
            }
        )
        (Json.field.optional.value
            { fieldName = "well_known"
            , toField = .wellKnown
            , description =
                [ "Optional client configuration provided by the server. If present, clients SHOULD use the provided object to reconfigure themselves, optionally validating the URLs within. This object takes the same form as the one returned from .well-known autodiscovery."
                ]
            , coder = disoveryInformationCoderV1
            }
        )


coderV5 : Json.Coder LoginWithUsernameAndPasswordOutputV5
coderV5 =
    Json.object7
        { name = "Login Response"
        , description =
            [ "Authenticates the user by password, and issues an access token they can use to authorize themself in subsequent requests."
            , "https://spec.matrix.org/v1.3/client-server-api/#post_matrixclientv3login"
            ]
        , init = LoginWithUsernameAndPasswordOutputV5
        }
        (Json.field.required
            { fieldName = "access_token"
            , toField = .accessToken
            , description =
                [ "An access token for the account. This access token can then be used to authorize other requests."
                ]
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "device_id"
            , toField = .deviceId
            , description =
                [ "ID of the logged-in device. Will be the same as the corresponding parameter in the request, if one was specified."
                ]
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "expires_in_ms"
            , toField = .expiresInMs
            , description =
                [ "The lifetime of the access token, in milliseconds. Once the access token has expired a new access token can be obtained by using the provided refresh token. If no refresh token is provided, the client will need to re-log in to obtain a new access token. If not given, the client can assume that the access token will not expire. "
                ]
            , coder = Json.int
            }
        )
        (Json.field.optional.value
            { fieldName = "home_server"
            , toField = .homeserver
            , description =
                [ "The hostname of the homeserver on which the account has been registered."
                ]
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "refresh_token"
            , toField = .refreshToken
            , description =
                [ "A refresh token for the account. This token can be used to obtain a new access token when it expires by calling the /refresh endpoint."
                ]
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "user_id"
            , toField = .user
            , description =
                [ "The fully-qualified Matrix ID that has been registered."
                ]
            , coder = User.coder
            }
        )
        (Json.field.optional.value
            { fieldName = "well_known"
            , toField = .wellKnown
            , description =
                [ "Optional client configuration provided by the server. If present, clients SHOULD use the provided object to reconfigure themselves, optionally validating the URLs within. This object takes the same form as the one returned from .well-known autodiscovery."
                ]
            , coder = disoveryInformationCoderV1
            }
        )


coderV6 : Json.Coder LoginWithUsernameAndPasswordOutputV6
coderV6 =
    Json.object7
        { name = "Login Response"
        , description =
            [ "Authenticates the user by password, and issues an access token they can use to authorize themself in subsequent requests."
            , "https://spec.matrix.org/v1.3/client-server-api/#post_matrixclientv3login"
            ]
        , init = LoginWithUsernameAndPasswordOutputV6
        }
        (Json.field.required
            { fieldName = "access_token"
            , toField = .accessToken
            , description =
                [ "An access token for the account. This access token can then be used to authorize other requests."
                ]
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "device_id"
            , toField = .deviceId
            , description =
                [ "ID of the logged-in device. Will be the same as the corresponding parameter in the request, if one was specified."
                ]
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "expires_in_ms"
            , toField = .expiresInMs
            , description =
                [ "The lifetime of the access token, in milliseconds. Once the access token has expired a new access token can be obtained by using the provided refresh token. If no refresh token is provided, the client will need to re-log in to obtain a new access token. If not given, the client can assume that the access token will not expire. "
                ]
            , coder = Json.int
            }
        )
        (Json.field.optional.value
            { fieldName = "home_server"
            , toField = .homeserver
            , description =
                [ "The hostname of the homeserver on which the account has been registered."
                ]
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "refresh_token"
            , toField = .refreshToken
            , description =
                [ "A refresh token for the account. This token can be used to obtain a new access token when it expires by calling the /refresh endpoint."
                ]
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "user_id"
            , toField = .user
            , description =
                [ "The fully-qualified Matrix ID that has been registered."
                ]
            , coder = User.coder
            }
        )
        (Json.field.optional.value
            { fieldName = "well_known"
            , toField = .wellKnown
            , description =
                [ "Optional client configuration provided by the server. If present, clients SHOULD use the provided object to reconfigure themselves, optionally validating the URLs within. This object takes the same form as the one returned from .well-known autodiscovery."
                ]
            , coder = disoveryInformationCoderV1
            }
        )


disoveryInformationCoderV1 : Json.Coder DiscoveryInformationV1
disoveryInformationCoderV1 =
    Json.object2
        { name = "Discovery Information"
        , description =
            [ "Gets discovery information about the domain. The file may include additional keys, which MUST follow the Java package naming convention, e.g. com.example.myapp.property. This ensures property names are suitably namespaced for each application and reduces the risk of clashes."
            , "Note that this endpoint is not necessarily handled by the homeserver, but by another webserver, to be used for discovering the homeserver URL."
            , "https://spec.matrix.org/v1.10/client-server-api/#getwell-knownmatrixclient"
            ]
        , init = DiscoveryInformationV1
        }
        (Json.field.required
            { fieldName = "m.homeserver"
            , toField = .homeserver
            , coder =
                Json.object2
                    { name = "Homeserver Information"
                    , description =
                        [ "Used by clients to discover homeserver information."
                        ]
                    , init = \a _ -> { baseUrl = a }
                    }
                    (Json.field.required
                        { fieldName = "base_url"
                        , toField = .baseUrl
                        , description =
                            [ "The base URL for the homeserver for client-server connections."
                            ]
                        , coder = Json.string
                        }
                    )
                    (Json.field.optional.value
                        { fieldName = L.field
                        , toField = always Nothing
                        , description =
                            [ "The Elm SDK always expects objects to have at least two fields."
                            , "Otherwise, what's the point of hiding the value in an object?"
                            , "For this reason, this empty placeholder key will always be ignored."
                            ]
                        , coder = Json.value
                        }
                    )
            , description =
                [ "Used by clients to discover homeserver information."
                ]
            }
        )
        (Json.field.optional.value
            { fieldName = "m.identity_server"
            , toField = .identityServer
            , coder =
                Json.object2
                    { name = "Homeserver Information"
                    , description =
                        [ "Used by clients to discover homeserver information."
                        ]
                    , init = \a _ -> { baseUrl = a }
                    }
                    (Json.field.required
                        { fieldName = "base_url"
                        , toField = .baseUrl
                        , description =
                            [ "The base URL for the homeserver for client-server connections."
                            ]
                        , coder = Json.string
                        }
                    )
                    (Json.field.optional.value
                        { fieldName = L.field
                        , toField = always Nothing
                        , description =
                            [ "The Elm SDK always expects objects to have at least two fields."
                            , "Otherwise, what's the point of hiding the value in an object?"
                            , "For this reason, this empty placeholder key will always be ignored."
                            ]
                        , coder = Json.value
                        }
                    )
            , description =
                [ "Used by clients to discover identity server information."
                ]
            }
        )
