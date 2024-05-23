module Internal.Api.LoginWithUsernameAndPassword.Api exposing (Phantom)

{-|


# Login

This module allows the user to log in using a username and password.

@docs Phantom

-}

import Internal.Api.Api as A
import Internal.Api.Request as R
import Internal.Tools.Json as Json
import Internal.Values.Context as Context
import Internal.Values.Envelope as E
import Internal.Values.User as User exposing (User)


type alias Phantom a =
    { a | baseUrl : (), versions : () }


type alias LoginWithUsernameAndPassword =
    { deviceId : Maybe String
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


type alias LoginWithUsernameAndPasswordOutputV1 =
    { accessToken : String
    , homeserver : String
    , refreshToken : Maybe String
    , user : User
    }


type alias PhantomV1 a =
    { a | baseUrl : (), now : () }


loginWithUsernameAndPasswordV1 : LoginWithUsernameAndPasswordInputV1 a -> A.TaskChain (PhantomV1 a) (PhantomV1 { a | accessToken : () })
loginWithUsernameAndPasswordV1 { username, password } context =
    A.request
        { attributes =
            [ R.bodyString "password" password
            , R.bodyString "type" "m.login.password"
            , R.bodyString "username" username
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
        (Json.field.required
            { fieldName = "user_id"
            , toField = .user
            , description =
                [ "The fully-qualified Matrix ID that has been registered."
                ]
            , coder = User.coder
            }
        )
