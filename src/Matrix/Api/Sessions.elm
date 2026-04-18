module Matrix.Api.Sessions exposing (logoutDevice, logoutEverywhere)

{-| Session endpoints.

Use these to end one session or all sessions for a user.


## Sessions

@docs logoutDevice, logoutEverywhere

-}

import Json.Encode as E
import Matrix.Http exposing (Error)
import Matrix.Types.AccessToken exposing (AccessToken)
import Matrix.Types.HomeserverUrl exposing (HomeserverUrl)


{-| Log out this device session.

    import Matrix.Api.Sessions as Sessions

    Sessions.logoutDevice
        { accessToken = accessToken
        , baseUrl = homeserver
        , toMsg = GotLoggedOut
        }

-}
logoutDevice :
    { accessToken : AccessToken
    , baseUrl : HomeserverUrl
    , toMsg : Result Error () -> msg
    }
    -> Cmd msg
logoutDevice data =
    Matrix.Http.post
        { accessToken = Just data.accessToken
        , baseUrl = data.baseUrl
        , path = [ "_matrix", "client", "v3", "logout" ]
        , body = E.object []
        , expect = Matrix.Http.expectWhatever data.toMsg
        }


{-| Log out all device sessions for this user.

    import Matrix.Api.Sessions as Sessions

    Sessions.logoutEverywhere
        { accessToken = accessToken
        , baseUrl = homeserver
        , toMsg = GotLoggedOut
        }

-}
logoutEverywhere :
    { accessToken : AccessToken
    , baseUrl : HomeserverUrl
    , toMsg : Result Error () -> msg
    }
    -> Cmd msg
logoutEverywhere data =
    Matrix.Http.post
        { accessToken = Just data.accessToken
        , baseUrl = data.baseUrl
        , path = [ "_matrix", "client", "v3", "logout", "all" ]
        , body = E.object []
        , expect = Matrix.Http.expectWhatever data.toMsg
        }
