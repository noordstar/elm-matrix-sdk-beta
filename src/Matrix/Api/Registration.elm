module Matrix.Api.Registration exposing (checkUsernameAvailable, checkRegistrationToken)

{-| Registration helpers.

These endpoints are useful before a user has logged in.


## Registration

@docs checkUsernameAvailable, checkRegistrationToken

-}

import Json.Decode as D
import Json.Encode as E
import Matrix.Http exposing (Error)
import Matrix.Types.HomeserverUrl as HomeserverUrl exposing (HomeserverUrl)


{-| Check whether a username is available for registration.

This endpoint returns `False` on the `M_USER_IN_USE` error.

    import Matrix.Api.Registration as Registration

    Registration.checkUsernameAvailable
        { baseUrl = homeserver
        , username = "alice"
        , toMsg = GotAvailability
        }

-}
checkUsernameAvailable :
    { baseUrl : HomeserverUrl
    , username : String
    , toMsg : Result Error Bool -> msg
    }
    -> Cmd msg
checkUsernameAvailable data =
    Matrix.Http.request
        { method = "GET"
        , accessToken = Nothing
        , baseUrl = data.baseUrl
        , path = [ "_matrix", "client", "v3", "register", "available" ]
        , query = [ ( "username", Just data.username ) ]
        , body = Nothing
        , expect =
            Matrix.Http.expectJson
                (\result ->
                    case result of
                        Err (Matrix.Http.ServerError { errorCode }) ->
                            if errorCode == "M_USER_IN_USE" then
                                data.toMsg (Ok False)

                            else
                                data.toMsg result

                        _ ->
                            data.toMsg result
                )
                (D.field "username" D.bool)
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Check whether a registration token is valid.

Some homeservers require a token for registration. This endpoint lets you
validate the token before showing a sign-up UI.

    import Matrix.Api.Registration as Registration

    Registration.checkRegistrationToken
        { baseUrl = homeserver
        , registrationToken = token
        , toMsg = GotTokenValidity
        }

-}
checkRegistrationToken :
    { baseUrl : HomeserverUrl
    , registrationToken : String
    , toMsg : Result Error Bool -> msg
    }
    -> Cmd msg
checkRegistrationToken data =
    Matrix.Http.request
        { method = "GET"
        , accessToken = Nothing
        , baseUrl = data.baseUrl
        , path = [ "_matrix", "client", "v1", "register", "m.login.registration_token", "validity" ]
        , query = []
        , body = Just <| E.object [ ( "token", E.string data.registrationToken ) ]
        , expect = Matrix.Http.expectJson data.toMsg D.bool
        , timeout = Nothing
        , tracker = Nothing
        }
