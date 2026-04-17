module Matrix.Api.Versions exposing (fetchServerVersion)

{-| Version discovery.

Use this to see which Matrix spec versions and unstable features a server
supports.


## Versions

@docs fetchServerVersion

-}

import Matrix.Http exposing (Error)
import Matrix.Types.AccessToken as AccessToken exposing (AccessToken)
import Matrix.Types.HomeserverUrl as HomeserverUrl exposing (HomeserverUrl)
import Matrix.Types.ServerVersion as ServerVersion exposing (ServerVersion)


{-| Fetch the versions and feature flags a server supports.

    import Matrix.Api.Versions as Versions

    Versions.fetchServerVersion
        { accessToken = Nothing
        , baseUrl = homeserver
        , toMsg = GotVersions
        }

Providing an access token is optional, because servers may enable or disable
features per user. If you do not provide a token, the server only returns
features that are available to all users.

-}
fetchServerVersion :
    { accessToken : Maybe AccessToken
    , baseUrl : HomeserverUrl
    , toMsg : Result Error ServerVersion -> msg
    }
    -> Cmd msg
fetchServerVersion data =
    Matrix.Http.get
        { accessToken = data.accessToken
        , baseUrl = data.baseUrl
        , path = [ "_matrix", "client", "versions" ]
        , expect = Matrix.Http.expectJson data.toMsg ServerVersion.decoder
        }
