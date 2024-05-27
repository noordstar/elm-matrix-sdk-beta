module Internal.Api.BaseUrl.Api exposing (baseUrl)

{-|


# Base URL

This module looks for the right homeserver address.

@docs baseUrl

-}

import Internal.Api.Chain as C
import Internal.Api.Request as R
import Internal.Config.Leaks as L
import Internal.Config.Log exposing (log)
import Internal.Tools.Json as Json
import Internal.Values.Context as Context
import Internal.Values.Envelope as E
import Internal.Values.Vault as V


{-| Get the homeserver base URL of a given server name.
-}
baseUrl : BaseUrlInput -> C.TaskChain R.Error (E.EnvelopeUpdate V.VaultUpdate) ph { ph | baseUrl : () }
baseUrl data =
    R.toChain
        { logHttp =
            \r ->
                ( E.HttpRequest r
                , String.concat
                    -- TODO: Move this to Internal.Config.Text module
                    [ "Matrix HTTP: "
                    , r.method
                    , " "
                    , r.url
                    ]
                    |> log.info
                    |> List.singleton
                )
        , coder = coder
        , request =
            \context ->
                { attributes = []
                , baseUrl = data.url
                , context = context
                , method = "GET"
                , path = [ ".well-known", "matrix", "client" ]
                }
        , toContextChange = \info -> Context.setBaseUrl info.homeserver.baseUrl
        , toUpdate =
            \info ->
                ( E.SetBaseUrl info.homeserver.baseUrl
                , String.concat
                    [ "Found baseURL of "
                    , data.url
                    , " at address "
                    , info.homeserver.baseUrl
                    ]
                    |> log.debug
                    |> List.singleton
                )
        }


type alias BaseUrlInput =
    { url : String }


type alias DiscoveryInformation =
    { homeserver : HomeserverInformation
    , identityServer : Maybe IdentityServerInformation
    }


type alias HomeserverInformation =
    { baseUrl : String }


type alias IdentityServerInformation =
    { baseUrl : String }


coder : Json.Coder DiscoveryInformation
coder =
    Json.object2
        { name = "Discovery Information"
        , description =
            [ "Gets discovery information about the domain. The file may include additional keys, which MUST follow the Java package naming convention, e.g. com.example.myapp.property. This ensures property names are suitably namespaced for each application and reduces the risk of clashes."
            , "Note that this endpoint is not necessarily handled by the homeserver, but by another webserver, to be used for discovering the homeserver URL."
            , "https://spec.matrix.org/v1.10/client-server-api/#getwell-knownmatrixclient"
            ]
        , init = DiscoveryInformation
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
