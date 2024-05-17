module Internal.Api.Api exposing (..)

{-|


# API

The API module is a front-end for implementing API endpoints according to spec.

-}

import Internal.Api.Chain as C
import Internal.Api.Request as R
import Internal.Config.Log exposing (log)
import Internal.Tools.Json as Json
import Internal.Values.Context exposing (APIContext)
import Internal.Values.Vault as V


{-| A TaskChain helps create a chain of HTTP requests.
-}
type alias TaskChain ph1 ph2 =
    C.TaskChain R.Error V.VaultUpdate { ph1 | baseUrl : () } { ph2 | baseUrl : () }


request :
    { attributes : List (R.Attribute { ph1 | baseUrl : () })
    , coder : Json.Coder V.VaultUpdate
    , contextChange : V.VaultUpdate -> (APIContext { ph1 | baseUrl : () } -> APIContext { ph2 | baseUrl : () })
    , method : String
    , path : List String
    }
    -> TaskChain ph1 ph2
request data =
    R.toChain
        { logHttp =
            \r ->
                ( V.HttpRequest r
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
        , coder = data.coder
        , request =
            R.callAPI
                { method = data.method
                , path = data.path
                }
                |> R.withAttributes data.attributes
        , toContextChange = data.contextChange
        }
