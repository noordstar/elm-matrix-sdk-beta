module Internal.Values.Context exposing
    ( Context, init, coder, encode, decoder
    , APIContext, apiFormat
    , setAccessToken, getAccessToken
    , setBaseUrl, getBaseUrl
    , setTransaction, getTransaction
    , setVersions, getVersions
    )

{-| The Context is the set of variables that the user (mostly) cannot control.
The Context contains tokens, values and other bits that the Vault receives from
the Matrix API.


## Context

@docs Context, init, coder, encode, decoder


## APIContext

Once the API starts needing information, that's when we use the APIContext type
to build the right environment for the API communication to work with.

@docs APIContext, apiFormat

Once the APIContext is ready, there's helper functions for each piece of
information that can be inserted.


### Access token

@docs setAccessToken, getAccessToken


### Base URL

@docs setBaseUrl, getBaseUrl


### Transaction id

@docs setTransaction, getTransaction


### Versions

@docs setVersions, getVersions

-}

import Internal.Config.Leaks as L
import Internal.Config.Text as Text
import Internal.Tools.Json as Json


{-| The Context type stores all the information in the Vault. This data type is
static and hence can be passed on easily.
-}
type alias Context =
    { accessToken : Maybe String
    , baseUrl : Maybe String
    , password : Maybe String
    , refreshToken : Maybe String
    , username : Maybe String
    , transaction : Maybe String
    , versions : Maybe (List String)
    }


{-| The APIContext is a separate type that uses a phantom type to trick the
compiler into requiring values to be present. This data type is used to gather
the right variables (like an access token) before accessing the Matrix API.
-}
type APIContext ph
    = APIContext
        { accessToken : String
        , baseUrl : String
        , context : Context
        , transaction : String
        , versions : List String
        }


{-| Create an unformatted APIContext type.
-}
apiFormat : Context -> APIContext {}
apiFormat context =
    APIContext
        { accessToken = context.accessToken |> Maybe.withDefault L.accessToken
        , baseUrl = context.baseUrl |> Maybe.withDefault L.baseUrl
        , context = context
        , transaction = context.transaction |> Maybe.withDefault L.transaction
        , versions = context.versions |> Maybe.withDefault L.versions
        }


coder : Json.Coder Context
coder =
    Json.object7
        { name = Text.docs.context.name
        , description = Text.docs.context.description
        , init = Context
        }
        (Json.field.optional.value
            { fieldName = "accessToken"
            , toField = .accessToken
            , description = Text.fields.context.accessToken
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "baseUrl"
            , toField = .baseUrl
            , description = Text.fields.context.baseUrl
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "password"
            , toField = .password
            , description = Text.fields.context.password
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "refreshToken"
            , toField = .refreshToken
            , description = Text.fields.context.refreshToken
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "username"
            , toField = .username
            , description = Text.fields.context.username
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "transaction"
            , toField = .transaction
            , description = Text.fields.context.transaction
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "versions"
            , toField = .versions
            , description = Text.fields.context.versions
            , coder = Json.list Json.string
            }
        )


{-| Decode a Context type from a JSON value.
-}
decoder : Json.Decoder Context
decoder =
    Json.decode coder


{-| Encode a Context type into a JSON value.
-}
encode : Json.Encoder Context
encode =
    Json.encode coder


{-| A basic, untouched version of the Context, containing no information.
-}
init : Context
init =
    { accessToken = Nothing
    , baseUrl = Nothing
    , refreshToken = Nothing
    , password = Nothing
    , username = Nothing
    , transaction = Nothing
    , versions = Nothing
    }


{-| Get an inserted access token.
-}
getAccessToken : APIContext { a | accessToken : () } -> String
getAccessToken (APIContext c) =
    c.accessToken


{-| Insert an access token into the APIContext.
-}
setAccessToken : String -> APIContext a -> APIContext { a | accessToken : () }
setAccessToken value (APIContext c) =
    APIContext { c | accessToken = value }


{-| Get an inserted base URL.
-}
getBaseUrl : APIContext { a | baseUrl : () } -> String
getBaseUrl (APIContext c) =
    c.baseUrl


{-| Insert a base URL into the APIContext.
-}
setBaseUrl : String -> APIContext a -> APIContext { a | baseUrl : () }
setBaseUrl value (APIContext c) =
    APIContext { c | baseUrl = value }


{-| Get an inserted transaction id.
-}
getTransaction : APIContext { a | transaction : () } -> String
getTransaction (APIContext c) =
    c.transaction


{-| Insert a transaction id into the APIContext.
-}
setTransaction : String -> APIContext a -> APIContext { a | transaction : () }
setTransaction value (APIContext c) =
    APIContext { c | transaction = value }


{-| Get an inserted versions list.
-}
getVersions : APIContext { a | versions : () } -> List String
getVersions (APIContext c) =
    c.versions


{-| Insert a versions list into the APIContext.
-}
setVersions : List String -> APIContext a -> APIContext { a | versions : () }
setVersions value (APIContext c) =
    APIContext { c | versions = value }
