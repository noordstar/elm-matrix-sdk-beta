module Internal.Values.Context exposing
    ( Context, AccessToken, init, coder, encode, decoder
    , mostPopularToken
    , APIContext, apiFormat, fromApiFormat
    , setAccessToken, getAccessToken
    , setBaseUrl, getBaseUrl
    , setNow, getNow
    , setTransaction, getTransaction
    , Versions, setVersions, getVersions
    , reset
    )

{-| The Context is the set of variables that the user (mostly) cannot control.
The Context contains tokens, values and other bits that the Vault receives from
the Matrix API.


## Context

@docs Context, AccessToken, init, coder, encode, decoder

Some functions are present to influence the general Context type itself.

@docs mostPopularToken


## APIContext

Once the API starts needing information, that's when we use the APIContext type
to build the right environment for the API communication to work with.

@docs APIContext, apiFormat, fromApiFormat

Once the APIContext is ready, there's helper functions for each piece of
information that can be inserted.


### Access token

@docs setAccessToken, getAccessToken


### Base URL

@docs setBaseUrl, getBaseUrl


### Timestamp

@docs setNow, getNow


### Transaction id

@docs setTransaction, getTransaction


### Versions

@docs Versions, setVersions, getVersions


### Reset

@docs reset

-}

import Internal.Config.Leaks as L
import Internal.Config.Text as Text
import Internal.Tools.Hashdict as Hashdict exposing (Hashdict)
import Internal.Tools.Json as Json
import Internal.Tools.Timestamp as Timestamp exposing (Timestamp)
import Json.Encode as E
import Set exposing (Set)
import Time


{-| The Access Token is a combination of access tokens, values and refresh
tokens that contain and summarizes all properties of a known access token.
-}
type alias AccessToken =
    { created : Timestamp
    , expiryMs : Maybe Int
    , lastUsed : Timestamp
    , refresh : Maybe String
    , value : String
    }


{-| The Context type stores all the information in the Vault. This data type is
static and hence can be passed on easily.
-}
type alias Context =
    { accessTokens : Hashdict AccessToken
    , baseUrl : Maybe String
    , deviceId : Maybe String
    , nextBatch : Maybe String
    , now : Maybe Timestamp
    , password : Maybe String
    , refreshToken : Maybe String
    , serverName : String
    , suggestedAccessToken : Maybe String
    , transaction : Maybe String
    , username : Maybe String
    , versions : Maybe Versions
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
        , now : Timestamp
        , transaction : String
        , versions : Versions
        }


type alias Versions =
    { versions : List String, unstableFeatures : Set String }


{-| Create an unformatted APIContext type.
-}
apiFormat : Context -> APIContext {}
apiFormat context =
    APIContext
        { accessToken =
            mostPopularToken context |> Maybe.withDefault L.accessToken
        , baseUrl = context.baseUrl |> Maybe.withDefault L.baseUrl
        , context = context
        , now = context.now |> Maybe.withDefault (Time.millisToPosix 0)
        , transaction = context.transaction |> Maybe.withDefault L.transaction
        , versions = context.versions |> Maybe.withDefault L.versions
        }


{-| Get the original context that contains all values from before any were
gotten from the Matrix API.
-}
fromApiFormat : APIContext a -> Context
fromApiFormat (APIContext c) =
    c.context


{-| Define how a Context can be encoded to and decoded from a JSON object.
-}
coder : Json.Coder Context
coder =
    Json.object12
        { name = Text.docs.context.name
        , description = Text.docs.context.description
        , init = Context
        }
        (Json.field.required
            { fieldName = "accessTokens"
            , toField = .accessTokens
            , description = Text.fields.context.accessToken
            , coder = Hashdict.coder .value coderAccessToken
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
            { fieldName = "deviceId"
            , toField = .deviceId
            , description = Text.fields.context.deviceId
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "nextBatch"
            , toField = .nextBatch
            , description = Text.fields.context.nextBatch
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "now"
            , toField = .now
            , description = Text.fields.context.now
            , coder = Timestamp.coder
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
        (Json.field.required
            { fieldName = "serverName"
            , toField = .serverName
            , description = Text.fields.context.serverName
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "suggestedAccessToken"
            , toField = always Nothing -- Do not save
            , description = Text.fields.context.suggestedAccessToken
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
            { fieldName = "username"
            , toField = .username
            , description = Text.fields.context.username
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "versions"
            , toField = .versions
            , description = Text.fields.context.versions
            , coder = versionsCoder
            }
        )


{-| JSON coder for an Access Token.
-}
coderAccessToken : Json.Coder AccessToken
coderAccessToken =
    Json.object5
        { name = Text.docs.accessToken.name
        , description = Text.docs.accessToken.description
        , init = AccessToken
        }
        (Json.field.required
            { fieldName = "created"
            , toField = .created
            , description = Text.fields.accessToken.created
            , coder = Timestamp.coder
            }
        )
        (Json.field.optional.value
            { fieldName = "expiryMs"
            , toField = .expiryMs
            , description = Text.fields.accessToken.expiryMs
            , coder = Json.int
            }
        )
        (Json.field.required
            { fieldName = "lastUsed"
            , toField = .lastUsed
            , description = Text.fields.accessToken.lastUsed
            , coder = Timestamp.coder
            }
        )
        (Json.field.optional.value
            { fieldName = "refresh"
            , toField = .refresh
            , description = Text.fields.accessToken.refresh
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "value"
            , toField = .value
            , description = Text.fields.accessToken.value
            , coder = Json.string
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
init : String -> Context
init sn =
    { accessTokens = Hashdict.empty .value
    , baseUrl = Nothing
    , deviceId = Nothing
    , nextBatch = Nothing
    , now = Nothing
    , refreshToken = Nothing
    , password = Nothing
    , serverName = sn
    , suggestedAccessToken = Nothing
    , transaction = Nothing
    , username = Nothing
    , versions = Nothing
    }


{-| Get the most popular access token available, if any.
-}
mostPopularToken : Context -> Maybe String
mostPopularToken c =
    case c.suggestedAccessToken of
        Just _ ->
            c.suggestedAccessToken

        Nothing ->
            c.accessTokens
                |> Hashdict.values
                |> List.sortBy
                    (\token ->
                        case token.expiryMs of
                            Nothing ->
                                ( 0, Timestamp.toMs token.created )

                            Just e ->
                                ( 1
                                , token.created
                                    |> Timestamp.add e
                                    |> Timestamp.toMs
                                )
                    )
                |> List.head
                |> Maybe.map .value


{-| Reset the phantom type of the Context, effectively forgetting all values.
-}
reset : APIContext a -> APIContext {}
reset (APIContext c) =
    APIContext c


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


{-| Get an inserted timestamp.
-}
getNow : APIContext { a | now : () } -> Timestamp
getNow (APIContext c) =
    c.now


{-| Insert a Timestamp into the APIContext.
-}
setNow : Timestamp -> APIContext a -> APIContext { a | now : () }
setNow t (APIContext c) =
    APIContext { c | now = t }


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
getVersions : APIContext { a | versions : () } -> Versions
getVersions (APIContext c) =
    c.versions


{-| Insert a versions list into the APIContext.
-}
setVersions : Versions -> APIContext a -> APIContext { a | versions : () }
setVersions value (APIContext c) =
    APIContext { c | versions = value }


versionsCoder : Json.Coder Versions
versionsCoder =
    Json.object2
        { name = Text.docs.versions.name
        , description = Text.docs.versions.description
        , init = Versions
        }
        (Json.field.required
            { fieldName = "versions"
            , toField = .versions
            , description = Text.fields.versions.versions
            , coder = Json.list Json.string
            }
        )
        (Json.field.optional.withDefault
            { fieldName = "unstableFeatures"
            , toField = .unstableFeatures
            , description = Text.fields.versions.unstableFeatures
            , coder = Json.set Json.string
            , default = ( Set.empty, [] )
            }
        )
