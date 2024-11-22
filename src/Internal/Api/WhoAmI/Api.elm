module Internal.Api.WhoAmI.Api exposing (..)

{-|


# Who Am I?

This module allows the Elm SDK user to view who the vault represents.

-}

import Internal.Api.Api as A
import Internal.Api.Request as R
import Internal.Tools.Json as Json
import Internal.Values.Envelope as E
import Internal.Values.Room as R
import Internal.Values.User as User exposing (User)


whoAmI : WhoAmIInput -> A.TaskChain (Phantom a) (Phantom a)
whoAmI =
    A.startWithVersion "r0.3.0" whoAmIV1
        |> A.forVersion "r0.4.0" whoAmIV2
        |> A.sameForVersion "r0.5.0"
        |> A.sameForVersion "r0.6.0"
        |> A.sameForVersion "r0.6.1"
        |> A.forVersion "v1.1" whoAmIV3
        |> A.forVersion "v1.2" whoAmIV4
        |> A.sameForVersion "v1.3"
        |> A.sameForVersion "v1.4"
        |> A.sameForVersion "v1.5"
        |> A.sameForVersion "v1.6"
        |> A.sameForVersion "v1.7"
        |> A.sameForVersion "v1.8"
        |> A.sameForVersion "v1.9"
        |> A.sameForVersion "v1.10"
        |> A.sameForVersion "v1.11"
        |> A.sameForVersion "v1.12"
        |> A.versionChain


{-| Context needed for setting global account data.
-}
type alias Phantom a =
    { a | accessToken : (), baseUrl : (), versions : () }


type alias PhantomV1 a =
    { a | accessToken : (), baseUrl : () }


type alias WhoAmIInput =
    {}


type alias WhoAmIInputV1 a =
    a


type alias WhoAmIOutputV1 =
    { user : User }


type alias WhoAmIOutputV2 =
    { deviceId : Maybe String, user : User }


type alias WhoAmIOutputV3 =
    { deviceId : Maybe String, isGuest : Bool, user : User }


whoAmIV1 : WhoAmIInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
whoAmIV1 _ =
    A.request
        { attributes = [ R.accessToken ]
        , coder = coderV1
        , contextChange = always identity
        , method = "GET"
        , path = [ "_matrix", "client", "r0", "account", "whoami" ]
        , toUpdate = \out -> ( E.SetUser out.user, [] )
        }


whoAmIV2 : WhoAmIInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
whoAmIV2 _ =
    A.request
        { attributes =
            [ R.accessToken
            , R.onStatusCode 401 "M_UNKNOWN_TOKEN"
            , R.onStatusCode 403 "M_FORBIDDEN"
            , R.onStatusCode 429 "M_LIMIT_EXCEEDED"
            ]
        , coder = coderV1
        , contextChange = always identity
        , method = "GET"
        , path = [ "_matrix", "client", "r0", "account", "whoami" ]
        , toUpdate = \out -> ( E.SetUser out.user, [] )
        }


whoAmIV3 : WhoAmIInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
whoAmIV3 _ =
    A.request
        { attributes =
            [ R.accessToken
            , R.onStatusCode 401 "M_UNKNOWN_TOKEN"
            , R.onStatusCode 403 "M_FORBIDDEN"
            , R.onStatusCode 429 "M_LIMIT_EXCEEDED"
            ]
        , coder = coderV2
        , contextChange = always identity
        , method = "GET"
        , path = [ "_matrix", "client", "v3", "account", "whoami" ]
        , toUpdate =
            \out ->
                ( E.More
                    [ E.SetUser out.user
                    , E.Optional (Maybe.map E.SetDeviceId out.deviceId)
                    ]
                , []
                )
        }


whoAmIV4 : WhoAmIInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
whoAmIV4 _ =
    A.request
        { attributes =
            [ R.accessToken
            , R.onStatusCode 401 "M_UNKNOWN_TOKEN"
            , R.onStatusCode 403 "M_FORBIDDEN"
            , R.onStatusCode 429 "M_LIMIT_EXCEEDED"
            ]
        , coder = coderV3
        , contextChange = always identity
        , method = "GET"
        , path = [ "_matrix", "client", "v3", "account", "whoami" ]
        , toUpdate =
            \out ->
                ( E.More
                    [ E.SetUser out.user
                    , E.Optional (Maybe.map E.SetDeviceId out.deviceId)
                    ]
                , []
                )
        }


coderV1 : Json.Coder WhoAmIOutputV1
coderV1 =
    Json.object1
        { name = "Who Am I Output"
        , description =
            [ "Reveals the identity of the account to the user"
            ]
        , init = WhoAmIOutputV1
        }
        (Json.field.required
            { fieldName = "user_id"
            , toField = .user
            , description = [ "The user that owns the access token" ]
            , coder = User.strictCoder
            }
        )


coderV2 : Json.Coder WhoAmIOutputV2
coderV2 =
    Json.object2
        { name = "Who Am I Output"
        , description =
            [ "Reveals the identity of the account to the user"
            ]
        , init = WhoAmIOutputV2
        }
        (Json.field.optional.value
            { fieldName = "device_id"
            , toField = .deviceId
            , description = [ "Device ID associated with the access token." ]
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "user_id"
            , toField = .user
            , description = [ "The user that owns the access token" ]
            , coder = User.strictCoder
            }
        )


coderV3 : Json.Coder WhoAmIOutputV3
coderV3 =
    Json.object3
        { name = "Who Am I Output"
        , description =
            [ "Reveals the identity of the account to the user"
            ]
        , init = WhoAmIOutputV3
        }
        (Json.field.optional.value
            { fieldName = "device_id"
            , toField = .deviceId
            , description = [ "Device ID associated with the access token." ]
            , coder = Json.string
            }
        )
        (Json.field.optional.withDefault
            { fieldName = "is_guest"
            , toField = .isGuest
            , description = [ "When true, the user is a guest user." ]
            , coder = Json.bool
            , default = ( False, [] )
            }
        )
        (Json.field.required
            { fieldName = "user_id"
            , toField = .user
            , description = [ "The user that owns the access token" ]
            , coder = User.strictCoder
            }
        )
