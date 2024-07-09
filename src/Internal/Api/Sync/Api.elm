module Internal.Api.Sync.Api exposing (sync, Phantom)

{-|


# Sync

The sync module might be one of the most crucial parts of the Elm SDK. It offers
users the guarantee that the `Vault` type remains up-to-date, and it helps
communicate with the Matrix server about the Vault's needs.

@docs sync, Phantom

-}

import Internal.Api.Api as A
import Internal.Api.Request as R
import Internal.Api.Sync.V1 as V1
import Internal.Api.Sync.V2 as V2
import Internal.Api.Sync.V3 as V3
import Internal.Api.Sync.V4 as V4
import Internal.Filter.Timeline as Filter


{-| Sync with the Matrix API.
-}
sync : SyncInput -> A.TaskChain (Phantom a) (Phantom a)
sync =
    A.startWithVersion "v1.1" syncV1
        |> A.forVersion "v1.2" syncV2
        |> A.sameForVersion "v1.3"
        |> A.forVersion "v1.4" syncV3
        |> A.sameForVersion "v1.5"
        |> A.sameForVersion "v1.6"
        |> A.sameForVersion "v1.7"
        |> A.sameForVersion "v1.8"
        |> A.sameForVersion "v1.9"
        |> A.sameForVersion "v1.10"
        |> A.forVersion "v1.11" syncV4
        |> A.versionChain



-- For simplicity, we will not use a filter for now
-- and assume that every client always wants to receive all events.
-- type FilterV1
--     = FilterV1 Filter
--     | FilterIdV1 String Filter
--     | NoFilter


type alias Phantom a =
    { a | accessToken : (), baseUrl : (), versions : () }


type alias PhantomV1 a =
    { a | accessToken : (), baseUrl : () }


type alias SyncInput =
    { -- filter : FilterV1,
      fullState : Maybe Bool
    , presence : Maybe String
    , since : Maybe String
    , timeout : Maybe Int
    }


type alias SyncInputV1 a =
    { a
        | -- filter : FilterV1 ,
          since : Maybe String
        , fullState : Maybe Bool
        , presence : Maybe String
        , timeout : Maybe Int
    }


presenceFromOptions : List String -> Maybe String -> Maybe String
presenceFromOptions options =
    Maybe.andThen
        (\v ->
            if List.member v options then
                Just v

            else
                Nothing
        )


syncV1 : SyncInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
syncV1 data =
    A.request
        { attributes =
            [ R.accessToken
            , R.queryOpString "filter" Nothing -- FILTER HERE
            , R.queryOpBool "full_state" data.fullState
            , data.presence
                |> presenceFromOptions [ "offline", "online", "unavailable" ]
                |> R.queryOpString "set_presence"
            , R.queryOpString "since" data.since
            , R.queryOpInt "timeout" data.timeout
            ]
        , coder = V1.coderSyncResponse
        , contextChange = always identity
        , method = "GET"
        , path = [ "_matrix", "client", "r0", "sync" ]
        , toUpdate =
            V1.updateSyncResponse { filter = Filter.pass, since = data.since }
        }


syncV2 : SyncInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
syncV2 data =
    A.request
        { attributes =
            [ R.accessToken
            , R.queryOpString "filter" Nothing
            , R.queryOpBool "full_state" data.fullState
            , data.presence
                |> presenceFromOptions [ "offline", "online", "unavailable" ]
                |> R.queryOpString "set_presence"
            , R.queryOpString "since" data.since
            , R.queryOpInt "timeout" data.timeout
            ]
        , coder = V2.coderSyncResponse
        , contextChange = always identity
        , method = "GET"
        , path = [ "_matrix", "client", "r0", "sync" ]
        , toUpdate =
            V2.updateSyncResponse { filter = Filter.pass, since = data.since }
        }


syncV3 : SyncInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
syncV3 data =
    A.request
        { attributes =
            [ R.accessToken
            , R.queryOpString "filter" Nothing
            , R.queryOpBool "full_state" data.fullState
            , data.presence
                |> presenceFromOptions [ "offline", "online", "unavailable" ]
                |> R.queryOpString "set_presence"
            , R.queryOpString "since" data.since
            , R.queryOpInt "timeout" data.timeout
            ]
        , coder = V3.coderSyncResponse
        , contextChange = always identity
        , method = "GET"
        , path = [ "_matrix", "client", "r0", "sync" ]
        , toUpdate =
            V3.updateSyncResponse { filter = Filter.pass, since = data.since }
        }


syncV4 : SyncInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
syncV4 data =
    A.request
        { attributes =
            [ R.accessToken
            , R.queryOpString "filter" Nothing
            , R.queryOpBool "full_state" data.fullState
            , data.presence
                |> presenceFromOptions [ "offline", "online", "unavailable" ]
                |> R.queryOpString "set_presence"
            , R.queryOpString "since" data.since
            , R.queryOpInt "timeout" data.timeout
            ]
        , coder = V4.coderSyncResponse
        , contextChange = always identity
        , method = "GET"
        , path = [ "_matrix", "client", "r0", "sync" ]
        , toUpdate =
            V4.updateSyncResponse { filter = Filter.pass, since = data.since }
        }
