module Internal.Api.Sync.Api exposing (..)

{-|


# Sync

The sync module might be one of the most crucial parts of the Elm SDK. It offers
users the guarantee that the `Vault` type remains up-to-date, and it helps
communicate with the Matrix server about the Vault's needs.

@docs Phantom

-}

import Internal.Api.Api as A
import Internal.Api.Request as R
import Internal.Api.Sync.V1 as V1
import Internal.Filter.Timeline as Filter



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


type PresenceV1
    = OfflineV1


type alias SyncInput =
    { -- filter : FilterV1,
      fullState : Maybe Bool
    , presenceV1 : Maybe PresenceV1
    , since : Maybe String
    , timeout : Maybe Int
    }


type alias SyncInputV1 a =
    { a
        | -- filter : FilterV1 ,
          since : Maybe String
        , fullState : Maybe Bool
        , presenceV1 : Maybe PresenceV1
        , timeout : Maybe Int
    }


sync : SyncInput -> A.TaskChain (Phantom a) (Phantom a)
sync =
    A.startWithVersion "r0.0.0" syncV1
        |> A.versionChain


syncV1 : SyncInputV1 i -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
syncV1 data =
    A.request
        { attributes =
            [ R.accessToken
            , R.queryOpString "filter" Nothing -- FILTER HERE
            , R.queryOpString "since" data.since
            , R.queryOpBool "full_state" data.fullState
            , data.presenceV1
                |> Maybe.map (always "offline")
                |> R.queryOpString "set_presence"
            , R.queryOpInt "timeout" data.timeout
            ]
        , coder = V1.syncResponseCoder
        , contextChange = always identity
        , method = "GET"
        , path = [ "_matrix", "client", "r0", "sync" ]
        , toUpdate =
            \out ->
                ( V1.syncResponseToUpdate
                    { filter = Filter.pass -- FILTER HERE
                    , since = data.since
                    }
                    out
                , []
                )
        }
