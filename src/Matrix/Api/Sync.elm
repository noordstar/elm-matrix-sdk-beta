module Matrix.Api.Sync exposing
    ( sync, syncWithServerVersion
    , SyncResponse, SyncPresence(..)
    , decoder
    )

{-| Raw sync endpoint access.


## API

@docs sync, syncWithServerVersion


## Types

@docs SyncResponse, SyncPresence


## JSON

@docs decoder

-}

import Json.Decode
import Matrix.Http exposing (Error)
import Matrix.Types.AccessToken exposing (AccessToken)
import Matrix.Types.HomeserverUrl exposing (HomeserverUrl)
import Matrix.Types.ServerVersion as ServerVersion exposing (ServerVersion)
import Matrix.Types.Sync as Sync exposing (SyncResponse)


{-| Raw response returned by the `/sync` endpoint.
-}
type alias SyncResponse =
    Sync.SyncResponse


{-| Presence mode to send with a `/sync` request.
-}
type SyncPresence
    = PresenceOffline
    | PresenceOnline
    | PresenceUnavailable


{-| Synchronize with the homeserver using the latest-compatible decoder.
-}
sync :
    { accessToken : AccessToken
    , baseUrl : HomeserverUrl
    , fullState : Maybe Bool
    , presence : Maybe SyncPresence
    , since : Maybe String
    , timeout : Maybe Int
    , toMsg : Result Error SyncResponse -> msg
    }
    -> Cmd msg
sync data =
    syncWithServerVersion
        { accessToken = data.accessToken
        , baseUrl = data.baseUrl
        , fullState = data.fullState
        , presence = data.presence
        , since = data.since
        , timeout = data.timeout
        , serverVersion = ServerVersion.naive
        , toMsg = data.toMsg
        }


{-| Synchronize with the homeserver using the server's advertised spec version.
-}
syncWithServerVersion :
    { accessToken : AccessToken
    , baseUrl : HomeserverUrl
    , fullState : Maybe Bool
    , presence : Maybe SyncPresence
    , since : Maybe String
    , timeout : Maybe Int
    , serverVersion : ServerVersion
    , toMsg : Result Error SyncResponse -> msg
    }
    -> Cmd msg
syncWithServerVersion data =
    Matrix.Http.request
        { method = "GET"
        , accessToken = Just data.accessToken
        , baseUrl = data.baseUrl
        , path = [ "_matrix", "client", "v3", "sync" ]
        , query =
            [ ( "full_state", Maybe.map boolString data.fullState )
            , ( "set_presence", Maybe.map presenceToString data.presence )
            , ( "since", data.since )
            , ( "timeout", Maybe.map String.fromInt data.timeout )
            ]
        , body = Nothing
        , expect = Matrix.Http.expectJson data.toMsg (Sync.decoderWithServerVersion data.serverVersion)
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Decode a `/sync` response using the latest-known response shape.
-}
decoder : Json.Decode.Decoder SyncResponse
decoder =
    Sync.decoder


presenceToString : SyncPresence -> String
presenceToString presence =
    case presence of
        PresenceOffline ->
            "offline"

        PresenceOnline ->
            "online"

        PresenceUnavailable ->
            "unavailable"


boolString : Bool -> String
boolString value =
    if value then
        "true"

    else
        "false"
