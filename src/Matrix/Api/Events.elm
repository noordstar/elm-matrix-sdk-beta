module Matrix.Api.Events exposing (fetchEvent, fetchEventWithServerVersion)

{-| Event retrieval.

Fetch single events when you have a room ID and event ID.


## Events

@docs fetchEvent, fetchEventWithServerVersion

-}

import Matrix.Api.Internal.GetEvent as GetEvent
import Matrix.Http exposing (Error)
import Matrix.Types.AccessToken exposing (AccessToken)
import Matrix.Types.ClientEvent as ClientEvent exposing (ClientEvent)
import Matrix.Types.EventId as EventId exposing (EventId)
import Matrix.Types.HomeserverUrl exposing (HomeserverUrl)
import Matrix.Types.RoomId as RoomId exposing (RoomId)
import Matrix.Types.ServerVersion as ServerVersion exposing (ServerVersion)


{-| Fetch a single event from a room.

This is useful for deep-linking or resolving an event reference without
syncing the entire room timeline.

    import Matrix.Api.Events as Events

    Events.fetchEvent
        { accessToken = accessToken
        , baseUrl = homeserver
        , eventId = eventId
        , roomId = roomId
        , toMsg = GotEvent
        }

-}
fetchEvent :
    { accessToken : AccessToken
    , baseUrl : HomeserverUrl
    , eventId : EventId
    , roomId : RoomId
    , toMsg : Result Error ClientEvent -> msg
    }
    -> Cmd msg
fetchEvent data =
    fetchEventWithServerVersion
        { accessToken = data.accessToken
        , baseUrl = data.baseUrl
        , eventId = data.eventId
        , roomId = data.roomId
        , serverVersion = ServerVersion.naive
        , toMsg = data.toMsg
        }


{-| Fetch a single event from a room using the server's advertised spec version.

Use this when you have already called `Matrix.Api.Versions.fetchServerVersion`
and want the SDK to choose the most compatible endpoint profile explicitly.

-}
fetchEventWithServerVersion :
    { accessToken : AccessToken
    , baseUrl : HomeserverUrl
    , eventId : EventId
    , roomId : RoomId
    , serverVersion : ServerVersion
    , toMsg : Result Error ClientEvent -> msg
    }
    -> Cmd msg
fetchEventWithServerVersion data =
    let
        profile =
            GetEvent.selectProfile data.serverVersion
    in
    Matrix.Http.get
        { accessToken = Just data.accessToken
        , baseUrl = data.baseUrl
        , path =
            [ "_matrix"
            , "client"
            , profile.clientPathVersion
            , "rooms"
            , RoomId.toString data.roomId
            , "event"
            , EventId.toString data.eventId
            ]
        , expect = Matrix.Http.expectJson data.toMsg (ClientEvent.decoderWithMembership profile.includeMembership)
        }
