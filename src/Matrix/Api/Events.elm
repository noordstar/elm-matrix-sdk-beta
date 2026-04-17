module Matrix.Api.Events exposing (fetchEvent)

{-| Event retrieval.

Fetch single events when you have a room ID and event ID.


## Events

@docs fetchEvent

-}

import Matrix.Http exposing (Error)
import Matrix.Types.AccessToken as AccessToken exposing (AccessToken)
import Matrix.Types.ClientEvent as ClientEvent exposing (ClientEvent)
import Matrix.Types.EventId as EventId exposing (EventId)
import Matrix.Types.HomeserverUrl as HomeserverUrl exposing (HomeserverUrl)
import Matrix.Types.RoomId as RoomId exposing (RoomId)


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
    Matrix.Http.get
        { accessToken = Just data.accessToken
        , baseUrl = data.baseUrl
        , path = [ "_matrix", "client", "v3", "rooms", RoomId.toString data.roomId, "event", EventId.toString data.eventId ]
        , expect = Matrix.Http.expectJson data.toMsg ClientEvent.decoder
        }
