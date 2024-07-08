module Internal.Api.Sync.V3 exposing (..)

{-|


# Sync response

This API module represents the /sync endpoint on Matrix spec version v1.1.

<https://spec.matrix.org/v1.3/client-server-api/#syncing>

-}

import FastDict exposing (Dict)
import Internal.Api.Sync.V2 as PV
import Internal.Tools.Json as Json


type alias SyncResponse =
    { accountData : Maybe AccountData
    , deviceLists : Maybe DeviceLists
    , deviceOneTimeKeysCount : Maybe (Dict String Int)
    , deviceUnusedFallbackKeyTypes : List String
    , nextBatch : String
    , presence : Maybe Presence
    , rooms : Maybe Rooms
    , toDevice : Maybe ToDevice
    }


type alias AccountData =
    { events : Maybe (List Event) }


type alias Event =
    { content : Json.Value
    , eventType : String
    }


type alias Presence =
    { events : Maybe (List Event) }


type alias Rooms =
    { invite : Maybe (Dict String InvitedRoom)
    , join : Maybe (Dict String JoinedRoom)
    , knock : Maybe (Dict String KnockedRoom)
    , leave : Maybe (Dict String LeftRoom)
    }


type alias InvitedRoom =
    { inviteState : Maybe InviteState }


type alias InviteState =
    { events : Maybe (List StrippedStateEvent) }


type alias StrippedStateEvent =
    { content : Json.Value
    , sender : String
    , stateKey : String
    , eventType : String
    }


type alias JoinedRoom =
    { accountData : Maybe AccountData
    , ephemeral : Maybe Ephemeral
    , state : Maybe State
    , summary : Maybe RoomSummary
    , timeline : Maybe Timeline
    , unreadNotifications : Maybe UnreadNotificationCounts
    }


type alias Ephemeral =
    { events : Maybe (List Event) }


type alias State =
    { events : Maybe (List ClientEventWithoutRoomID) }


type alias ClientEventWithoutRoomID =
    { content : Json.Value
    , eventId : String
    , originServerTs : Int
    , sender : String
    , stateKey : Maybe String
    , eventType : String
    , unsigned : Maybe UnsignedData
    }


type alias UnsignedData =
    PV.UnsignedData


type alias RoomSummary =
    { mHeroes : Maybe (List String)
    , mInvitedMemberCount : Maybe Int
    , mJoinedMemberCount : Maybe Int
    }


type alias Timeline =
    { events : List ClientEventWithoutRoomID
    , limited : Maybe Bool
    , prevBatch : Maybe String
    }


type alias UnreadNotificationCounts =
    { highlightCount : Maybe Int
    , notificationCount : Maybe Int
    }


type alias KnockedRoom =
    { knockState : Maybe KnockState }


type alias KnockState =
    { events : Maybe (List StrippedStateEvent) }


type alias LeftRoom =
    { accountData : Maybe AccountData
    , state : Maybe State
    , timeline : Maybe Timeline
    }


type alias DeviceLists =
    { changed : Maybe (List String)
    , left : Maybe (List String)
    }


type alias ToDevice =
    { events : Maybe (List ToDeviceEvent) }


type alias ToDeviceEvent =
    { content : Maybe Json.Value
    , sender : Maybe String
    , eventType : Maybe String
    }


coderSyncResponse : Json.Coder SyncResponse
coderSyncResponse =
    PV.coderSyncResponse


coderAccountData : Json.Coder AccountData
coderAccountData =
    PV.coderAccountData


coderEvent : Json.Coder Event
coderEvent =
    PV.coderEvent


coderPresence : Json.Coder Presence
coderPresence =
    PV.coderPresence


coderRooms : Json.Coder Rooms
coderRooms =
    PV.coderRooms


coderInvitedRoom : Json.Coder InvitedRoom
coderInvitedRoom =
    PV.coderInvitedRoom


coderInviteState : Json.Coder InviteState
coderInviteState =
    PV.coderInviteState


coderStrippedStateEvent : Json.Coder StrippedStateEvent
coderStrippedStateEvent =
    PV.coderStrippedStateEvent


coderJoinedRoom : Json.Coder JoinedRoom
coderJoinedRoom =
    PV.coderJoinedRoom


coderEphemeral : Json.Coder Ephemeral
coderEphemeral =
    PV.coderEphemeral


coderState : Json.Coder State
coderState =
    PV.coderState


coderClientEventWithoutRoomID : Json.Coder ClientEventWithoutRoomID
coderClientEventWithoutRoomID =
    PV.coderClientEventWithoutRoomID


coderUnsignedData : Json.Coder UnsignedData
coderUnsignedData =
    PV.coderUnsignedData


coderRoomSummary : Json.Coder RoomSummary
coderRoomSummary =
    PV.coderRoomSummary


coderTimeline : Json.Coder Timeline
coderTimeline =
    PV.coderTimeline


coderUnreadNotificationCounts : Json.Coder UnreadNotificationCounts
coderUnreadNotificationCounts =
    PV.coderUnreadNotificationCounts


coderKnockedRoom : Json.Coder KnockedRoom
coderKnockedRoom =
    PV.coderKnockedRoom


coderKnockState : Json.Coder KnockState
coderKnockState =
    PV.coderKnockState


coderLeftRoom : Json.Coder LeftRoom
coderLeftRoom =
    PV.coderLeftRoom


coderDeviceLists : Json.Coder DeviceLists
coderDeviceLists =
    PV.coderDeviceLists


coderToDevice : Json.Coder ToDevice
coderToDevice =
    PV.coderToDevice


coderToDeviceEvent : Json.Coder ToDeviceEvent
coderToDeviceEvent =
    PV.coderToDeviceEvent
