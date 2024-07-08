module Internal.Api.Sync.V2 exposing (..)

{-|


# Sync response

This API module represents the /sync endpoint on Matrix spec version v1.1.

<https://spec.matrix.org/v1.2/client-server-api/#syncing>
<https://spec.matrix.org/v1.3/client-server-api/#syncing>

-}

import FastDict exposing (Dict)
import Internal.Api.Sync.V1 as PV
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


type UnsignedData
    = UnsignedData
        { age : Maybe Int
        , prevContent : Maybe Json.Value
        , redactedBecause : Maybe ClientEventWithoutRoomID
        , transactionId : Maybe String
        }


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
    Json.object8
        { name = "SyncResponse"
        , description = [ "An event that is part of a response." ]
        , init = SyncResponse
        }
        (Json.field.optional.value
            { fieldName = "account_data"
            , toField = .accountData
            , description = [ "The global private data created by this user." ]
            , coder = coderAccountData
            }
        )
        (Json.field.optional.value
            { fieldName = "device_lists"
            , toField = .deviceLists
            , description = [ "Information on end-to-end device updates, as specified in End-to-end encryption." ]
            , coder = coderDeviceLists
            }
        )
        (Json.field.optional.value
            { fieldName = "device_one_time_keys_count"
            , toField = .deviceOneTimeKeysCount
            , description = [ "Information on end-to-end encryption keys, as specified in End-to-end encryption." ]
            , coder = Json.fastDict Json.int
            }
        )
        (Json.field.required
            { fieldName = "device_unused_fallback_key_types"
            , toField = .deviceUnusedFallbackKeyTypes
            , description = [ "The unused fallback key algorithms." ]
            , coder = Json.list Json.string
            }
        )
        (Json.field.required
            { fieldName = "next_batch"
            , toField = .nextBatch
            , description = [ "Required: The batch token to supply in the since param of the next /sync request." ]
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "presence"
            , toField = .presence
            , description = [ "The updates to the presence status of other users." ]
            , coder = coderPresence
            }
        )
        (Json.field.optional.value
            { fieldName = "rooms"
            , toField = .rooms
            , description = [ "Updates to rooms." ]
            , coder = coderRooms
            }
        )
        (Json.field.optional.value
            { fieldName = "to_device"
            , toField = .toDevice
            , description = [ "Information on the send-to-device messages for the client device, as defined in Send-to-Device messaging." ]
            , coder = coderToDevice
            }
        )


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
    Json.object4
        { name = "Rooms"
        , description = [ "Updates to rooms." ]
        , init = Rooms
        }
        (Json.field.optional.value
            { fieldName = "invite"
            , toField = .invite
            , description = [ "The rooms that the user has been invited to, mapped as room ID to room information." ]
            , coder = Json.fastDict coderInvitedRoom
            }
        )
        (Json.field.optional.value
            { fieldName = "join"
            , toField = .join
            , description = [ "The rooms that the user has joined, mapped as room ID to room information." ]
            , coder = Json.fastDict coderJoinedRoom
            }
        )
        (Json.field.optional.value
            { fieldName = "knock"
            , toField = .knock
            , description = [ "The rooms that the user has knocked upon, mapped as room ID to room information." ]
            , coder = Json.fastDict coderKnockedRoom
            }
        )
        (Json.field.optional.value
            { fieldName = "leave"
            , toField = .leave
            , description = [ "The rooms that the user has left or been banned from, mapped as room ID to room information." ]
            , coder = Json.fastDict coderLeftRoom
            }
        )


coderInvitedRoom : Json.Coder InvitedRoom
coderInvitedRoom =
    PV.coderInvitedRoom


coderInviteState : Json.Coder InviteState
coderInviteState =
    PV.coderInviteState


coderStrippedStateEvent : Json.Coder StrippedStateEvent
coderStrippedStateEvent =
    PV.coderStrippedState


coderJoinedRoom : Json.Coder JoinedRoom
coderJoinedRoom =
    Json.object6
        { name = "JoinedRoom"
        , description = [ "The rooms that the user has joined." ]
        , init = JoinedRoom
        }
        (Json.field.optional.value
            { fieldName = "account_data"
            , toField = .accountData
            , description = [ "The private data that this user has attached to this room." ]
            , coder = coderAccountData
            }
        )
        (Json.field.optional.value
            { fieldName = "ephemeral"
            , toField = .ephemeral
            , description = [ "The ephemeral events in the room that aren’t recorded in the timeline or state of the room. e.g. typing." ]
            , coder = coderEphemeral
            }
        )
        (Json.field.optional.value
            { fieldName = "state"
            , toField = .state
            , description = [ "Updates to the state, between the time indicated by the since parameter, and the start of the timeline (or all state up to the start of the timeline, if since is not given, or full_state is true).", "N.B. state updates for m.room.member events will be incomplete if lazy_load_members is enabled in the /sync filter, and only return the member events required to display the senders of the timeline events in this response." ]
            , coder = coderState
            }
        )
        (Json.field.optional.value
            { fieldName = "summary"
            , toField = .summary
            , description = [ "Information about the room which clients may need to correctly render it to users." ]
            , coder = coderRoomSummary
            }
        )
        (Json.field.optional.value
            { fieldName = "timeline"
            , toField = .timeline
            , description = [ "The timeline of messages and state changes in the room." ]
            , coder = coderTimeline
            }
        )
        (Json.field.optional.value
            { fieldName = "unread_notifications"
            , toField = .unreadNotifications
            , description = [ "Counts of unread notifications for this room. See the Receiving notifications section for more information on how these are calculated." ]
            , coder = coderUnreadNotificationCounts
            }
        )


coderEphemeral : Json.Coder Ephemeral
coderEphemeral =
    PV.coderEphemeral


coderState : Json.Coder State
coderState =
    Json.object1
        { name = "State"
        , description = [ "Updates to the state." ]
        , init = State
        }
        (Json.field.optional.value
            { fieldName = "events"
            , toField = .events
            , description = [ "List of events." ]
            , coder = Json.list coderClientEventWithoutRoomID
            }
        )


coderClientEventWithoutRoomID : Json.Coder ClientEventWithoutRoomID
coderClientEventWithoutRoomID =
    Json.object7
        { name = "ClientEventWithoutRoomID"
        , description = [ "An event without a room ID." ]
        , init = ClientEventWithoutRoomID
        }
        (Json.field.required
            { fieldName = "content"
            , toField = .content
            , description = [ "Required: The body of this event, as created by the client which sent it." ]
            , coder = Json.value
            }
        )
        (Json.field.required
            { fieldName = "event_id"
            , toField = .eventId
            , description = [ "Required: The globally unique identifier for this event." ]
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "origin_server_ts"
            , toField = .originServerTs
            , description = [ "Required: Timestamp (in milliseconds since the unix epoch) on originating homeserver when this event was sent." ]
            , coder = Json.int
            }
        )
        (Json.field.required
            { fieldName = "sender"
            , toField = .sender
            , description = [ "Required: Contains the fully-qualified ID of the user who sent this event." ]
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "state_key"
            , toField = .stateKey
            , description = [ "Present if, and only if, this event is a state event. The key making this piece of state unique in the room. Note that it is often an empty string." ]
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "type"
            , toField = .eventType
            , description = [ "Required: The type of the event." ]
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "unsigned"
            , toField = .unsigned
            , description = [ "Contains optional extra information about the event." ]
            , coder = coderUnsignedData
            }
        )


coderUnsignedData : Json.Coder UnsignedData
coderUnsignedData =
    Json.object4
        { name = "UnsignedData"
        , description = [ "Contains optional extra information about the event." ]
        , init =
            \a b c d ->
                UnsignedData
                    { age = a
                    , prevContent = b
                    , redactedBecause = c
                    , transactionId = d
                    }
        }
        (Json.field.optional.value
            { fieldName = "age"
            , toField = \(UnsignedData u) -> u.age
            , description = [ "The time in milliseconds that has elapsed since the event was sent. This field is generated by the local homeserver, and may be incorrect if the local time on at least one of the two servers is out of sync, which can cause the age to either be negative or greater than it actually is." ]
            , coder = Json.int
            }
        )
        (Json.field.optional.value
            { fieldName = "prev_content"
            , toField = \(UnsignedData u) -> u.prevContent
            , description = [ "The previous content for this event. This field is generated by the local homeserver, and is only returned if the event is a state event, and the client has permission to see the previous content.", "Changed in v1.2: Previously, this field was specified at the top level of returned events rather than in unsigned (with the exception of the GET .../notifications endpoint), though in practice no known server implementations honoured this." ]
            , coder = Json.value
            }
        )
        (Json.field.optional.value
            { fieldName = "redacted_because"
            , toField = \(UnsignedData u) -> u.redactedBecause
            , description = [ "The event that redacted this event, if any." ]
            , coder = Json.lazy (\_ -> coderClientEventWithoutRoomID)
            }
        )
        (Json.field.optional.value
            { fieldName = "transaction_id"
            , toField = \(UnsignedData u) -> u.transactionId
            , description = [ "The client-supplied transaction ID, for example, provided via PUT /_matrix/client/v3/rooms/{roomId}/send/{eventType}/{txnId}, if the client being given the event is the same one which sent it." ]
            , coder = Json.string
            }
        )


coderRoomSummary : Json.Coder RoomSummary
coderRoomSummary =
    PV.coderRoomSummary


coderTimeline : Json.Coder Timeline
coderTimeline =
    Json.object3
        { name = "Timeline"
        , description = [ "The timeline of messages and state changes in the room." ]
        , init = Timeline
        }
        (Json.field.required
            { fieldName = "events"
            , toField = .events
            , description = [ "Required: List of events." ]
            , coder = Json.list coderClientEventWithoutRoomID
            }
        )
        (Json.field.optional.value
            { fieldName = "limited"
            , toField = .limited
            , description = [ "True if the number of events returned was limited by the limit on the filter." ]
            , coder = Json.bool
            }
        )
        (Json.field.optional.value
            { fieldName = "prev_batch"
            , toField = .prevBatch
            , description = [ "A token that can be supplied to the from parameter of the /rooms/<room_id>/messages endpoint in order to retrieve earlier events. If no earlier events are available, this property may be omitted from the response." ]
            , coder = Json.string
            }
        )


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
    Json.object3
        { name = "LeftRoom"
        , description = [ "The rooms that the user has left or been banned from." ]
        , init = LeftRoom
        }
        (Json.field.optional.value
            { fieldName = "account_data"
            , toField = .accountData
            , description = [ "The private data that this user has attached to this room." ]
            , coder = coderAccountData
            }
        )
        (Json.field.optional.value
            { fieldName = "state"
            , toField = .state
            , description = [ "The state updates for the room up to the start of the timeline." ]
            , coder = coderState
            }
        )
        (Json.field.optional.value
            { fieldName = "timeline"
            , toField = .timeline
            , description = [ "The timeline of messages and state changes in the room up to the point when the user left." ]
            , coder = coderTimeline
            }
        )


coderDeviceLists : Json.Coder DeviceLists
coderDeviceLists =
    PV.coderDeviceLists


coderToDevice : Json.Coder ToDevice
coderToDevice =
    PV.coderToDevice


coderToDeviceEvent : Json.Coder ToDeviceEvent
coderToDeviceEvent =
    PV.coderToDeviceEvent
