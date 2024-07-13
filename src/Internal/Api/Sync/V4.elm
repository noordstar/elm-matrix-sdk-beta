module Internal.Api.Sync.V4 exposing (..)

{-|


# Sync response

This API module represents the /sync endpoint on Matrix spec version v1.11.

<https://spec.matrix.org/v1.11/client-server-api/#syncing>

-}

import FastDict as Dict exposing (Dict)
import Internal.Api.Sync.V3 as PV
import Internal.Config.Log exposing (Log, log)
import Internal.Config.Text as Text
import Internal.Filter.Timeline exposing (Filter)
import Internal.Tools.Json as Json
import Internal.Tools.Timestamp as Timestamp exposing (Timestamp)
import Internal.Values.Envelope as E
import Internal.Values.Event as Event
import Internal.Values.Room as R
import Internal.Values.User as User exposing (User)
import Internal.Values.Vault as V
import Recursion


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
    , sender : User
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
    , unreadThreadNotifications : Maybe (Dict String ThreadNotificationCounts)
    }


type alias Ephemeral =
    { events : Maybe (List Event) }


type alias State =
    { events : Maybe (List ClientEventWithoutRoomID) }


type alias ClientEventWithoutRoomID =
    { content : Json.Value
    , eventId : String
    , originServerTs : Timestamp
    , sender : User
    , stateKey : Maybe String
    , eventType : String
    , unsigned : Maybe UnsignedData
    }


type UnsignedData
    = UnsignedData
        { age : Maybe Int
        , membership : Maybe String
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


type alias ThreadNotificationCounts =
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
    , sender : Maybe User
    , eventType : Maybe String
    }


coderSyncResponse : Json.Coder SyncResponse
coderSyncResponse =
    Json.object8
        { name = "SyncResponse"
        , description = [ "The response received when the server successfully processes the request." ]
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
            , description = [ "The batch token to supply in the since param of the next /sync request." ]
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
    Json.object1
        { name = "AccountData"
        , description = [ "The global private data created by this user." ]
        , init = AccountData
        }
        (Json.field.optional.value
            { fieldName = "events"
            , toField = .events
            , description = [ "List of events." ]
            , coder = Json.list coderEvent
            }
        )


coderEvent : Json.Coder Event
coderEvent =
    Json.object2
        { name = "Event"
        , description = [ "Details of an event." ]
        , init = Event
        }
        (Json.field.required
            { fieldName = "content"
            , toField = .content
            , description = [ "The fields in this object will vary depending on the type of event. When interacting with the REST API, this is the HTTP body." ]
            , coder = Json.value
            }
        )
        (Json.field.required
            { fieldName = "type"
            , toField = .eventType
            , description = [ "The type of event. This SHOULD be namespaced similar to Java package naming conventions e.g. ‘com.example.subdomain.event.type’" ]
            , coder = Json.string
            }
        )


coderPresence : Json.Coder Presence
coderPresence =
    Json.object1
        { name = "Presence"
        , description = [ "The updates to the presence status of other users." ]
        , init = Presence
        }
        (Json.field.optional.value
            { fieldName = "events"
            , toField = .events
            , description = [ "List of events." ]
            , coder = Json.list coderEvent
            }
        )


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
    Json.object1
        { name = "InvitedRoom"
        , description = [ "The room that the user has been invited to." ]
        , init = InvitedRoom
        }
        (Json.field.optional.value
            { fieldName = "invite_state"
            , toField = .inviteState
            , description = [ "The stripped state of a room that the user has been invited to." ]
            , coder = coderInviteState
            }
        )


coderInviteState : Json.Coder InviteState
coderInviteState =
    Json.object1
        { name = "InviteState"
        , description = [ "The stripped state of a room that the user has been invited to." ]
        , init = InviteState
        }
        (Json.field.optional.value
            { fieldName = "events"
            , toField = .events
            , description = [ "The stripped state events that form the invite state." ]
            , coder = Json.list coderStrippedStateEvent
            }
        )


coderStrippedStateEvent : Json.Coder StrippedStateEvent
coderStrippedStateEvent =
    Json.object4
        { name = "StrippedStateEvent"
        , description = [ "A stripped state event that forms part of the invite state." ]
        , init = StrippedStateEvent
        }
        (Json.field.required
            { fieldName = "content"
            , toField = .content
            , description = [ "The content for the event." ]
            , coder = Json.value
            }
        )
        (Json.field.required
            { fieldName = "sender"
            , toField = .sender
            , description = [ "The sender for the event." ]
            , coder = User.coder
            }
        )
        (Json.field.required
            { fieldName = "state_key"
            , toField = .stateKey
            , description = [ "The state_key for the event." ]
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "type"
            , toField = .eventType
            , description = [ "The type for the event." ]
            , coder = Json.string
            }
        )


coderJoinedRoom : Json.Coder JoinedRoom
coderJoinedRoom =
    Json.object7
        { name = "JoinedRoom"
        , description = [ "The room that the user has joined." ]
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
            , description = [ "The new ephemeral events in the room (events that aren’t recorded in the timeline or state of the room). In this version of the spec, these are typing notification and read receipt events." ]
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
            , description = [ "Counts of unread notifications for this room. See the Receiving notifications section for more information on how these are calculated.", "If unread_thread_notifications was specified as true on the RoomEventFilter, these counts will only be for the main timeline rather than all events in the room. See the threading module for more information.", "Changed in v1.4: Updated to reflect behaviour of having unread_thread_notifications as true in the RoomEventFilter for /sync." ]
            , coder = coderUnreadNotificationCounts
            }
        )
        (Json.field.optional.value
            { fieldName = "unread_thread_notifications"
            , toField = .unreadThreadNotifications
            , description = [ "If unread_thread_notifications was specified as true on the RoomEventFilter, the notification counts for each thread in this room. The object is keyed by thread root ID, with values matching unread_notifications.", "If a thread does not have any notifications it can be omitted from this object. If no threads have notification counts, this whole object can be omitted.", "Added in v1.4" ]
            , coder = Json.fastDict coderThreadNotificationCounts
            }
        )


coderEphemeral : Json.Coder Ephemeral
coderEphemeral =
    Json.object1
        { name = "Ephemeral"
        , description = [ "The new ephemeral events in the room." ]
        , init = Ephemeral
        }
        (Json.field.optional.value
            { fieldName = "events"
            , toField = .events
            , description = [ "List of events." ]
            , coder = Json.list coderEvent
            }
        )


coderState : Json.Coder State
coderState =
    Json.object1
        { name = "State"
        , description = [ "Updates to the state of the room." ]
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
        , description = [ "An event without the room ID." ]
        , init = ClientEventWithoutRoomID
        }
        (Json.field.required
            { fieldName = "content"
            , toField = .content
            , description = [ "The body of this event, as created by the client which sent it." ]
            , coder = Json.value
            }
        )
        (Json.field.required
            { fieldName = "event_id"
            , toField = .eventId
            , description = [ "The globally unique identifier for this event." ]
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "origin_server_ts"
            , toField = .originServerTs
            , description = [ "Timestamp (in milliseconds since the unix epoch) on originating homeserver when this event was sent." ]
            , coder = Timestamp.coder
            }
        )
        (Json.field.required
            { fieldName = "sender"
            , toField = .sender
            , description = [ "Contains the fully-qualified ID of the user who sent this event." ]
            , coder = User.coder
            }
        )
        (Json.field.optional.value
            { fieldName = "state_key"
            , toField = .stateKey
            , description = [ "Present if, and only if, this event is a state event. The key making this piece of state unique in the room. Note that it is often an empty string.", "State keys starting with an @ are reserved for referencing user IDs, such as room members. With the exception of a few events, state events set with a given user’s ID as the state key MUST only be set by that user." ]
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "type"
            , toField = .eventType
            , description = [ "The type of the event." ]
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
    Json.object5
        { name = "UnsignedData"
        , description = [ "Contains optional extra information about the event." ]
        , init =
            \a b c d e ->
                UnsignedData
                    { age = a
                    , membership = b
                    , prevContent = c
                    , redactedBecause = d
                    , transactionId = e
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
            { fieldName = "membership"
            , toField = \(UnsignedData u) -> u.membership
            , description = [ "The room membership of the user making the request, at the time of the event.", "This property is the value of the membership property of the requesting user’s m.room.member state at the point of the event, including any changes caused by the event. If the user had yet to join the room at the time of the event (i.e, they have no m.room.member state), this property is set to leave.", "Homeservers SHOULD populate this property wherever practical, but they MAY omit it if necessary (for example, if calculating the value is expensive, servers might choose to only implement it in encrypted rooms). The property is not normally populated in events pushed to application services via the application service transaction API (where there is no clear definition of “requesting user”).", "Added in v1.11" ]
            , coder = Json.string
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
    Json.object3
        { name = "RoomSummary"
        , description = [ "Information about the room which clients may need to correctly render it to users." ]
        , init = RoomSummary
        }
        (Json.field.optional.value
            { fieldName = "m.heroes"
            , toField = .mHeroes
            , description = [ "The users which can be used to generate a room name if the room does not have one. Required if the room’s m.room.name or m.room.canonical_alias state events are unset or empty.", "This should be the first 5 members of the room, ordered by stream ordering, which are joined or invited. The list must never include the client’s own user ID. When no joined or invited members are available, this should consist of the banned and left users. More than 5 members may be provided, however less than 5 should only be provided when there are less than 5 members to represent.", "When lazy-loading room members is enabled, the membership events for the heroes MUST be included in the state, unless they are redundant. When the list of users changes, the server notifies the client by sending a fresh list of heroes. If there are no changes since the last sync, this field may be omitted." ]
            , coder = Json.list Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "m.invited_member_count"
            , toField = .mInvitedMemberCount
            , description = [ "The number of users with membership of invite. If this field has not changed since the last sync, it may be omitted. Required otherwise." ]
            , coder = Json.int
            }
        )
        (Json.field.optional.value
            { fieldName = "m.joined_member_count"
            , toField = .mJoinedMemberCount
            , description = [ "The number of users with membership of join, including the client’s own user ID. If this field has not changed since the last sync, it may be omitted. Required otherwise." ]
            , coder = Json.int
            }
        )


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
            , description = [ "List of events." ]
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
    Json.object2
        { name = "UnreadNotificationCounts"
        , description = [ "Counts of unread notifications for this room." ]
        , init = UnreadNotificationCounts
        }
        (Json.field.optional.value
            { fieldName = "highlight_count"
            , toField = .highlightCount
            , description = [ "The number of unread notifications for this room with the highlight flag set." ]
            , coder = Json.int
            }
        )
        (Json.field.optional.value
            { fieldName = "notification_count"
            , toField = .notificationCount
            , description = [ "The total number of unread notifications for this room." ]
            , coder = Json.int
            }
        )


coderThreadNotificationCounts : Json.Coder ThreadNotificationCounts
coderThreadNotificationCounts =
    Json.object2
        { name = "ThreadNotificationCounts"
        , description = [ "The notification counts for each thread in this room." ]
        , init = ThreadNotificationCounts
        }
        (Json.field.optional.value
            { fieldName = "highlight_count"
            , toField = .highlightCount
            , description = [ "The number of unread notifications for this thread with the highlight flag set." ]
            , coder = Json.int
            }
        )
        (Json.field.optional.value
            { fieldName = "notification_count"
            , toField = .notificationCount
            , description = [ "The total number of unread notifications for this thread." ]
            , coder = Json.int
            }
        )


coderKnockedRoom : Json.Coder KnockedRoom
coderKnockedRoom =
    Json.object1
        { name = "KnockedRoom"
        , description = [ "The room that the user has knocked upon." ]
        , init = KnockedRoom
        }
        (Json.field.optional.value
            { fieldName = "knock_state"
            , toField = .knockState
            , description = [ "The stripped state of a room that the user has knocked upon." ]
            , coder = coderKnockState
            }
        )


coderKnockState : Json.Coder KnockState
coderKnockState =
    Json.object1
        { name = "KnockState"
        , description = [ "The stripped state of a room that the user has knocked upon." ]
        , init = KnockState
        }
        (Json.field.optional.value
            { fieldName = "events"
            , toField = .events
            , description = [ "The stripped state events that form the knock state." ]
            , coder = Json.list coderStrippedStateEvent
            }
        )


coderLeftRoom : Json.Coder LeftRoom
coderLeftRoom =
    Json.object3
        { name = "LeftRoom"
        , description = [ "The room that the user has left or been banned from." ]
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
    Json.object2
        { name = "DeviceLists"
        , description = [ "Information on end-to-end device updates, as specified in End-to-end encryption." ]
        , init = DeviceLists
        }
        (Json.field.optional.value
            { fieldName = "changed"
            , toField = .changed
            , description = [ "List of users who have updated their device identity or cross-signing keys, or who now share an encrypted room with the client since the previous sync response." ]
            , coder = Json.list Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "left"
            , toField = .left
            , description = [ "List of users with whom we do not share any encrypted rooms anymore since the previous sync response." ]
            , coder = Json.list Json.string
            }
        )


coderToDevice : Json.Coder ToDevice
coderToDevice =
    Json.object1
        { name = "ToDevice"
        , description = [ "Information on the send-to-device messages for the client device, as defined in Send-to-Device messaging." ]
        , init = ToDevice
        }
        (Json.field.optional.value
            { fieldName = "events"
            , toField = .events
            , description = [ "List of send-to-device messages." ]
            , coder = Json.list coderToDeviceEvent
            }
        )


coderToDeviceEvent : Json.Coder ToDeviceEvent
coderToDeviceEvent =
    Json.object3
        { name = "ToDeviceEvent"
        , description = [ "A send-to-device event." ]
        , init = ToDeviceEvent
        }
        (Json.field.optional.value
            { fieldName = "content"
            , toField = .content
            , description = [ "The content of this event. The fields in this object will vary depending on the type of event." ]
            , coder = Json.value
            }
        )
        (Json.field.optional.value
            { fieldName = "sender"
            , toField = .sender
            , description = [ "The Matrix user ID of the user who sent this event." ]
            , coder = User.coder
            }
        )
        (Json.field.optional.value
            { fieldName = "type"
            , toField = .eventType
            , description = [ "The type of event." ]
            , coder = Json.string
            }
        )


updateSyncResponse : { filter : Filter, since : Maybe String } -> SyncResponse -> ( E.EnvelopeUpdate V.VaultUpdate, List Log )
updateSyncResponse { filter, since } response =
    -- Account data
    [ response.accountData
        |> Maybe.andThen .events
        |> Maybe.map (List.map (\e -> V.SetAccountData e.eventType e.content))
        |> Maybe.map
            (\x ->
                ( E.ContentUpdate <| V.More x
                , if List.length x > 0 then
                    List.length x
                        |> Text.logs.syncAccountDataFound
                        |> log.debug
                        |> List.singleton

                  else
                    []
                )
            )

    -- TODO: Add device lists
    -- Next batch
    , Just ( E.SetNextBatch response.nextBatch, [] )

    -- TODO: Add presence
    -- Rooms
    , Maybe.map
        (updateRooms { filter = filter, nextBatch = response.nextBatch, since = since }
            >> Tuple.mapFirst E.ContentUpdate
        )
        response.rooms

    -- TODO: Add to_device
    ]
        |> List.filterMap identity
        |> List.unzip
        |> Tuple.mapFirst E.More
        |> Tuple.mapSecond List.concat


updateRooms : { filter : Filter, nextBatch : String, since : Maybe String } -> Rooms -> ( V.VaultUpdate, List Log )
updateRooms { filter, nextBatch, since } rooms =
    let
        ( roomUpdate, roomLogs ) =
            rooms.join
                |> Maybe.withDefault Dict.empty
                |> Dict.toList
                |> List.map
                    (\( roomId, room ) ->
                        let
                            ( u, l ) =
                                updateJoinedRoom
                                    { filter = filter
                                    , nextBatch = nextBatch
                                    , roomId = roomId
                                    , since = since
                                    }
                                    room
                        in
                        ( V.MapRoom roomId u, l )
                    )
                |> List.unzip
                |> Tuple.mapBoth V.More List.concat
    in
    ( V.More
        -- Add rooms
        [ rooms.join
            |> Maybe.withDefault Dict.empty
            |> Dict.keys
            |> List.map V.CreateRoomIfNotExists
            |> V.More

        -- Update rooms
        , roomUpdate

        -- TODO: Add invited rooms
        -- TODO: Add knocked rooms
        -- TODO: Add left rooms
        ]
    , roomLogs
    )


updateJoinedRoom : { filter : Filter, nextBatch : String, roomId : String, since : Maybe String } -> JoinedRoom -> ( R.RoomUpdate, List Log )
updateJoinedRoom data room =
    ( R.More
        [ room.accountData
            |> Maybe.andThen .events
            |> Maybe.map
                (\events ->
                    events
                        |> List.map (\e -> R.SetAccountData e.eventType e.content)
                        |> R.More
                )
            |> R.Optional
        , room.ephemeral
            |> Maybe.andThen .events
            |> Maybe.map R.SetEphemeral
            |> R.Optional

        -- TODO: Add state
        -- TODO: Add RoomSummary
        , room.timeline
            |> Maybe.map (updateTimeline data)
            |> R.Optional

        -- TODO: Add unread notifications
        -- TODO: Add unread thread notifications
        ]
    , []
    )


updateTimeline : { filter : Filter, nextBatch : String, roomId : String, since : Maybe String } -> Timeline -> R.RoomUpdate
updateTimeline { filter, nextBatch, roomId, since } timeline =
    let
        limited : Bool
        limited =
            Maybe.withDefault False timeline.limited

        newEvents : List Event.Event
        newEvents =
            List.map (toEvent roomId) timeline.events
    in
    case ( limited, timeline.prevBatch ) of
        ( False, Just p ) ->
            if timeline.prevBatch == since then
                R.AddSync
                    { events = newEvents
                    , filter = filter
                    , start = Just p
                    , end = nextBatch
                    }

            else
                R.More
                    [ R.AddSync
                        { events = []
                        , filter = filter
                        , start = since
                        , end = p
                        }
                    , R.AddSync
                        { events = newEvents
                        , filter = filter
                        , start = Just p
                        , end = nextBatch
                        }
                    ]

        ( False, Nothing ) ->
            R.AddSync
                { events = newEvents
                , filter = filter
                , start = since
                , end = nextBatch
                }

        ( True, _ ) ->
            R.AddSync
                { events = newEvents
                , filter = filter
                , start = timeline.prevBatch
                , end = nextBatch
                }


toEvent : String -> ClientEventWithoutRoomID -> Event.Event
toEvent roomId event =
    Recursion.runRecursion
        (\ev ->
            case Maybe.andThen (\(UnsignedData u) -> u.redactedBecause) ev.unsigned of
                Just e ->
                    Recursion.recurseThen e
                        (\eo ->
                            Recursion.base
                                { content = ev.content
                                , eventId = ev.eventId
                                , originServerTs = ev.originServerTs
                                , roomId = roomId
                                , sender = ev.sender
                                , stateKey = ev.stateKey
                                , eventType = ev.eventType
                                , unsigned = toUnsigned (Just eo) ev.unsigned
                                }
                        )

                Nothing ->
                    Recursion.base
                        { content = ev.content
                        , eventId = ev.eventId
                        , originServerTs = ev.originServerTs
                        , roomId = roomId
                        , sender = ev.sender
                        , stateKey = ev.stateKey
                        , eventType = ev.eventType
                        , unsigned = toUnsigned Nothing ev.unsigned
                        }
        )
        event


toUnsigned : Maybe Event.Event -> Maybe UnsignedData -> Maybe Event.UnsignedData
toUnsigned ev unsigned =
    case ( ev, unsigned ) of
        ( Nothing, Nothing ) ->
            Nothing

        ( Just e, Nothing ) ->
            { age = Nothing
            , membership = Nothing
            , prevContent = Nothing
            , redactedBecause = Just e
            , transactionId = Nothing
            }
                |> Event.UnsignedData
                |> Just

        ( _, Just (UnsignedData u) ) ->
            { age = u.age
            , membership = Nothing
            , prevContent = u.prevContent
            , redactedBecause = ev
            , transactionId = u.transactionId
            }
                |> Event.UnsignedData
                |> Just
