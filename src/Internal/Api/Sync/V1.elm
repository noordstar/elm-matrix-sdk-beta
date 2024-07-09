module Internal.Api.Sync.V1 exposing (..)

{-|


# Sync response

This API module represents the /sync endpoint on Matrix spec version v1.1.

<https://spec.matrix.org/v1.1/client-server-api/#syncing>

-}

import FastDict as Dict exposing (Dict)
import Internal.Config.Log exposing (Log, log)
import Internal.Config.Text as Text
import Internal.Filter.Timeline exposing (Filter)
import Internal.Tools.Json as Json
import Internal.Tools.StrippedEvent as StrippedEvent
import Internal.Tools.Timestamp as Timestamp exposing (Timestamp)
import Internal.Values.Envelope as E
import Internal.Values.Event as Event
import Internal.Values.Room as R
import Internal.Values.User as User exposing (User)
import Internal.Values.Vault as V


type alias SyncResponse =
    { accountData : Maybe AccountData
    , deviceLists : Maybe DeviceLists
    , deviceOneTimeKeysCount : Maybe (Dict String Int)
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
    { events : Maybe (List StrippedState) }


type alias StrippedState =
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
    }


type alias Ephemeral =
    { events : Maybe (List Event) }


type alias State =
    { events : Maybe (List SyncStateEvent) }


type alias SyncStateEvent =
    { content : Json.Value
    , eventId : String
    , originServerTs : Timestamp
    , prevContent : Maybe Json.Value
    , sender : User
    , stateKey : String
    , eventType : String
    , unsigned : Maybe UnsignedData
    }


type alias UnsignedData =
    { age : Maybe Int
    , redactedBecause : Maybe Event
    , transactionId : Maybe String
    }


type alias RoomSummary =
    { mHeroes : Maybe (List String)
    , mInvitedMemberCount : Maybe Int
    , mJoinedMemberCount : Maybe Int
    }


type alias Timeline =
    { events : Maybe (List SyncRoomEvent)
    , limited : Maybe Bool
    , prevBatch : Maybe String
    }


type alias SyncRoomEvent =
    { content : Json.Value
    , eventId : String
    , originServerTs : Timestamp
    , sender : User
    , eventType : String
    , unsigned : Maybe UnsignedData
    }


type alias UnreadNotificationCounts =
    { highlightCount : Maybe Int
    , notificationCount : Maybe Int
    }


type alias KnockedRoom =
    { knockState : Maybe KnockState }


type alias KnockState =
    { events : Maybe (List StrippedState) }


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
    Json.object7
        { name = "SyncResponse"
        , description = [ "The event that is returned on a 200 response." ]
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
    StrippedEvent.coder


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
        , description = [ "The rooms that the user has been invited to, mapped as room ID to room information." ]
        , init = InvitedRoom
        }
        (Json.field.optional.value
            { fieldName = "invite_state"
            , toField = .inviteState
            , description = [ "The state of a room that the user has been invited to.", "These state events may only have the sender, type, state_key and content keys present.", "These events do not replace any state that the client already has for the room, for example if the client has archived the room.", "Instead the client should keep two separate copies of the state: the one from the invite_state and one from the archived state.", "If the client joins the room then the current state will be given as a delta against the archived state not the invite_state." ]
            , coder = coderInviteState
            }
        )


coderInviteState : Json.Coder InviteState
coderInviteState =
    Json.object1
        { name = "InviteState"
        , description = [ "The state of a room that the user has been invited to." ]
        , init = InviteState
        }
        (Json.field.optional.value
            { fieldName = "events"
            , toField = .events
            , description = [ "The StrippedState events that form the invite state." ]
            , coder = Json.list coderStrippedState
            }
        )


coderStrippedState : Json.Coder StrippedState
coderStrippedState =
    Json.object4
        { name = "StrippedState"
        , description = [ "The StrippedState events that form the invite state." ]
        , init = StrippedState
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
    Json.object6
        { name = "JoinedRoom"
        , description = [ "The rooms that the user has joined, mapped as room ID to room information." ]
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
    Json.object1
        { name = "Ephemeral"
        , description = [ "The ephemeral events in the room that aren’t recorded in the timeline or state of the room. e.g. typing." ]
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
        , description = [ "Updates to the state, between the time indicated by the since parameter, and the start of the timeline (or all state up to the start of the timeline, if since is not given, or full_state is true)." ]
        , init = State
        }
        (Json.field.optional.value
            { fieldName = "events"
            , toField = .events
            , description = [ "List of events." ]
            , coder = Json.list coderSyncStateEvent
            }
        )


coderSyncStateEvent : Json.Coder SyncStateEvent
coderSyncStateEvent =
    Json.object8
        { name = "SyncStateEvent"
        , description = [ "Represents a state event within a sync response." ]
        , init = SyncStateEvent
        }
        (Json.field.required
            { fieldName = "content"
            , toField = .content
            , description = [ "The fields in this object will vary depending on the type of event. When interacting with the REST API, this is the HTTP body." ]
            , coder = Json.value
            }
        )
        (Json.field.required
            { fieldName = "event_id"
            , toField = .eventId
            , description = [ "The globally unique event identifier." ]
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "origin_server_ts"
            , toField = .originServerTs
            , description = [ "Timestamp in milliseconds on originating homeserver when this event was sent." ]
            , coder = Timestamp.coder
            }
        )
        (Json.field.optional.value
            { fieldName = "prev_content"
            , toField = .prevContent
            , description = [ "Optional. The previous content for this event. If there is no previous content, this key will be missing." ]
            , coder = Json.value
            }
        )
        (Json.field.required
            { fieldName = "sender"
            , toField = .sender
            , description = [ "Contains the fully-qualified ID of the user who sent this event." ]
            , coder = User.coder
            }
        )
        (Json.field.required
            { fieldName = "state_key"
            , toField = .stateKey
            , description = [ "A unique key which defines the overwriting semantics for this piece of room state.", "This value is often a zero-length string. The presence of this key makes this event a State Event.", "State keys starting with an @ are reserved for referencing user IDs, such as room members.", "With the exception of a few events, state events set with a given user’s ID as the state key MUST only be set by that user." ]
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "type"
            , toField = .eventType
            , description = [ "The type of event. This SHOULD be namespaced similar to Java package naming conventions e.g. ‘com.example.subdomain.event.type’" ]
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
    Json.object3
        { name = "UnsignedData"
        , description = [ "Contains optional extra information about the event." ]
        , init = UnsignedData
        }
        (Json.field.optional.value
            { fieldName = "age"
            , toField = .age
            , description = [ "The time in milliseconds that has elapsed since the event was sent.", "This field is generated by the local homeserver, and may be incorrect if the local time on at least one of the two servers is out of sync, which can cause the age to either be negative or greater than it actually is." ]
            , coder = Json.int
            }
        )
        (Json.field.optional.value
            { fieldName = "redacted_because"
            , toField = .redactedBecause
            , description = [ "The event that redacted this event, if any." ]
            , coder = coderEvent
            }
        )
        (Json.field.optional.value
            { fieldName = "transaction_id"
            , toField = .transactionId
            , description = [ "The client-supplied transaction ID, for example, provided via PUT /_matrix/client/r0/rooms/{roomId}/send/{eventType}/{txnId}, if the client being given the event is the same one which sent it." ]
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
            , description = [ "The users which can be used to generate a room name if the room does not have one. Required if the room’s m.room.name or m.room.canonical_alias state events are unset or empty.", "This should be the first 5 members of the room, ordered by stream ordering, which are joined or invited.", "The list must never include the client’s own user ID.", "When no joined or invited members are available, this should consist of the banned and left users.", "More than 5 members may be provided, however less than 5 should only be provided when there are less than 5 members to represent.", "When lazy-loading room members is enabled, the membership events for the heroes MUST be included in the state, unless they are redundant.", "When the list of users changes, the server notifies the client by sending a fresh list of heroes.", "If there are no changes since the last sync, this field may be omitted." ]
            , coder = Json.list Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "m.invited_member_count"
            , toField = .mInvitedMemberCount
            , description = [ "The number of users with membership of invite.", "If this field has not changed since the last sync, it may be omitted. Required otherwise." ]
            , coder = Json.int
            }
        )
        (Json.field.optional.value
            { fieldName = "m.joined_member_count"
            , toField = .mJoinedMemberCount
            , description = [ "The number of users with membership of join, including the client’s own user ID.", "If this field has not changed since the last sync, it may be omitted. Required otherwise." ]
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
        (Json.field.optional.value
            { fieldName = "events"
            , toField = .events
            , description = [ "List of events." ]
            , coder = Json.list coderSyncRoomEvent
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
            , description = [ "A token that can be supplied to the from parameter of the /rooms/<room_id>/messages endpoint in order to retrieve earlier events.", "If no earlier events are available, this property may be omitted from the response." ]
            , coder = Json.string
            }
        )


coderSyncRoomEvent : Json.Coder SyncRoomEvent
coderSyncRoomEvent =
    Json.object6
        { name = "SyncRoomEvent"
        , description = [ "Represents a room event within a sync response." ]
        , init = SyncRoomEvent
        }
        (Json.field.required
            { fieldName = "content"
            , toField = .content
            , description = [ "The fields in this object will vary depending on the type of event. When interacting with the REST API, this is the HTTP body." ]
            , coder = Json.value
            }
        )
        (Json.field.required
            { fieldName = "event_id"
            , toField = .eventId
            , description = [ "The globally unique event identifier." ]
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "origin_server_ts"
            , toField = .originServerTs
            , description = [ "Timestamp in milliseconds on originating homeserver when this event was sent." ]
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
        (Json.field.required
            { fieldName = "type"
            , toField = .eventType
            , description = [ "The type of event. This SHOULD be namespaced similar to Java package naming conventions e.g. ‘com.example.subdomain.event.type’" ]
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


coderKnockedRoom : Json.Coder KnockedRoom
coderKnockedRoom =
    Json.object1
        { name = "KnockedRoom"
        , description = [ "The rooms that the user has knocked upon, mapped as room ID to room information." ]
        , init = KnockedRoom
        }
        (Json.field.optional.value
            { fieldName = "knock_state"
            , toField = .knockState
            , description = [ "The state of a room that the user has knocked upon.", "The state events contained here have the same restrictions as InviteState above." ]
            , coder = coderKnockState
            }
        )


coderKnockState : Json.Coder KnockState
coderKnockState =
    Json.object1
        { name = "KnockState"
        , description = [ "The state of a room that the user has knocked upon." ]
        , init = KnockState
        }
        (Json.field.optional.value
            { fieldName = "events"
            , toField = .events
            , description = [ "The StrippedState events that form the knock state." ]
            , coder = Json.list coderStrippedState
            }
        )


coderLeftRoom : Json.Coder LeftRoom
coderLeftRoom =
    Json.object3
        { name = "LeftRoom"
        , description = [ "The rooms that the user has left or been banned from, mapped as room ID to room information." ]
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
        , description = [ "An event." ]
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
            |> Maybe.andThen
                (updateTimeline data)
            |> R.Optional

        -- TODO: Add unread notifications
        ]
    , []
    )


updateTimeline : { filter : Filter, nextBatch : String, roomId : String, since : Maybe String } -> Timeline -> Maybe R.RoomUpdate
updateTimeline { filter, nextBatch, roomId, since } timeline =
    timeline.events
        |> Maybe.map
            (\events ->
                R.AddSync
                    { events = List.map (toEvent roomId) events
                    , filter = filter
                    , start =
                        case timeline.prevBatch of
                            Just _ ->
                                timeline.prevBatch

                            Nothing ->
                                since
                    , end = nextBatch
                    }
            )


toEvent : String -> SyncRoomEvent -> Event.Event
toEvent roomId event =
    { content = event.content
    , eventId = event.eventId
    , originServerTs = event.originServerTs
    , roomId = roomId
    , sender = event.sender
    , stateKey = Nothing
    , eventType = event.eventType
    , unsigned = Maybe.map toUnsigned event.unsigned
    }


toUnsigned : UnsignedData -> Event.UnsignedData
toUnsigned u =
    Event.UnsignedData
        { age = u.age
        , prevContent = Nothing
        , redactedBecause = Nothing
        , transactionId = u.transactionId
        }
