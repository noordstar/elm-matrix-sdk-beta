module Matrix.Types.Sync exposing
    ( SyncResponse, AccountData, RawEvent, Presence, SyncEvent, Rooms
    , InvitedRoom, JoinedRoom, KnockedRoom, LeftRoom
    , InviteState, KnockState, Timeline, State, Ephemeral
    , RoomSummary, UnreadNotificationCounts, ThreadNotificationCounts
    , DeviceLists, ToDevice, ToDeviceEvent, UnsignedData, StrippedStateEvent
    , accountData, deviceLists, deviceOneTimeKeysCount, deviceUnusedFallbackKeyTypes
    , nextBatch, presence, rooms, toDevice
    , accountDataEvents, presenceEvents
    , rawEventContent, rawEventType
    , syncEventContent, syncEventId, syncEventOriginServerTs, syncEventSender
    , syncEventStateKey, syncEventType, syncEventUnsigned
    , unsignedAge, unsignedMembership, unsignedPrevContent, unsignedRedactedBecause, unsignedTransactionId
    , strippedStateContent, strippedStateSender, strippedStateKey, strippedStateType
    , roomsInvite, roomsJoin, roomsKnock, roomsLeave
    , inviteState
    , invitedRoomState, joinedRoomAccountData, joinedRoomEphemeral, joinedRoomState, joinedRoomSummary
    , joinedRoomTimeline, joinedRoomUnreadNotifications, joinedRoomUnreadThreadNotifications
    , knockedRoomState, leftRoomAccountData, leftRoomState, leftRoomTimeline
    , inviteStateEvents, knockStateEvents
    , ephemeralEvents, stateEvents, timelineEvents, timelineLimited, timelinePrevBatch
    , roomSummaryHeroes, roomSummaryInvitedMemberCount, roomSummaryJoinedMemberCount
    , unreadHighlightCount, unreadNotificationCount
    , threadHighlightCount, threadNotificationCount
    , deviceListsChanged, deviceListsLeft
    , toDeviceEvents, toDeviceEventContent, toDeviceEventSender, toDeviceEventType
    , decoder, decoderWithServerVersion
    )

{-| Raw `/sync` response types and decoders.

This module models the Matrix `/sync` response without applying it to a local
Vault. All exposed types here are opaque so the SDK can evolve the internal
representation safely in future releases.


## Sync Types

@docs SyncResponse, AccountData, RawEvent, Presence, SyncEvent, Rooms
@docs InvitedRoom, JoinedRoom, KnockedRoom, LeftRoom
@docs InviteState, KnockState, Timeline, State, Ephemeral
@docs RoomSummary, UnreadNotificationCounts, ThreadNotificationCounts
@docs DeviceLists, ToDevice, ToDeviceEvent, UnsignedData, StrippedStateEvent


## SyncResponse

@docs accountData, deviceLists, deviceOneTimeKeysCount, deviceUnusedFallbackKeyTypes
@docs nextBatch, presence, rooms, toDevice


## Common Event Data

@docs accountDataEvents, presenceEvents
@docs rawEventContent, rawEventType
@docs syncEventContent, syncEventId, syncEventOriginServerTs, syncEventSender
@docs syncEventStateKey, syncEventType, syncEventUnsigned
@docs unsignedAge, unsignedMembership, unsignedPrevContent, unsignedRedactedBecause, unsignedTransactionId
@docs strippedStateContent, strippedStateSender, strippedStateKey, strippedStateType


## Room Sections

@docs roomsInvite, roomsJoin, roomsKnock, roomsLeave
@docs inviteState
@docs invitedRoomState, joinedRoomAccountData, joinedRoomEphemeral, joinedRoomState, joinedRoomSummary
@docs joinedRoomTimeline, joinedRoomUnreadNotifications, joinedRoomUnreadThreadNotifications
@docs knockedRoomState, leftRoomAccountData, leftRoomState, leftRoomTimeline
@docs inviteStateEvents, knockStateEvents
@docs ephemeralEvents, stateEvents, timelineEvents, timelineLimited, timelinePrevBatch
@docs roomSummaryHeroes, roomSummaryInvitedMemberCount, roomSummaryJoinedMemberCount
@docs unreadHighlightCount, unreadNotificationCount
@docs threadHighlightCount, threadNotificationCount


## Device Data

@docs deviceListsChanged, deviceListsLeft
@docs toDeviceEvents, toDeviceEventContent, toDeviceEventSender, toDeviceEventType


## JSON

@docs decoder, decoderWithServerVersion

-}

import Dict exposing (Dict)
import Json.Decode as D
import Matrix.Api.Internal.Sync as InternalSync
import Matrix.Types.EventId as EventId exposing (EventId)
import Matrix.Types.RoomId as RoomId exposing (RoomId)
import Matrix.Types.ServerVersion as ServerVersion exposing (ServerVersion)
import Matrix.Types.UserId as UserId exposing (UserId)
import Time


{-| The full raw response returned by the Matrix `/sync` endpoint.
-}
type SyncResponse
    = SyncResponse
        { accountData_ : Maybe AccountData
        , deviceLists_ : Maybe DeviceLists
        , deviceOneTimeKeysCount_ : Maybe (Dict String Int)
        , deviceUnusedFallbackKeyTypes_ : List String
        , nextBatch_ : String
        , presence_ : Maybe Presence
        , rooms_ : Maybe Rooms
        , toDevice_ : Maybe ToDevice
        }


{-| Global account-data updates returned by `/sync`.
-}
type AccountData
    = AccountData
        { events_ : List RawEvent
        }


{-| A lightweight event with only `content` and `type`.
-}
type RawEvent
    = RawEvent
        { content_ : D.Value
        , eventType_ : String
        }


{-| Presence updates returned by `/sync`.
-}
type Presence
    = Presence
        { events_ : List RawEvent
        }


{-| Room sections returned by `/sync`.
-}
type Rooms
    = Rooms
        { invite_ : List ( RoomId, InvitedRoom )
        , join_ : List ( RoomId, JoinedRoom )
        , knock_ : List ( RoomId, KnockedRoom )
        , leave_ : List ( RoomId, LeftRoom )
        }


{-| An invited room section in `/sync`.
-}
type InvitedRoom
    = InvitedRoom
        { inviteState_ : Maybe InviteState
        }


{-| Stripped state for an invited room.
-}
type InviteState
    = InviteState
        { events_ : List StrippedStateEvent
        }


{-| A stripped-state event for invites and knocks.
-}
type StrippedStateEvent
    = StrippedStateEvent
        { content_ : D.Value
        , sender_ : UserId
        , stateKey_ : String
        , eventType_ : String
        }


{-| A joined room section in `/sync`.
-}
type JoinedRoom
    = JoinedRoom
        { accountData_ : Maybe AccountData
        , ephemeral_ : Maybe Ephemeral
        , state_ : Maybe State
        , summary_ : Maybe RoomSummary
        , timeline_ : Maybe Timeline
        , unreadNotifications_ : Maybe UnreadNotificationCounts
        , unreadThreadNotifications_ : Maybe (List ( EventId, ThreadNotificationCounts ))
        }


{-| Ephemeral room data such as typing and receipts.
-}
type Ephemeral
    = Ephemeral
        { events_ : List RawEvent
        }


{-| State delta data for a room.
-}
type State
    = State
        { events_ : List SyncEvent
        }


{-| A full room event as it appears in `/sync`.
-}
type SyncEvent
    = SyncEvent
        { content_ : D.Value
        , eventId_ : EventId
        , originServerTs_ : Time.Posix
        , sender_ : UserId
        , stateKey_ : Maybe String
        , eventType_ : String
        , unsigned_ : Maybe UnsignedData
        }


{-| Optional `unsigned` metadata attached to a sync event.
-}
type UnsignedData
    = UnsignedData
        { age_ : Maybe Int
        , membership_ : Maybe String
        , prevContent_ : Maybe D.Value
        , redactedBecause_ : Maybe SyncEvent
        , transactionId_ : Maybe String
        }


{-| Summary data for rendering a room.
-}
type RoomSummary
    = RoomSummary
        { heroes_ : List UserId
        , invitedMemberCount_ : Maybe Int
        , joinedMemberCount_ : Maybe Int
        }


{-| Timeline data for a room in `/sync`.
-}
type Timeline
    = Timeline
        { events_ : List SyncEvent
        , limited_ : Maybe Bool
        , prevBatch_ : Maybe String
        }


{-| Unread notification counters for a room.
-}
type UnreadNotificationCounts
    = UnreadNotificationCounts
        { highlightCount_ : Maybe Int
        , notificationCount_ : Maybe Int
        }


{-| Unread notification counters for a thread.
-}
type ThreadNotificationCounts
    = ThreadNotificationCounts
        { highlightCount_ : Maybe Int
        , notificationCount_ : Maybe Int
        }


{-| A knocked room section in `/sync`.
-}
type KnockedRoom
    = KnockedRoom
        { knockState_ : Maybe KnockState
        }


{-| Stripped state for a knocked room.
-}
type KnockState
    = KnockState
        { events_ : List StrippedStateEvent
        }


{-| A left or banned room section in `/sync`.
-}
type LeftRoom
    = LeftRoom
        { accountData_ : Maybe AccountData
        , state_ : Maybe State
        , timeline_ : Maybe Timeline
        }


{-| Device-list updates returned by `/sync`.
-}
type DeviceLists
    = DeviceLists
        { changed_ : List UserId
        , left_ : List UserId
        }


{-| To-device events returned by `/sync`.
-}
type ToDevice
    = ToDevice
        { events_ : List ToDeviceEvent
        }


{-| A single to-device event from `/sync`.
-}
type ToDeviceEvent
    = ToDeviceEvent
        { content_ : Maybe D.Value
        , sender_ : Maybe UserId
        , eventType_ : Maybe String
        }


{-| Decode a `/sync` response using the latest-known response shape.
-}
decoder : D.Decoder SyncResponse
decoder =
    decoderWithServerVersion ServerVersion.naive


{-| Decode a `/sync` response according to the server's advertised spec version.
-}
decoderWithServerVersion : ServerVersion -> D.Decoder SyncResponse
decoderWithServerVersion serverVersion =
    decoderWithSelectedProfile (InternalSync.selectDecoderProfile serverVersion)


{-| Get the global account-data section from a sync response.
-}
accountData : SyncResponse -> Maybe AccountData
accountData (SyncResponse data) =
    data.accountData_


{-| Get device-list updates from a sync response.
-}
deviceLists : SyncResponse -> Maybe DeviceLists
deviceLists (SyncResponse data) =
    data.deviceLists_


{-| Get one-time key counts from a sync response.
-}
deviceOneTimeKeysCount : SyncResponse -> Maybe (Dict String Int)
deviceOneTimeKeysCount (SyncResponse data) =
    data.deviceOneTimeKeysCount_


{-| Get the fallback-key algorithms reported by the server.
-}
deviceUnusedFallbackKeyTypes : SyncResponse -> List String
deviceUnusedFallbackKeyTypes (SyncResponse data) =
    data.deviceUnusedFallbackKeyTypes_


{-| Get the `next_batch` token for the next sync request.
-}
nextBatch : SyncResponse -> String
nextBatch (SyncResponse data) =
    data.nextBatch_


{-| Get presence updates from a sync response.
-}
presence : SyncResponse -> Maybe Presence
presence (SyncResponse data) =
    data.presence_


{-| Get the room sections from a sync response.
-}
rooms : SyncResponse -> Maybe Rooms
rooms (SyncResponse data) =
    data.rooms_


{-| Get the to-device section from a sync response.
-}
toDevice : SyncResponse -> Maybe ToDevice
toDevice (SyncResponse data) =
    data.toDevice_


{-| Get the events from an account-data section.
-}
accountDataEvents : AccountData -> List RawEvent
accountDataEvents (AccountData data) =
    data.events_


{-| Get the events from a presence section.
-}
presenceEvents : Presence -> List RawEvent
presenceEvents (Presence data) =
    data.events_


{-| Get the raw JSON `content` of a lightweight event.
-}
rawEventContent : RawEvent -> D.Value
rawEventContent (RawEvent data) =
    data.content_


{-| Get the event `type` of a lightweight event.
-}
rawEventType : RawEvent -> String
rawEventType (RawEvent data) =
    data.eventType_


{-| Get invited rooms from the rooms section.
-}
roomsInvite : Rooms -> List ( RoomId, InvitedRoom )
roomsInvite (Rooms data) =
    data.invite_


{-| Get joined rooms from the rooms section.
-}
roomsJoin : Rooms -> List ( RoomId, JoinedRoom )
roomsJoin (Rooms data) =
    data.join_


{-| Get knocked rooms from the rooms section.
-}
roomsKnock : Rooms -> List ( RoomId, KnockedRoom )
roomsKnock (Rooms data) =
    data.knock_


{-| Get left rooms from the rooms section.
-}
roomsLeave : Rooms -> List ( RoomId, LeftRoom )
roomsLeave (Rooms data) =
    data.leave_


{-| Get the invite-state payload of an invited room.
-}
inviteState : InvitedRoom -> Maybe InviteState
inviteState (InvitedRoom data) =
    data.inviteState_


{-| Alias for `inviteState`.
-}
invitedRoomState : InvitedRoom -> Maybe InviteState
invitedRoomState =
    inviteState


{-| Get room account data for a joined room.
-}
joinedRoomAccountData : JoinedRoom -> Maybe AccountData
joinedRoomAccountData (JoinedRoom data) =
    data.accountData_


{-| Get ephemeral room data for a joined room.
-}
joinedRoomEphemeral : JoinedRoom -> Maybe Ephemeral
joinedRoomEphemeral (JoinedRoom data) =
    data.ephemeral_


{-| Get the state delta for a joined room.
-}
joinedRoomState : JoinedRoom -> Maybe State
joinedRoomState (JoinedRoom data) =
    data.state_


{-| Get the summary data for a joined room.
-}
joinedRoomSummary : JoinedRoom -> Maybe RoomSummary
joinedRoomSummary (JoinedRoom data) =
    data.summary_


{-| Get the timeline for a joined room.
-}
joinedRoomTimeline : JoinedRoom -> Maybe Timeline
joinedRoomTimeline (JoinedRoom data) =
    data.timeline_


{-| Get unread room notification counts for a joined room.
-}
joinedRoomUnreadNotifications : JoinedRoom -> Maybe UnreadNotificationCounts
joinedRoomUnreadNotifications (JoinedRoom data) =
    data.unreadNotifications_


{-| Get unread thread notification counts for a joined room.
-}
joinedRoomUnreadThreadNotifications : JoinedRoom -> Maybe (List ( EventId, ThreadNotificationCounts ))
joinedRoomUnreadThreadNotifications (JoinedRoom data) =
    data.unreadThreadNotifications_


{-| Get the knock-state payload of a knocked room.
-}
knockedRoomState : KnockedRoom -> Maybe KnockState
knockedRoomState (KnockedRoom data) =
    data.knockState_


{-| Get room account data for a left room.
-}
leftRoomAccountData : LeftRoom -> Maybe AccountData
leftRoomAccountData (LeftRoom data) =
    data.accountData_


{-| Get the state payload for a left room.
-}
leftRoomState : LeftRoom -> Maybe State
leftRoomState (LeftRoom data) =
    data.state_


{-| Get the timeline for a left room.
-}
leftRoomTimeline : LeftRoom -> Maybe Timeline
leftRoomTimeline (LeftRoom data) =
    data.timeline_


{-| Get all stripped-state events from an invite state.
-}
inviteStateEvents : InviteState -> List StrippedStateEvent
inviteStateEvents (InviteState data) =
    data.events_


{-| Get all stripped-state events from a knock state.
-}
knockStateEvents : KnockState -> List StrippedStateEvent
knockStateEvents (KnockState data) =
    data.events_


{-| Get the raw JSON `content` of a stripped-state event.
-}
strippedStateContent : StrippedStateEvent -> D.Value
strippedStateContent (StrippedStateEvent data) =
    data.content_


{-| Get the sender of a stripped-state event.
-}
strippedStateSender : StrippedStateEvent -> UserId
strippedStateSender (StrippedStateEvent data) =
    data.sender_


{-| Get the `state_key` of a stripped-state event.
-}
strippedStateKey : StrippedStateEvent -> String
strippedStateKey (StrippedStateEvent data) =
    data.stateKey_


{-| Get the event `type` of a stripped-state event.
-}
strippedStateType : StrippedStateEvent -> String
strippedStateType (StrippedStateEvent data) =
    data.eventType_


{-| Get ephemeral events from an ephemeral section.
-}
ephemeralEvents : Ephemeral -> List RawEvent
ephemeralEvents (Ephemeral data) =
    data.events_


{-| Get state events from a state section.
-}
stateEvents : State -> List SyncEvent
stateEvents (State data) =
    data.events_


{-| Get timeline events from a timeline section.
-}
timelineEvents : Timeline -> List SyncEvent
timelineEvents (Timeline data) =
    data.events_


{-| Report whether a timeline was limited.
-}
timelineLimited : Timeline -> Maybe Bool
timelineLimited (Timeline data) =
    data.limited_


{-| Get the `prev_batch` token from a timeline section.
-}
timelinePrevBatch : Timeline -> Maybe String
timelinePrevBatch (Timeline data) =
    data.prevBatch_


{-| Get the raw JSON `content` of a sync event.
-}
syncEventContent : SyncEvent -> D.Value
syncEventContent (SyncEvent data) =
    data.content_


{-| Get the event ID of a sync event.
-}
syncEventId : SyncEvent -> EventId
syncEventId (SyncEvent data) =
    data.eventId_


{-| Get the origin server timestamp of a sync event.
-}
syncEventOriginServerTs : SyncEvent -> Time.Posix
syncEventOriginServerTs (SyncEvent data) =
    data.originServerTs_


{-| Get the sender of a sync event.
-}
syncEventSender : SyncEvent -> UserId
syncEventSender (SyncEvent data) =
    data.sender_


{-| Get the optional `state_key` of a sync event.
-}
syncEventStateKey : SyncEvent -> Maybe String
syncEventStateKey (SyncEvent data) =
    data.stateKey_


{-| Get the event `type` of a sync event.
-}
syncEventType : SyncEvent -> String
syncEventType (SyncEvent data) =
    data.eventType_


{-| Get the `unsigned` metadata of a sync event.
-}
syncEventUnsigned : SyncEvent -> Maybe UnsignedData
syncEventUnsigned (SyncEvent data) =
    data.unsigned_


{-| Get the `age` value from unsigned event metadata.
-}
unsignedAge : UnsignedData -> Maybe Int
unsignedAge (UnsignedData data) =
    data.age_


{-| Get the `membership` value from unsigned event metadata.
-}
unsignedMembership : UnsignedData -> Maybe String
unsignedMembership (UnsignedData data) =
    data.membership_


{-| Get the previous content from unsigned event metadata.
-}
unsignedPrevContent : UnsignedData -> Maybe D.Value
unsignedPrevContent (UnsignedData data) =
    data.prevContent_


{-| Get the redaction event from unsigned metadata, if present.
-}
unsignedRedactedBecause : UnsignedData -> Maybe SyncEvent
unsignedRedactedBecause (UnsignedData data) =
    data.redactedBecause_


{-| Get the transaction ID from unsigned event metadata.
-}
unsignedTransactionId : UnsignedData -> Maybe String
unsignedTransactionId (UnsignedData data) =
    data.transactionId_


{-| Get room heroes from a room summary.
-}
roomSummaryHeroes : RoomSummary -> List UserId
roomSummaryHeroes (RoomSummary data) =
    data.heroes_


{-| Get the invited member count from a room summary.
-}
roomSummaryInvitedMemberCount : RoomSummary -> Maybe Int
roomSummaryInvitedMemberCount (RoomSummary data) =
    data.invitedMemberCount_


{-| Get the joined member count from a room summary.
-}
roomSummaryJoinedMemberCount : RoomSummary -> Maybe Int
roomSummaryJoinedMemberCount (RoomSummary data) =
    data.joinedMemberCount_


{-| Get the highlight-count unread counter.
-}
unreadHighlightCount : UnreadNotificationCounts -> Maybe Int
unreadHighlightCount (UnreadNotificationCounts data) =
    data.highlightCount_


{-| Get the notification-count unread counter.
-}
unreadNotificationCount : UnreadNotificationCounts -> Maybe Int
unreadNotificationCount (UnreadNotificationCounts data) =
    data.notificationCount_


{-| Get the highlight-count unread counter for a thread.
-}
threadHighlightCount : ThreadNotificationCounts -> Maybe Int
threadHighlightCount (ThreadNotificationCounts data) =
    data.highlightCount_


{-| Get the notification-count unread counter for a thread.
-}
threadNotificationCount : ThreadNotificationCounts -> Maybe Int
threadNotificationCount (ThreadNotificationCounts data) =
    data.notificationCount_


{-| Get changed-user device list updates.
-}
deviceListsChanged : DeviceLists -> List UserId
deviceListsChanged (DeviceLists data) =
    data.changed_


{-| Get users who no longer share encrypted rooms with the client.
-}
deviceListsLeft : DeviceLists -> List UserId
deviceListsLeft (DeviceLists data) =
    data.left_


{-| Get to-device events from the to-device section.
-}
toDeviceEvents : ToDevice -> List ToDeviceEvent
toDeviceEvents (ToDevice data) =
    data.events_


{-| Get the raw JSON `content` of a to-device event.
-}
toDeviceEventContent : ToDeviceEvent -> Maybe D.Value
toDeviceEventContent (ToDeviceEvent data) =
    data.content_


{-| Get the sender of a to-device event.
-}
toDeviceEventSender : ToDeviceEvent -> Maybe UserId
toDeviceEventSender (ToDeviceEvent data) =
    data.sender_


{-| Get the event `type` of a to-device event.
-}
toDeviceEventType : ToDeviceEvent -> Maybe String
toDeviceEventType (ToDeviceEvent data) =
    data.eventType_


decoderWithSelectedProfile : InternalSync.DecoderProfile -> D.Decoder SyncResponse
decoderWithSelectedProfile profile =
    D.lazy
        (\() ->
            D.map8
                (\maybeAccountData maybeDeviceLists maybeKeyCounts fallbackKeyTypes nextBatchValue maybePresence maybeRooms maybeToDevice ->
                    SyncResponse
                        { accountData_ = maybeAccountData
                        , deviceLists_ = maybeDeviceLists
                        , deviceOneTimeKeysCount_ = maybeKeyCounts
                        , deviceUnusedFallbackKeyTypes_ = fallbackKeyTypes
                        , nextBatch_ = nextBatchValue
                        , presence_ = maybePresence
                        , rooms_ = maybeRooms
                        , toDevice_ = maybeToDevice
                        }
                )
                (optionalField "account_data" accountDataDecoder)
                (optionalField "device_lists" deviceListsDecoder)
                (optionalField "device_one_time_keys_count" (D.dict D.int))
                (fallbackKeyTypesDecoder profile.requiresFallbackKeyTypes)
                (D.field "next_batch" D.string)
                (optionalField "presence" presenceDecoder)
                (optionalField "rooms" (roomsDecoder profile))
                (optionalField "to_device" toDeviceDecoder)
        )


accountDataDecoder : D.Decoder AccountData
accountDataDecoder =
    D.map
        (\events ->
            AccountData { events_ = events }
        )
        (optionalField "events" (D.list rawEventDecoder)
            |> D.map (Maybe.withDefault [])
        )


rawEventDecoder : D.Decoder RawEvent
rawEventDecoder =
    D.map2
        (\contentValue eventTypeValue ->
            RawEvent { content_ = contentValue, eventType_ = eventTypeValue }
        )
        (D.field "content" D.value)
        (D.field "type" D.string)


presenceDecoder : D.Decoder Presence
presenceDecoder =
    D.map
        (\events ->
            Presence { events_ = events }
        )
        (optionalField "events" (D.list rawEventDecoder)
            |> D.map (Maybe.withDefault [])
        )


roomsDecoder : InternalSync.DecoderProfile -> D.Decoder Rooms
roomsDecoder profile =
    D.map4
        (\invitePairs joinPairs knockPairs leavePairs ->
            Rooms
                { invite_ = invitePairs
                , join_ = joinPairs
                , knock_ = knockPairs
                , leave_ = leavePairs
                }
        )
        (optionalField "invite" (roomDictDecoder invitedRoomDecoder) |> D.map (Maybe.withDefault []))
        (optionalField "join" (roomDictDecoder (joinedRoomDecoder profile)) |> D.map (Maybe.withDefault []))
        (optionalField "knock" (roomDictDecoder knockedRoomDecoder) |> D.map (Maybe.withDefault []))
        (optionalField "leave" (roomDictDecoder (leftRoomDecoder profile)) |> D.map (Maybe.withDefault []))


roomDictDecoder : D.Decoder a -> D.Decoder (List ( RoomId, a ))
roomDictDecoder valueDecoder =
    D.dict valueDecoder
        |> D.andThen
            (\raw ->
                raw
                    |> Dict.toList
                    |> traverseResult (\( roomId, value ) -> Ok ( RoomId.fromString roomId, value ))
                    |> resultDecoder identity
            )


invitedRoomDecoder : D.Decoder InvitedRoom
invitedRoomDecoder =
    D.map
        (\maybeInviteState ->
            InvitedRoom { inviteState_ = maybeInviteState }
        )
        (optionalField "invite_state" inviteStateDecoder)


inviteStateDecoder : D.Decoder InviteState
inviteStateDecoder =
    D.map
        (\events ->
            InviteState { events_ = events }
        )
        (optionalField "events" (D.list strippedStateEventDecoder)
            |> D.map (Maybe.withDefault [])
        )


strippedStateEventDecoder : D.Decoder StrippedStateEvent
strippedStateEventDecoder =
    D.map4
        (\contentValue senderValue stateKeyValue eventTypeValue ->
            StrippedStateEvent
                { content_ = contentValue
                , sender_ = senderValue
                , stateKey_ = stateKeyValue
                , eventType_ = eventTypeValue
                }
        )
        (D.field "content" D.value)
        (D.field "sender" UserId.decoder)
        (D.field "state_key" D.string)
        (D.field "type" D.string)


joinedRoomDecoder : InternalSync.DecoderProfile -> D.Decoder JoinedRoom
joinedRoomDecoder profile =
    D.map7
        (\maybeAccountData maybeEphemeral maybeState maybeSummary maybeTimeline maybeUnread maybeThreadUnread ->
            JoinedRoom
                { accountData_ = maybeAccountData
                , ephemeral_ = maybeEphemeral
                , state_ = maybeState
                , summary_ = maybeSummary
                , timeline_ = maybeTimeline
                , unreadNotifications_ = maybeUnread
                , unreadThreadNotifications_ = maybeThreadUnread
                }
        )
        (optionalField "account_data" accountDataDecoder)
        (optionalField "ephemeral" ephemeralDecoder)
        (optionalField "state" (stateDecoder profile))
        (optionalField "summary" roomSummaryDecoder)
        (optionalField "timeline" (timelineDecoder profile))
        (optionalField "unread_notifications" unreadNotificationCountsDecoder)
        (unreadThreadNotificationsDecoder profile.supportsUnreadThreadNotifications)


ephemeralDecoder : D.Decoder Ephemeral
ephemeralDecoder =
    D.map
        (\events ->
            Ephemeral { events_ = events }
        )
        (optionalField "events" (D.list rawEventDecoder)
            |> D.map (Maybe.withDefault [])
        )


stateDecoder : InternalSync.DecoderProfile -> D.Decoder State
stateDecoder profile =
    D.map
        (\events ->
            State { events_ = events }
        )
        (optionalField "events" (D.list (syncEventDecoder profile))
            |> D.map (Maybe.withDefault [])
        )


timelineDecoder : InternalSync.DecoderProfile -> D.Decoder Timeline
timelineDecoder profile =
    D.map3
        (\events maybeLimited maybePrevBatch ->
            Timeline
                { events_ = events
                , limited_ = maybeLimited
                , prevBatch_ = maybePrevBatch
                }
        )
        (optionalField "events" (D.list (syncEventDecoder profile))
            |> D.map (Maybe.withDefault [])
        )
        (optionalField "limited" D.bool)
        (optionalField "prev_batch" D.string)


syncEventDecoder : InternalSync.DecoderProfile -> D.Decoder SyncEvent
syncEventDecoder profile =
    D.lazy
        (\() ->
            D.map7
                (\contentValue eventIdValue originServerTsValue senderValue stateKeyValue eventTypeValue unsignedValue ->
                    SyncEvent
                        { content_ = contentValue
                        , eventId_ = eventIdValue
                        , originServerTs_ = originServerTsValue
                        , sender_ = senderValue
                        , stateKey_ = stateKeyValue
                        , eventType_ = eventTypeValue
                        , unsigned_ = unsignedValue
                        }
                )
                (D.field "content" D.value)
                (D.field "event_id" EventId.decoder)
                (D.field "origin_server_ts" (D.map Time.millisToPosix D.int))
                (D.field "sender" UserId.decoder)
                (optionalField "state_key" D.string)
                (D.field "type" D.string)
                (unsignedDecoder profile)
        )


unsignedDecoder : InternalSync.DecoderProfile -> D.Decoder (Maybe UnsignedData)
unsignedDecoder profile =
    D.oneOf
        [ D.map Just (D.field "unsigned" (unsignedDataDecoder profile))
        , noUnsignedSyncEventDecoder profile
        ]


noUnsignedSyncEventDecoder : InternalSync.DecoderProfile -> D.Decoder (Maybe UnsignedData)
noUnsignedSyncEventDecoder profile =
    case profile.prevContentLocation of
        InternalSync.PrevContentTopLevel ->
            optionalField "prev_content" D.value
                |> D.map
                    (\prevContentValue ->
                        if prevContentValue == Nothing then
                            Nothing

                        else
                            Just
                                (UnsignedData
                                    { age_ = Nothing
                                    , membership_ = Nothing
                                    , prevContent_ = prevContentValue
                                    , redactedBecause_ = Nothing
                                    , transactionId_ = Nothing
                                    }
                                )
                    )

        InternalSync.PrevContentUnsigned ->
            D.succeed Nothing


unsignedDataDecoder : InternalSync.DecoderProfile -> D.Decoder UnsignedData
unsignedDataDecoder profile =
    D.map5
        (\maybeAge maybeMembership maybePrevContent maybeRedactedBecause maybeTransactionId ->
            UnsignedData
                { age_ = maybeAge
                , membership_ = maybeMembership
                , prevContent_ = maybePrevContent
                , redactedBecause_ = maybeRedactedBecause
                , transactionId_ = maybeTransactionId
                }
        )
        (optionalField "age" D.int)
        (membershipDecoder profile.supportsUnsignedMembership)
        (prevContentDecoder profile.prevContentLocation)
        (optionalField "redacted_because" (syncEventDecoder profile))
        (optionalField "transaction_id" D.string)


membershipDecoder : Bool -> D.Decoder (Maybe String)
membershipDecoder supported =
    if supported then
        optionalField "membership" D.string

    else
        D.succeed Nothing


prevContentDecoder : InternalSync.PrevContentLocation -> D.Decoder (Maybe D.Value)
prevContentDecoder location =
    case location of
        InternalSync.PrevContentTopLevel ->
            D.succeed Nothing

        InternalSync.PrevContentUnsigned ->
            optionalField "prev_content" D.value


roomSummaryDecoder : D.Decoder RoomSummary
roomSummaryDecoder =
    D.map3
        (\heroesValue maybeInvited maybeJoined ->
            RoomSummary
                { heroes_ = heroesValue
                , invitedMemberCount_ = maybeInvited
                , joinedMemberCount_ = maybeJoined
                }
        )
        (optionalField "m.heroes" (D.list UserId.decoder)
            |> D.map (Maybe.withDefault [])
        )
        (optionalField "m.invited_member_count" D.int)
        (optionalField "m.joined_member_count" D.int)


unreadNotificationCountsDecoder : D.Decoder UnreadNotificationCounts
unreadNotificationCountsDecoder =
    D.map2
        (\maybeHighlight maybeNotification ->
            UnreadNotificationCounts
                { highlightCount_ = maybeHighlight
                , notificationCount_ = maybeNotification
                }
        )
        (optionalField "highlight_count" D.int)
        (optionalField "notification_count" D.int)


threadNotificationCountsDecoder : D.Decoder ThreadNotificationCounts
threadNotificationCountsDecoder =
    D.map2
        (\maybeHighlight maybeNotification ->
            ThreadNotificationCounts
                { highlightCount_ = maybeHighlight
                , notificationCount_ = maybeNotification
                }
        )
        (optionalField "highlight_count" D.int)
        (optionalField "notification_count" D.int)


knockedRoomDecoder : D.Decoder KnockedRoom
knockedRoomDecoder =
    D.map
        (\maybeKnockState ->
            KnockedRoom { knockState_ = maybeKnockState }
        )
        (optionalField "knock_state" knockStateDecoder)


knockStateDecoder : D.Decoder KnockState
knockStateDecoder =
    D.map
        (\events ->
            KnockState { events_ = events }
        )
        (optionalField "events" (D.list strippedStateEventDecoder)
            |> D.map (Maybe.withDefault [])
        )


leftRoomDecoder : InternalSync.DecoderProfile -> D.Decoder LeftRoom
leftRoomDecoder profile =
    D.map3
        (\maybeAccountData maybeState maybeTimeline ->
            LeftRoom
                { accountData_ = maybeAccountData
                , state_ = maybeState
                , timeline_ = maybeTimeline
                }
        )
        (optionalField "account_data" accountDataDecoder)
        (optionalField "state" (stateDecoder profile))
        (optionalField "timeline" (timelineDecoder profile))


deviceListsDecoder : D.Decoder DeviceLists
deviceListsDecoder =
    D.map2
        (\changedUsers leftUsers ->
            DeviceLists
                { changed_ = changedUsers
                , left_ = leftUsers
                }
        )
        (optionalField "changed" (D.list UserId.decoder) |> D.map (Maybe.withDefault []))
        (optionalField "left" (D.list UserId.decoder) |> D.map (Maybe.withDefault []))


toDeviceDecoder : D.Decoder ToDevice
toDeviceDecoder =
    D.map
        (\events ->
            ToDevice { events_ = events }
        )
        (optionalField "events" (D.list toDeviceEventDecoder)
            |> D.map (Maybe.withDefault [])
        )


toDeviceEventDecoder : D.Decoder ToDeviceEvent
toDeviceEventDecoder =
    D.map3
        (\maybeContent maybeSender maybeType ->
            ToDeviceEvent
                { content_ = maybeContent
                , sender_ = maybeSender
                , eventType_ = maybeType
                }
        )
        (optionalField "content" D.value)
        (optionalField "sender" UserId.decoder)
        (optionalField "type" D.string)


fallbackKeyTypesDecoder : Bool -> D.Decoder (List String)
fallbackKeyTypesDecoder required =
    if required then
        D.field "device_unused_fallback_key_types" (D.list D.string)

    else
        optionalField "device_unused_fallback_key_types" (D.list D.string)
            |> D.map (Maybe.withDefault [])


unreadThreadNotificationsDecoder : Bool -> D.Decoder (Maybe (List ( EventId, ThreadNotificationCounts )))
unreadThreadNotificationsDecoder supported =
    if supported then
        optionalField "unread_thread_notifications" (eventIdDictDecoder threadNotificationCountsDecoder)

    else
        D.succeed Nothing


eventIdDictDecoder : D.Decoder a -> D.Decoder (List ( EventId, a ))
eventIdDictDecoder valueDecoder =
    D.dict valueDecoder
        |> D.andThen
            (\raw ->
                raw
                    |> Dict.toList
                    |> traverseResult (\( eventId, value ) -> Ok ( EventId.fromString eventId, value ))
                    |> resultDecoder identity
            )


optionalField : String -> D.Decoder a -> D.Decoder (Maybe a)
optionalField fieldName fieldDecoder =
    D.oneOf
        [ D.field fieldName fieldDecoder |> D.map Just
        , D.succeed Nothing
        ]


traverseResult : (a -> Result x b) -> List a -> Result x (List b)
traverseResult toResult values =
    List.foldr
        (\value acc ->
            case ( toResult value, acc ) of
                ( Ok next, Ok collected ) ->
                    Ok (next :: collected)

                ( Err err, _ ) ->
                    Err err

                ( _, Err err ) ->
                    Err err
        )
        (Ok [])
        values


resultDecoder : (a -> b) -> Result String a -> D.Decoder b
resultDecoder mapValue result =
    case result of
        Ok value ->
            D.succeed (mapValue value)

        Err err ->
            D.fail err
