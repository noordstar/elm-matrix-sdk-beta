module Internal.Api.Sync.V3 exposing (..)

{-|


# Sync response

This API module represents the /sync endpoint on the following Matrix spec
versions:

<https://spec.matrix.org/v1.4/client-server-api/#syncing>
<https://spec.matrix.org/v1.5/client-server-api/#syncing>
<https://spec.matrix.org/v1.6/client-server-api/#syncing>
<https://spec.matrix.org/v1.7/client-server-api/#syncing>
<https://spec.matrix.org/v1.8/client-server-api/#syncing>
<https://spec.matrix.org/v1.9/client-server-api/#syncing>
<https://spec.matrix.org/v1.10/client-server-api/#syncing>

-}

import FastDict as Dict exposing (Dict)
import Internal.Api.Sync.V2 as PV
import Internal.Config.Log exposing (Log, log)
import Internal.Config.Text as Text
import Internal.Filter.Timeline exposing (Filter)
import Internal.Tools.Json as Json
import Internal.Tools.Timestamp exposing (Timestamp)
import Internal.Values.Envelope as E
import Internal.Values.Event as Event
import Internal.Values.Room as R
import Internal.Values.User exposing (User)
import Internal.Values.Vault as V


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
        , description = [ "The response for a sync request." ]
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
    PV.coderStrippedStateEvent


coderJoinedRoom : Json.Coder JoinedRoom
coderJoinedRoom =
    Json.object7
        { name = "JoinedRoom"
        , description = [ "Information about a room the user has joined." ]
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
        ( inviteUpdate, inviteLogs ) =
            rooms.invite
                |> Maybe.withDefault Dict.empty
                |> Dict.toList
                |> List.map (\( roomId, invite ) -> updateInvitedRoom roomId invite)
                |> List.unzip
                |> Tuple.mapBoth V.More List.concat

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

        -- Update invites
        , inviteUpdate

        -- Update rooms
        , roomUpdate

        -- TODO: Add knocked rooms
        -- TODO: Add left rooms
        ]
    , List.append inviteLogs roomLogs
    )


updateInvitedRoom : String -> InvitedRoom -> ( V.VaultUpdate, List Log )
updateInvitedRoom =
    PV.updateInvitedRoom


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
updateTimeline =
    PV.updateTimeline


toEvent : String -> ClientEventWithoutRoomID -> Event.Event
toEvent =
    PV.toEvent


toUnsigned : Maybe Event.Event -> Maybe UnsignedData -> Maybe Event.UnsignedData
toUnsigned =
    PV.toUnsigned
