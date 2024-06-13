module Internal.Api.Sync.V1 exposing (..)

{-|


# Sync V1

Given the complexity of the /sync endpoint, the JSON coders have been placed
in separate modules. Version 1 provides a valid JSON coder for the following
spec versions:

  - r0.3.0 : <https://spec.matrix.org/legacy/client_server/r0.3.0.html#get-matrix-client-r0-sync>

-- @docs coder

-}

import FastDict as Dict exposing (Dict)
import Internal.Config.Log exposing (Log)
import Internal.Filter.Timeline exposing (Filter)
import Internal.Tools.Json as Json
import Internal.Tools.StrippedEvent as StrippedEvent
import Internal.Tools.Timestamp as Timestamp exposing (Timestamp)
import Internal.Values.Envelope as E exposing (EnvelopeUpdate(..))
import Internal.Values.Event as Event
import Internal.Values.Room as R exposing (RoomUpdate(..))
import Internal.Values.User as User exposing (User)
import Internal.Values.Vault as V exposing (VaultUpdate(..))


type alias SyncResponse =
    { accountData : Maybe AccountData
    , deviceLists : Maybe DeviceLists
    , nextBatch : Maybe String
    , presence : Maybe Presence
    , rooms : Maybe Rooms
    , toDevice : Maybe ToDevice
    }


syncResponseCoder : Json.Coder SyncResponse
syncResponseCoder =
    Json.object6
        { name = "SyncResponse"
        , description =
            [ "Response from the /sync endpoint"
            ]
        , init = SyncResponse
        }
        (Json.field.optional.value
            { fieldName = "account_data"
            , toField = .accountData
            , description =
                [ "The global private data created by this user."
                ]
            , coder = accountDataCoder
            }
        )
        (Json.field.optional.value
            { fieldName = "device_lists"
            , toField = .deviceLists
            , description =
                [ "Information on end-to-end device updates, as specified in End-to-end encryption."
                ]
            , coder = deviceListsCoder
            }
        )
        (Json.field.optional.value
            { fieldName = "next_batch"
            , toField = .nextBatch
            , description =
                [ "The batch token to supply in the since param of the next /sync request."
                ]
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "presence"
            , toField = .presence
            , description =
                [ "The updates to the presence status of other users."
                ]
            , coder = presenceCoder
            }
        )
        (Json.field.optional.value
            { fieldName = "rooms"
            , toField = .rooms
            , description =
                [ "Updates to rooms."
                ]
            , coder = roomsCoder
            }
        )
        (Json.field.optional.value
            { fieldName = "to_device"
            , toField = .toDevice
            , description =
                [ "Information on the send-to-device messages for the client device, as defined in Send-to-Device messaging."
                ]
            , coder = toDeviceCoder
            }
        )


syncResponseToUpdate : { filter : Filter, since : Maybe String } -> SyncResponse -> EnvelopeUpdate VaultUpdate
syncResponseToUpdate data response =
    E.More
        [ E.ContentUpdate
            (V.More
                -- global account data
                [ response.accountData
                    |> Maybe.map
                        (\acd ->
                            acd.events
                                |> List.map (\e -> V.SetAccountData e.eventType e.content)
                                |> V.More
                        )
                    |> V.Optional

                -- rooms to update
                , Maybe.map2
                    (\rooms nextBatch ->
                        roomsToUpdate
                            { filter = data.filter
                            , nextBatch = nextBatch
                            , since = data.since
                            }
                            rooms
                    )
                    response.rooms
                    response.nextBatch
                    |> V.Optional

                -- next batch
                , response.nextBatch
                    |> Maybe.map V.SetNextBatch
                    |> V.Optional
                ]
            )

        -- Add more updates here
        ]


type alias ToDevice =
    { events : List DeviceEvent }


toDeviceCoder : Json.Coder ToDevice
toDeviceCoder =
    Json.object1
        { name = "ToDevice"
        , description =
            [ "Events indicating users' to_device changes, usually involving cryptography"
            ]
        , init = ToDevice
        }
        (Json.field.required
            { fieldName = "events"
            , toField = .events
            , description =
                [ "List of events"
                ]
            , coder = Json.list deviceEventCoder
            }
        )


type alias DeviceEvent =
    { content : EventContent, sender : User, eventType : String }


deviceEventCoder : Json.Coder DeviceEvent
deviceEventCoder =
    Json.object3
        { name = "DeviceEvent"
        , description =
            [ "Partially stripped event used for sending to_device events."
            ]
        , init = DeviceEvent
        }
        (Json.field.required
            { fieldName = "content"
            , toField = .content
            , description =
                [ "The content of this event. The fields in this object will vary depending on the type of event."
                ]
            , coder = eventContentCoder
            }
        )
        (Json.field.required
            { fieldName = "sender"
            , toField = .sender
            , description =
                [ "The Matrix user ID of the user who sent this event."
                ]
            , coder = User.coder
            }
        )
        (Json.field.required
            { fieldName = "type"
            , toField = .eventType
            , description =
                [ "The type of event."
                ]
            , coder = Json.string
            }
        )


type alias DeviceLists =
    { changed : List String }


deviceListsCoder : Json.Coder DeviceLists
deviceListsCoder =
    Json.object1
        { name = "DeviceLists"
        , description =
            [ "This module adds an optional device_lists property to the /sync response, as specified below. The server need only populate this property for an incremental /sync (ie, one where the since parameter was specified). The client is expected to use /keys/query or /keys/changes for the equivalent functionality after an initial sync, as documented in \"Tracking the device list for a user.\""
            ]
        , init = DeviceLists
        }
        (Json.field.required
            { fieldName = "changed"
            , toField = .changed
            , description =
                [ "List of users who have updated their device identity keys since the previous sync response."
                ]
            , coder = Json.list Json.string
            }
        )


type alias Rooms =
    { invite : Dict String InvitedRoom
    , join : Dict String JoinedRoom
    , leave : Dict String LeftRoom
    }


roomsCoder : Json.Coder Rooms
roomsCoder =
    Json.object3
        { name = "Rooms"
        , description =
            [ "Summary of all relevant updates to all rooms."
            ]
        , init = Rooms
        }
        (Json.field.optional.withDefault
            { fieldName = "invite"
            , toField = .invite
            , description =
                [ "The rooms that the user has been invited to."
                ]
            , coder = Json.fastDict invitedRoomCoder
            , default = ( Dict.empty, [] )
            , defaultToString = always "{}"
            }
        )
        (Json.field.optional.withDefault
            { fieldName = "join"
            , toField = .join
            , description =
                [ "The rooms that the user is a member of."
                ]
            , coder = Json.fastDict joinedRoomCoder
            , default = ( Dict.empty, [] )
            , defaultToString = always "{}"
            }
        )
        (Json.field.optional.withDefault
            { fieldName = "leave"
            , toField = .leave
            , description =
                [ "The rooms that the user has left."
                ]
            , coder = Json.fastDict leftRoomCoder
            , default = ( Dict.empty, [] )
            , defaultToString = always "{}"
            }
        )


roomsToUpdate : { filter : Filter, nextBatch : String, since : Maybe String } -> Rooms -> VaultUpdate
roomsToUpdate data rooms =
    rooms.join
        |> Dict.foldl
            (\roomId joinedRoom items ->
                List.append items
                    [ V.CreateRoomIfNotExists roomId
                    , V.MapRoom roomId
                        (joinedRoomToUpdate
                            { filter = data.filter
                            , nextBatch = data.nextBatch
                            , roomId = roomId
                            , since = data.since
                            }
                            joinedRoom
                        )
                    ]
            )
            []
        |> V.More


type alias InvitedRoom =
    { inviteState : Maybe InviteState }


invitedRoomCoder : Json.Coder InvitedRoom
invitedRoomCoder =
    Json.object1
        { name = "InvitedRoom"
        , description =
            [ "Room that the user was invited to."
            ]
        , init = InvitedRoom
        }
        (Json.field.optional.value
            { fieldName = "invite_state"
            , toField = .inviteState
            , description =
                [ "The state of a room that the user has been invited to. These state events may only have the sender, type, state_key and content keys present. These events do not replace any state that the client already has for the room, for example if the client has archived the room. Instead the client should keep two separate copies of the state: the one from the invite_state and one from the archived state. If the client joins the room then the current state will be given as a delta against the archived state not the invite_state."
                ]
            , coder = inviteStateCoder
            }
        )


type alias InviteState =
    { events : List Event }


inviteStateCoder : Json.Coder InviteState
inviteStateCoder =
    Json.object1
        { name = "InviteState"
        , description =
            [ "Invite (state) events describing the state of an invited room."
            ]
        , init = Ephemeral
        }
        (Json.field.required
            { fieldName = "events"
            , toField = .events
            , description =
                [ "List of events"
                ]
            , coder = Json.list eventCoder
            }
        )


type alias JoinedRoom =
    { accountData : Maybe AccountData
    , ephemeral : Maybe Ephemeral
    , state : Maybe State
    , timeline : Maybe Timeline
    }


joinedRoomCoder : Json.Coder JoinedRoom
joinedRoomCoder =
    Json.object4
        { name = "JoinedRoom"
        , description =
            [ "Matrix room that the user is a member of."
            ]
        , init = JoinedRoom
        }
        (Json.field.optional.value
            { fieldName = "account_data"
            , toField = .accountData
            , description =
                [ "The private data that this user has attached to this room."
                ]
            , coder = accountDataCoder
            }
        )
        (Json.field.optional.value
            { fieldName = "ephemeral"
            , toField = .ephemeral
            , description =
                [ "The ephemeral events in the room that aren't recorded in the timeline or state of the room. e.g. typing."
                ]
            , coder = ephemeralCoder
            }
        )
        (Json.field.optional.value
            { fieldName = "state"
            , toField = .state
            , description =
                [ "Updates to the state, between the time indicated by the since parameter, and the start of the timeline (or all state up to the start of the timeline, if since is not given, or full_state is true)."
                ]
            , coder = stateCoder
            }
        )
        (Json.field.optional.value
            { fieldName = "timeline"
            , toField = .timeline
            , description =
                [ "The timeline of messages and state changes in the room."
                ]
            , coder = timelineCoder
            }
        )


joinedRoomToUpdate : { filter : Filter, nextBatch : String, roomId : String, since : Maybe String } -> JoinedRoom -> RoomUpdate
joinedRoomToUpdate data room =
    R.More
        [ room.accountData
            |> Maybe.map
                (\acd ->
                    acd.events
                        |> List.map (\event -> R.SetAccountData event.eventType event.content)
                        |> R.More
                )
            |> R.Optional
        , room.ephemeral
            |> Maybe.map ephemeralToUpdate
            |> R.Optional
        , room.timeline
            |> Maybe.map (timelineToUpdate data)
            |> R.Optional
        ]


type alias Ephemeral =
    { events : List Event }


ephemeralCoder : Json.Coder Ephemeral
ephemeralCoder =
    Json.object1
        { name = "Ephemeral"
        , description =
            [ "Ephemeral events sent to the user from a room."
            ]
        , init = Ephemeral
        }
        (Json.field.required
            { fieldName = "events"
            , toField = .events
            , description =
                [ "List of events"
                ]
            , coder = Json.list eventCoder
            }
        )


ephemeralToUpdate : Ephemeral -> RoomUpdate
ephemeralToUpdate { events } =
    events
        |> List.map StrippedEvent.strip
        |> R.SetEphemeral


type alias LeftRoom =
    { state : Maybe State, timeline : Maybe Timeline }


leftRoomCoder : Json.Coder LeftRoom
leftRoomCoder =
    Json.object2
        { name = "LeftRoom"
        , description =
            [ "Room that the user is no longer a member of."
            ]
        , init = LeftRoom
        }
        (Json.field.optional.value
            { fieldName = "state"
            , toField = .state
            , description =
                [ "The state updates for the room up to the start of the timeline."
                ]
            , coder = stateCoder
            }
        )
        (Json.field.optional.value
            { fieldName = "timeline"
            , toField = .timeline
            , description =
                [ "The timeline of messages and state changes in the room up to the point when the user left."
                ]
            , coder = timelineCoder
            }
        )


type alias State =
    { events : List Event }


stateCoder : Json.Coder State
stateCoder =
    Json.object1
        { name = "State"
        , description =
            [ "List of (state) events describing the room's state."
            ]
        , init = State
        }
        (Json.field.required
            { fieldName = "events"
            , toField = .events
            , description = [ "List of events" ]
            , coder = Json.list eventCoder
            }
        )


type alias Timeline =
    { events : List Event
    , limited : Bool
    , prevBatch : Maybe String
    }


timelineCoder : Json.Coder Timeline
timelineCoder =
    Json.object3
        { name = "Timeline"
        , description =
            [ "Timeline type describing a timeline in a room."
            ]
        , init = Timeline
        }
        (Json.field.required
            { fieldName = "events"
            , toField = .events
            , description =
                [ "List of events"
                ]
            , coder = Json.list eventCoder
            }
        )
        (Json.field.optional.withDefault
            { fieldName = "limited"
            , toField = .limited
            , description =
                [ "True if the number of events returned was limited by the limit on the filter"
                ]
            , coder = Json.bool
            , default = ( False, [] )
            , defaultToString =
                \b ->
                    if b then
                        "true"

                    else
                        "false"
            }
        )
        (Json.field.optional.value
            { fieldName = "prev_batch"
            , toField = .prevBatch
            , description =
                [ "A token that can be supplied to to the from parameter of the rooms/{roomId}/messages endpoint"
                , "If the batch was limited then this is a token that can be supplied to the server to retrieve earlier events"
                ]
            , coder = Json.string
            }
        )


timelineToUpdate : { filter : Filter, nextBatch : String, roomId : String, since : Maybe String } -> Timeline -> RoomUpdate
timelineToUpdate { filter, nextBatch, roomId, since } t =
    R.AddSync
        { events = List.map (eventToUpdate roomId) t.events
        , filter = filter
        , start =
            case t.prevBatch of
                Just _ ->
                    t.prevBatch

                Nothing ->
                    since
        , end = nextBatch
        }


type alias Presence =
    { events : List Event }


presenceCoder : Json.Coder Presence
presenceCoder =
    Json.object1
        { name = "Presence"
        , description =
            [ "Events indicating users' presence"
            ]
        , init = Ephemeral
        }
        (Json.field.required
            { fieldName = "events"
            , toField = .events
            , description =
                [ "List of events"
                ]
            , coder = Json.list eventCoder
            }
        )


type alias AccountData =
    { events : List Event }


accountDataCoder : Json.Coder AccountData
accountDataCoder =
    Json.object1
        { name = "AccountData"
        , description =
            [ "Account data events sent by the user in a given room using a different client."
            ]
        , init = Ephemeral
        }
        (Json.field.required
            { fieldName = "events"
            , toField = .events
            , description =
                [ "List of events"
                ]
            , coder = Json.list eventCoder
            }
        )


type alias Event =
    { content : EventContent
    , eventId : String
    , originServerTs : Timestamp
    , sender : User
    , stateKey : Maybe String
    , eventType : String
    , unsigned : Maybe Unsigned
    }


eventCoder : Json.Coder Event
eventCoder =
    Json.object7
        { name = "Event"
        , description =
            [ "Event describing a JSON object sent in a room."
            ]
        , init = Event
        }
        (Json.field.required
            { fieldName = "content"
            , toField = .content
            , description =
                [ "The content of this event. The fields in this object will vary depending on the type of event."
                ]
            , coder = eventContentCoder
            }
        )
        (Json.field.required
            { fieldName = "event_id"
            , toField = .eventId
            , description =
                [ "The ID of this event, if applicable."
                ]
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "origin_server_ts"
            , toField = .originServerTs
            , description =
                [ "Timestamp in milliseconds on originating homeserver when this event was sent."
                ]
            , coder = Timestamp.coder
            }
        )
        (Json.field.required
            { fieldName = "sender"
            , toField = .sender
            , description =
                [ "The MXID of the user who sent this event."
                ]
            , coder = User.coder
            }
        )
        (Json.field.optional.value
            { fieldName = "state_key"
            , toField = .stateKey
            , description =
                [ "This key will only be present for state events. A unique key which defines the overwriting semantics for this piece of room state."
                ]
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "type"
            , toField = .eventType
            , description =
                [ "The type of event."
                ]
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "unsigned"
            , toField = .unsigned
            , description =
                [ "Information about this event which was not sent by the originating homeserver"
                ]
            , coder = unsignedCoder
            }
        )


eventToUpdate : String -> Event -> Event.Event
eventToUpdate roomId e =
    -- TODO: Use Recursion module for call stack safety
    { content = e.content
    , eventId = e.eventId
    , originServerTs = e.originServerTs
    , roomId = roomId
    , sender = e.sender
    , stateKey = e.stateKey
    , eventType = e.eventType
    , unsigned =
        Maybe.map
            (\(Unsigned u) ->
                Event.UnsignedData
                    { age = Just u.age
                    , transactionId = u.transactionId
                    , redactedBecause = Maybe.map (eventToUpdate roomId) u.redactedBecause
                    , prevContent = u.prevContent
                    }
            )
            e.unsigned
    }


type Unsigned
    = Unsigned { age : Int, prevContent : Maybe EventContent, redactedBecause : Maybe Event, transactionId : Maybe String }


unsignedCoder : Json.Coder Unsigned
unsignedCoder =
    Json.object4
        { name = "Unsigned"
        , description =
            [ "Information about the event that doesn't originate from the original homeserver."
            ]
        , init = \a b c d -> Unsigned { age = a, prevContent = b, redactedBecause = c, transactionId = d }
        }
        (Json.field.required
            { fieldName = "age"
            , toField = \(Unsigned u) -> u.age
            , description =
                [ "Time in milliseconds since the event was sent." ]
            , coder = Json.int
            }
        )
        (Json.field.optional.value
            { fieldName = "prev_content"
            , toField = \(Unsigned u) -> u.prevContent
            , description =
                [ "The previous content for this state. This will be present only for state events appearing in the timeline. If this is not a state event, or there is no previous content, this key will be missing."
                ]
            , coder = eventContentCoder
            }
        )
        (Json.field.optional.value
            { fieldName = "redacted_because"
            , toField = \(Unsigned u) -> u.redactedBecause
            , description =
                [ "The event that redacted this event, if any."
                ]
            , coder = Json.lazy (\() -> eventCoder)
            }
        )
        (Json.field.optional.value
            { fieldName = "transaction_id"
            , toField = \(Unsigned u) -> u.transactionId
            , description =
                [ "The transaction ID set when this message was sent. This key will only be present for message events sent by the device calling this API."
                ]
            , coder = Json.string
            }
        )


type alias EventContent =
    Json.Value


eventContentCoder : Json.Coder EventContent
eventContentCoder =
    Json.value
