module Internal.Values.Room exposing
    ( Room, init
    , RoomUpdate(..), update
    , Batch, addBatch, addSync, addEvents, mostRecentEvents
    , getAccountData, setAccountData
    , coder, encode, decode
    )

{-|


# Room

What is usually called a chat, a channel, a conversation or a group chat on
other platforms, the term used in Matrix is a "room". A room is a conversation
where a group of users talk to each other.

This module is the internal module of a room. Its functions serve the update
the local room state. Its changes do **NOT** reflect the actual room state on
the homeserver: as a matter of fact, these functions are meant to help the local
room state reflect the homeserver state of the room.


## Room

@docs Room, init


## Update

@docs RoomUpdate, update


## Timeline

@docs Batch, addBatch, addSync, addEvents, mostRecentEvents


## Account data

@docs getAccountData, setAccountData


## JSON coding

@docs coder, encode, decode

-}

import FastDict as Dict exposing (Dict)
import Internal.Config.Log exposing (log)
import Internal.Config.Text as Text
import Internal.Filter.Timeline as Filter exposing (Filter)
import Internal.Tools.Hashdict as Hashdict exposing (Hashdict)
import Internal.Tools.Json as Json
import Internal.Tools.StrippedEvent as StrippedEvent exposing (StrippedEvent)
import Internal.Values.Event as Event exposing (Event)
import Internal.Values.StateManager as StateManager exposing (StateManager)
import Internal.Values.Timeline as Timeline exposing (Timeline)
import Internal.Values.User exposing (User)
import Json.Encode as E


{-| The Batch is a group of new events from somewhere in the timeline.
-}
type alias Batch =
    { events : List Event, filter : Filter, start : Maybe String, end : String }


{-| The Matrix Room is a representation of a Matrix Room as portrayed by the
homeserver.
-}
type alias Room =
    { accountData : Dict String Json.Value
    , ephemeral : List StrippedEvent
    , events : Hashdict Event
    , roomId : String
    , state : StateManager
    , timeline : Timeline
    }


{-| The RoomUpdate type explains how to update a room based on new information
from the Matrix API.
-}
type RoomUpdate
    = AddEvent Event
    | AddSync Batch
    | Invite User
    | More (List RoomUpdate)
    | Optional (Maybe RoomUpdate)
    | SetAccountData String Json.Value
    | SetEphemeral (List { eventType : String, content : Json.Value })


{-| Add new events to the Room's event directory + Room's timeline.
-}
addEventsToTimeline : (Timeline.Batch -> Timeline -> Timeline) -> Batch -> Room -> Room
addEventsToTimeline f { events, filter, start, end } room =
    let
        batch : Timeline.Batch
        batch =
            { events = List.map .eventId events
            , filter = filter
            , start = start
            , end = end
            }
    in
    { room
        | events = List.foldl Hashdict.insert room.events events
        , timeline = f batch room.timeline
    }


{-| Add a batch of events to the Room.
-}
addBatch : Batch -> Room -> Room
addBatch =
    addEventsToTimeline Timeline.insert


{-| Add events to the room, with no particular information about their location
on the timeline. This is especially helpful for events that offer information
like the room's state, given that it is essential to know them but they have
often been sent a long time ago.
-}
addEvents : List Event -> Room -> Room
addEvents events room =
    { room
        | events = List.foldl Hashdict.insert room.events events
    }


{-| Add a new sync to the Room. The difference with the
[addBatch](Internal-Values-Room#addBatch) function is that this function
explicitly tells the Timeline that it is at the front of the timeline.
-}
addSync : Batch -> Room -> Room
addSync =
    addEventsToTimeline Timeline.addSync


{-| Define how a Room can be encoded and decoded to and from a JavaScript value.
-}
coder : Json.Coder Room
coder =
    Json.object6
        { name = Text.docs.room.name
        , description = Text.docs.room.description
        , init = Room
        }
        (Json.field.optional.withDefault
            { fieldName = "accountData"
            , toField = .accountData
            , description = Text.fields.room.accountData
            , coder = Json.fastDict Json.value
            , default = ( Dict.empty, [] )
            , defaultToString = Json.encode (Json.fastDict Json.value) >> E.encode 0
            }
        )
        (Json.field.optional.withDefault
            { fieldName = "ephemeral"
            , toField = .ephemeral
            , description = Text.fields.room.ephemeral
            , coder = Json.list StrippedEvent.coder
            , default = ( [], [] )
            , defaultToString = Json.encode (Json.list StrippedEvent.coder) >> E.encode 0
            }
        )
        (Json.field.optional.withDefault
            { fieldName = "events"
            , toField = .events
            , description = Text.fields.room.events
            , coder = Hashdict.coder .eventId Event.coder
            , default = ( Hashdict.empty .eventId, [ log.warn "Found a room with no known events! Is it empty?" ] )
            , defaultToString = Json.encode (Hashdict.coder .eventId Event.coder) >> E.encode 0
            }
        )
        (Json.field.required
            { fieldName = "roomId"
            , toField = .roomId
            , description = Text.fields.room.roomId
            , coder = Json.string
            }
        )
        (Json.field.optional.withDefault
            { fieldName = "state"
            , toField = .state
            , description = Text.fields.room.state
            , coder = StateManager.coder
            , default = ( StateManager.empty, [] )
            , defaultToString = Json.encode StateManager.coder >> E.encode 0
            }
        )
        (Json.field.optional.withDefault
            { fieldName = "timeline"
            , toField = .timeline
            , description = Text.fields.room.timeline
            , coder = Timeline.coder
            , default = ( Timeline.empty, [] )
            , defaultToString = Json.encode Timeline.coder >> E.encode 0
            }
        )


{-| Decode a Room from JSON format.
-}
decode : Json.Decoder Room
decode =
    Json.decode coder


{-| Encode a Room into JSON format.
-}
encode : Json.Encoder Room
encode =
    Json.encode coder


{-| Get a piece of account data as information from the room.
-}
getAccountData : String -> Room -> Maybe Json.Value
getAccountData key room =
    Dict.get key room.accountData


{-| Create an empty room for which nothing is known.
-}
init : String -> Room
init roomId =
    { accountData = Dict.empty
    , ephemeral = []
    , events = Hashdict.empty .eventId
    , roomId = roomId
    , state = StateManager.empty
    , timeline = Timeline.empty
    }


{-| Get the most recent events from the timeline.
-}
mostRecentEvents : Room -> List Event
mostRecentEvents room =
    room.timeline
        |> Timeline.mostRecentEvents Filter.pass
        |> List.map (List.filterMap (\e -> Hashdict.get e room.events))
        |> List.sortBy List.length
        -- Get the largest list of events
        |> List.head
        |> Maybe.withDefault []


{-| Set a piece of account data as information about the room.
-}
setAccountData : String -> Json.Value -> Room -> Room
setAccountData key value room =
    { room | accountData = Dict.insert key value room.accountData }


{-| Update the Room based on given instructions.
-}
update : RoomUpdate -> Room -> Room
update ru room =
    case ru of
        AddEvent _ ->
            -- TODO: Add event
            room

        AddSync batch ->
            addSync batch room

        Invite _ ->
            -- TODO: Invite user
            room

        More items ->
            List.foldl update room items

        Optional (Just u) ->
            update u room

        Optional Nothing ->
            room

        SetAccountData key value ->
            setAccountData key value room

        SetEphemeral eph ->
            { room | ephemeral = eph }
