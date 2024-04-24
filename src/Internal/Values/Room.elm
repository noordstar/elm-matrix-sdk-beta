module Internal.Values.Room exposing
    ( Room, init
    , Batch, addBatch, addSync, addEvents
    , getAccountData, setAccountData
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


## Timeline

@docs Batch, addBatch, addSync, addEvents


## Account data

@docs getAccountData, setAccountData

-}

import FastDict as Dict exposing (Dict)
import Internal.Config.Log exposing (log)
import Internal.Config.Text as Text
import Internal.Filter.Timeline exposing (Filter)
import Internal.Tools.Hashdict as Hashdict exposing (Hashdict)
import Internal.Tools.Json as Json
import Internal.Values.Event as Event exposing (Event)
import Internal.Values.StateManager as StateManager exposing (StateManager)
import Internal.Values.Timeline as Timeline exposing (Timeline)
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
    , events : Hashdict Event
    , roomId : String
    , state : StateManager
    , timeline : Timeline
    }


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
    Json.object5
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
    , events = Hashdict.empty .eventId
    , roomId = roomId
    , state = StateManager.empty
    , timeline = Timeline.empty
    }


{-| Set a piece of account data as information about the room.
-}
setAccountData : String -> Json.Value -> Room -> Room
setAccountData key value room =
    { room | accountData = Dict.insert key value room.accountData }
