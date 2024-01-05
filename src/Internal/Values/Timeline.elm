module Internal.Values.Timeline exposing
    ( Batch, fromToken, fromSlice
    , Timeline
    , empty, singleton
    , mostRecentEvents
    , addSync, insert
    )

{-|


# Timeline

The Timeline data type represents a timeline in the Matrix room. The Matrix room
timeline is quite a complex data type, as it is constantly only partially known
by the Matrix client. This module exposes a data type that helps explore, track
and maintain this room state.


## Batch

@docs Batch, fromToken, fromSlice


## Timeline

@docs Timeline


## Create

@docs empty, singleton


## Query

@docs mostRecentEvents


## Manipulate

@docs addSync, insert

-}

import Internal.Filter.Timeline as Filter exposing (Filter)


{-| The Timeline type represents the timeline state in a Matrix room.
-}
type Timeline
    = Timeline


{-| A batch is a batch of events that is placed onto the Timeline. Functions
that require an insertion, generally require this data type.
-}
type Batch
    = Batch


{-| When syncing a Matrix room to its most recent state, add the most recent
batch to the front of the Timeline.
-}
addSync : Batch -> Timeline -> Timeline
addSync _ timeline =
    timeline


{-| Create a new empty timeline.
-}
empty : Timeline
empty =
    Timeline


{-| Turn a single token into a batch.
-}
fromToken : String -> Batch
fromToken _ =
    Batch


{-| Turn a slice of events into a batch.

NOTE: `start` must generally be a value. If it is `Nothing`, then it is
connected until the start of the timeline.

-}
fromSlice : { start : Maybe String, events : List String, filter : Filter, end : String } -> Batch
fromSlice _ =
    Batch


{-| Insert a batch anywhere else in the timeline.
-}
insert : Batch -> Timeline -> Timeline
insert _ timeline =
    timeline


{-| Under a given filter, find the most recent events.
-}
mostRecentEvents : Filter -> Timeline -> List String
mostRecentEvents _ _ =
    []


{-| Create a timeline with a single batch inserted. This batch is considered the
most recent batch, as if created by a sync.
-}
singleton : Batch -> Timeline
singleton b =
    addSync b empty
