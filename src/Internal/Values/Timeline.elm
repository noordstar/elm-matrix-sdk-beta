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
import Internal.Tools.Mashdict as Mashdict exposing (Mashdict)
import Internal.Tools.Iddict as Iddict exposing (Iddict)
import FastDict as Dict exposing (Dict)
import Set exposing (Set)


{-| A batch is a batch of events that is placed onto the Timeline. Functions
that require an insertion, generally require this data type.
-}
type Batch
    = StartOfTimeline
    | BatchToken String
    | BatchSlice Batch (List String) Filter String

{-| Internal batch that's being saved by the Timeline to track a list of events.
-}
type alias IBatch =
    { events : List String
    , filter : Filter
    , start : ITokenPTR
    , end : ITokenPTR
    }

{-| Pointer to an IBatch in the Timeline.
-}
type IBatchPTR = IBatchPTR Int

{-| Internal token value that's being stored by the Timeline.

If name is `Nothing`, it indicates the start of the timeline.
-}
type alias IToken =
    { name : Maybe String
    , starts : Set Int      -- This itoken starts the following batches
    , ends : Set Int        -- This itoken ends the following batches
    , inFrontOf : Set Int   -- This itoken is in front of the following tokens
    , behind : Set Int      -- This itoken is behind the following tokens
    }

{-| Pointer to an IToken in the Timeline.
-}
type ITokenPTR = ITokenPTR String

{-| The Timeline type represents the timeline state in a Matrix room.

Following the description of the Matrix spec, a timeline contains the following
items:

- Events that indicate timeline events
- Batch values that can be used to paginate through the timeline

The topological shape of the timeline makes older API responses somewhat
unreliable - as a result,

-}
type Timeline
    = Timeline
        { batches : Iddict IBatch
        , mostRecentSync : ITokenPTR
        , tokens : Mashdict IToken
        }


{-| When syncing a Matrix room to its most recent state, add the most recent
batch to the front of the Timeline.
-}
addSync : Batch -> Timeline -> Timeline
addSync _ timeline =
    timeline

{-| Connect two tokens to each other, revealing their relative location.
-}
connectITokentoIToken : ITokenPTR -> ITokenPTR -> Timeline -> Timeline
connectITokentoIToken (ITokenPTR early) (ITokenPTR late) (Timeline tl) =
    Timeline
        { tl
        | tokens =
            tl.tokens
                |> Iddict.map early
                    (\data ->
                        { data | behind = Set.insert late data.behind }
                    )
                |> Iddict.map late
                    (\data -> 
                        { data | inFrontOf = Set.insert early data.inFrontOf }
                    )
        }

{-| Create a new empty timeline.
-}
empty : Timeline
empty =
    case Iddict.singleton Nothing of
        ( key, iddict ) ->
            Timeline
                { batches = Iddict.empty
                , mostRecentSync = ITokenPTR key
                , tokens = iddict
                , tokenToPtr = Dict.empty
                }

{-| Get an IBatch from the Timeline.
-}
getIBatch : IBatchPTR -> Timeline -> Maybe IBatch
getIBatch (IBatchPTR ptr) (Timeline { batches }) =
    Iddict.get ptr batches

getITokenFromPTR : ITokenPTR -> Timeline -> Maybe IToken
getITokenFromPTR (ITokenPTR ptr) ( Timeline { tokens }) =
    Iddict.get ptr tokens

{-| Turn a single token into a batch.
-}
fromToken : String -> Batch
fromToken token =
    BatchToken token


{-| Turn a slice of events into a batch.

NOTE: `start` must generally be a value. If it is `Nothing`, then it is
connected until the start of the timeline.

-}
fromSlice : { start : Maybe String, events : List String, filter : Filter, end : String } -> Batch
fromSlice data =
    BatchSlice
        ( case data.start of
                Just s ->
                    BatchToken s
                
                Nothing ->
                    StartOfTimeline
        )
        data.events data.filter data.end


{-| Insert a batch anywhere else in the timeline.
-}
insert : Batch -> Timeline -> Timeline
insert batch (Timeline tl) =
    (Timeline tl)

-- {-| Insert a batch anywhere else in the timeline, and gain a ptr to its
-- location.
-- -}
-- insertBatch : Batch -> Timeline -> { start : ITokenPTR, end : ITokenPTR, tl : Timeline }
-- insertBatch batch (Timeline tl) =
--     case batch of
--         StartOfTimeline ->
--             case Iddict.insert Nothing tl.tokens of
--                 ( key, iddict ) ->
--                     { start = ITokenPTR key
--                     , end = ITokenPTR key
--                     , tl = Timeline { tl | tokens = iddict }
--                     }
        
--         BatchToken token ->
--             -- TODO: Do not insert if it already exists
--             case Iddict.insert (Just token) tl.tokens of
--                 ( key, iddict ) ->
--                     { start = ITokenPTR key
--                     , end = ITokenPTR key
--                     , tl = Timeline
--                             { tl
--                             | tokens = iddict
--                             , tokenToPtr =
--                                 Dict.insert token (ITokenPTR key) tl.tokenToPtr
--                             }
--                     }
        
--         BatchSlice prevBatch events filter end ->
--             -- Insert previous batch
--             case insertBatch prevBatch (Timeline tl) of
--                 result ->
--                     case result.tl of
--                         (Timeline tl2) ->
--                             { start = result.start
--                             , end = 
--                             }

{-| Insert an internal batch into the timeline, and determine its result.
-}
insertIBatch : IBatch -> Timeline -> ( IBatchPTR, Timeline )
insertIBatch ibatch (Timeline tl) =
    case Iddict.insert ibatch tl.batches of
        ( key, iddict ) ->
            ( IBatchPTR key, Timeline { tl | batches = iddict } )

insertIToken : IToken -> Timeline -> ( ITokenPTR, Timeline )
insertIToken itoken (Timeline tl) =
    case Maybe.andThen (\n -> Dict.get n tl.tokenToPtr) itoken.name of
        -- Already exists: merge
        Just ((ITokenPTR ptr) as pointer) ->
            ( pointer
            , Timeline
                { tl
                | tokens =
                    Iddict.map ptr
                        (\data ->
                            { name = data.name
                            , 
                            }
                        )
                }
            )

        -- Doesn't exist yet: insert!
        Nothing ->
            (ITokenPTR 0, Timeline tl)



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
