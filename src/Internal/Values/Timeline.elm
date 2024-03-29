module Internal.Values.Timeline exposing
    ( Batch, Timeline
    , empty, singleton
    , mostRecentEvents, mostRecentEventsFrom
    , insert
    , coder
    )

{-|


# Timeline

The Timeline data type represents a timeline in the Matrix room. The Matrix room
timeline is quite a complex data type, as it is constantly only partially known
by the Matrix client. This module exposes a data type that helps explore, track
and maintain this room state.

This design of the timeline uses the batches as waypoints to maintain an order.
The Matrix API often returns batches that have the following four pieces of
information:

1.  A list of events.
2.  A filter for which all of the events meet the criteria.
3.  An end batch token.
4.  _(Optional)_ A start batch token. If it is not provided, it is the start of
    the timeline.

Here's an example of such a timeline batch:

           |-->[■]->[■]->[●]->[■]->[■]->[●]-->|
           |                                  |
           |<-- filter: only ■ and ●, no ★ -->|
           |                                  |
         start:                              end:
        <token_1>                          <token_2>

When the Matrix API later returns a batch token that starts with `<token_2>`,
we know that we can connect it to the batch above and make a longer list of
events!


## Batch

@docs Batch, Timeline


## Create

@docs empty, singleton


## Query

@docs mostRecentEvents, mostRecentEventsFrom


## Manipulate

@docs addSync, insert


## JSON coder

@docs coder, encode, decoder

-}

import FastDict as Dict exposing (Dict)
import Internal.Filter.Timeline as Filter exposing (Filter)
import Internal.Tools.Hashdict as Hashdict exposing (Hashdict)
import Internal.Tools.Iddict as Iddict exposing (Iddict)
import Internal.Tools.Json as Json
import Internal.Config.Text as Text
import Recursion
import Recursion.Traverse
import Set exposing (Set)


{-| A batch is a batch of events that is placed onto the Timeline. Functions
that require an insertion, generally require this data type.

If the `start` value is `Nothing`, it is either the start of the timeline or the
start of the timeline part that the user is allowed to view.
-}
type alias Batch =
    { events : List String
    , filter : Filter
    , start : Maybe TokenValue
    , end : TokenValue
    }


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
type IBatchPTR
    = IBatchPTR IBatchPTRValue


{-| Location indicator of an IBatch in the Timeline.
-}
type alias IBatchPTRValue =
    Int


{-| Internal token value that's being stored by the Timeline.

If name is `Nothing`, it indicates the start of the timeline.

-}
type alias IToken =
    { name : TokenValue
    , starts : Set IBatchPTRValue -- This itoken starts the following batches
    , ends : Set IBatchPTRValue -- This itoken ends the following batches
    , inFrontOf : Set ITokenPTRValue -- This itoken is in front of the following tokens
    , behind : Set ITokenPTRValue -- This itoken is behind the following tokens
    }


{-| Pointer to an IToken in the Timeline.
-}
type ITokenPTR
    = ITokenPTR ITokenPTRValue
    | StartOfTimeline


{-| Location indicator of an IToken in the Timeline.
-}
type alias ITokenPTRValue =
    String


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
        , events : Dict String ( IBatchPTR, List IBatchPTR )
        , filledBatches : Int
        , mostRecentBatch : ITokenPTR
        , tokens : Hashdict IToken
        }


{-| Opaque token value sent by the Matrix API
-}
type alias TokenValue =
    String

coder : Json.Coder Timeline
coder =
    Json.object5
        { name = Text.docs.timeline.name
        , description = Text.docs.timeline.description
        , init =
            (\a b c d e ->
                Timeline
                    { batches = a, events = b, filledBatches = c
                    , mostRecentBatch = d, tokens = e
                    }
            )
        }
        ( Json.field.required
            { fieldName = "batches"
            , toField = (\(Timeline t) -> t.batches)
            , description = Text.fields.timeline.batches
            , coder = Iddict.coder coderIBatch
            }
        )
        ( Json.field.required
            { fieldName = "events"
            , toField = (\(Timeline t) -> t.events)
            , description = Text.fields.timeline.events
            , coder = Json.fastDict (Json.listWithOne coderIBatchPTR)
            }
        )
        ( Json.field.optional.withDefault
            { fieldName = "filledBatches"
            , toField = (\(Timeline t) -> t.filledBatches)
            , description = Text.fields.timeline.filledBatches
            , coder = Json.int
            , default = ( 0, [] )
            , defaultToString = String.fromInt
            }
        )
        ( Json.field.required
            { fieldName = "mostRecentBatch"
            , toField = (\(Timeline t) -> t.mostRecentBatch)
            , description = Text.fields.timeline.mostRecentBatch
            , coder = coderITokenPTR
            }
        )
        ( Json.field.required
            { fieldName = "tokens"
            , toField = (\(Timeline t) -> t.tokens)
            , description = Text.fields.timeline.tokens
            , coder = Hashdict.coder .name coderIToken
            }
        )

coderIBatch : Json.Coder IBatch
coderIBatch =
    Json.object4
        { name = Text.docs.ibatch.name
        , description = Text.docs.ibatch.description
        , init = IBatch
        }
        ( Json.field.required
            { fieldName = "events"
            , toField = .events
            , description = Text.fields.ibatch.events
            , coder = Json.list Json.string
            }
        )
        ( Json.field.required
            { fieldName = "filter"
            , toField = .filter
            , description = Text.fields.ibatch.filter
            , coder = Filter.coder
            }
        )
        ( Json.field.required
            { fieldName = "start"
            , toField = .start
            , description = Text.fields.ibatch.start
            , coder = coderITokenPTR
            }
        )
        ( Json.field.required
            { fieldName = "end"
            , toField = .end
            , description = Text.fields.ibatch.end
            , coder = coderITokenPTR
            }
        )

coderIBatchPTR : Json.Coder IBatchPTR
coderIBatchPTR =
    Json.map
        { name = Text.docs.itoken.name
        , description = Text.docs.itoken.description
        , back = (\(IBatchPTR value) -> value)
        , forth = IBatchPTR
        }
        coderIBatchPTRValue

coderIBatchPTRValue : Json.Coder IBatchPTRValue
coderIBatchPTRValue = Json.int

coderIToken : Json.Coder IToken
coderIToken =
    Json.object5
        { name = Text.docs.itoken.name
        , description = Text.docs.itoken.description
        , init = IToken
        }
        ( Json.field.required
            { fieldName = "name"
            , toField = .name
            , description = Text.fields.itoken.name
            , coder = coderTokenValue
            }
        )
        ( Json.field.optional.withDefault
            { fieldName = "starts"
            , toField = .starts
            , description = Text.fields.itoken.starts
            , coder = Json.set coderIBatchPTRValue
            , default = ( Set.empty, [] )
            , defaultToString = always "[]"
            }
        )
        ( Json.field.optional.withDefault
            { fieldName = "ends"
            , toField = .ends
            , description = Text.fields.itoken.ends
            , coder = Json.set coderIBatchPTRValue
            , default = ( Set.empty, [] )
            , defaultToString = always "[]"
            }
        )
        ( Json.field.optional.withDefault
            { fieldName = "inFrontOf"
            , toField = .inFrontOf
            , description = Text.fields.itoken.inFrontOf
            , coder = Json.set coderITokenPTRValue
            , default = ( Set.empty, [] )
            , defaultToString = always "[]"
            }
        )
        ( Json.field.optional.withDefault
            { fieldName = "behind"
            , toField = .behind
            , description = Text.fields.itoken.behind
            , coder = Json.set coderITokenPTRValue
            , default = ( Set.empty, [] )
            , defaultToString = always "[]"
            }
        )

coderITokenPTR : Json.Coder ITokenPTR
coderITokenPTR =
    Json.maybe coderITokenPTRValue
        |> Json.map
            { name = Text.mappings.itokenPTR.name
            , description = Text.mappings.itokenPTR.description
            , back =
                (\itokenptr ->
                    case itokenptr of
                        ITokenPTR name ->
                            Just name
                        
                        StartOfTimeline ->
                            Nothing
                )
            , forth =
                (\value ->
                    case value of
                        Just name ->
                            ITokenPTR name
                        
                        Nothing ->
                            StartOfTimeline
                )
            }

coderITokenPTRValue : Json.Coder ITokenPTRValue
coderITokenPTRValue = Json.string

coderTokenValue : Json.Coder TokenValue
coderTokenValue = Json.string

{-| Append a token at the end of a batch.
-}
connectIBatchToIToken : IBatchPTR -> ITokenPTR -> Timeline -> Timeline
connectIBatchToIToken (IBatchPTR bptr) pointer (Timeline tl) =
    case pointer of
        StartOfTimeline ->
            Timeline tl

        ITokenPTR tptr ->
            Timeline
                { tl
                    | batches =
                        Iddict.map bptr
                            (\batch -> { batch | end = pointer })
                            tl.batches
                    , tokens =
                        Hashdict.map tptr
                            (\token -> { token | ends = Set.insert bptr token.ends })
                            tl.tokens
                }


{-| Append a token at the start of a batch.
-}
connectITokenToIBatch : ITokenPTR -> IBatchPTR -> Timeline -> Timeline
connectITokenToIBatch pointer (IBatchPTR bptr) (Timeline tl) =
    case pointer of
        StartOfTimeline ->
            Timeline tl

        ITokenPTR tptr ->
            Timeline
                { tl
                    | tokens =
                        Hashdict.map tptr
                            (\token -> { token | starts = Set.insert bptr token.starts })
                            tl.tokens
                    , batches =
                        Iddict.map bptr
                            (\batch -> { batch | start = pointer })
                            tl.batches
                }


{-| Connect two tokens to each other, revealing their relative location.
-}
connectITokenToIToken : ITokenPTR -> ITokenPTR -> Timeline -> Timeline
connectITokenToIToken pointer1 pointer2 (Timeline tl) =
    case ( pointer1, pointer2 ) of
        ( ITokenPTR early, ITokenPTR late ) ->
            if early == late then
                Timeline tl

            else
                Timeline
                    { tl
                        | tokens =
                            tl.tokens
                                |> Hashdict.map early
                                    (\data ->
                                        { data | behind = Set.insert late data.behind }
                                    )
                                |> Hashdict.map late
                                    (\data ->
                                        { data | inFrontOf = Set.insert early data.inFrontOf }
                                    )
                    }

        ( _, _ ) ->
            Timeline tl


{-| Create a new empty timeline.
-}
empty : Timeline
empty =
    Timeline
        { batches = Iddict.empty
        , events = Dict.empty
        , filledBatches = 0
        , mostRecentBatch = StartOfTimeline
        , tokens = Hashdict.empty .name
        }


{-| Get an IBatch from the Timeline.
-}
getIBatch : IBatchPTR -> Timeline -> Maybe IBatch
getIBatch (IBatchPTR ptr) (Timeline { batches }) =
    Iddict.get ptr batches


{-| Get an IToken from the Timeline.
-}
getITokenFromPTR : ITokenPTR -> Timeline -> Maybe IToken
getITokenFromPTR pointer (Timeline { tokens }) =
    case pointer of
        ITokenPTR ptr ->
            Hashdict.get ptr tokens

        StartOfTimeline ->
            Nothing


{-| Insert a batch anywhere else in the timeline.
-}
insert : Batch -> Timeline -> Timeline
insert batch timeline =
    timeline
        |> insertBatch batch
        |> Tuple.first


{-| Insert a batch into the timeline.
-}
insertBatch : Batch -> Timeline -> ( Timeline, { start : ITokenPTR, end : ITokenPTR } )
insertBatch batch timeline =
    case batch.start of
        Just start ->
            timeline
                |> invokeIToken start
                |> Tuple.mapSecond (invokeIToken batch.end)
                |> (\( startPTR, ( endPTR, newTimeline ) ) ->
                        ( insertIBatch
                            { events = batch.events
                            , filter = batch.filter
                            , start = startPTR
                            , end = endPTR
                            }
                            newTimeline
                        , { start = startPTR, end = endPTR }
                        )
                   )

        Nothing ->
            timeline
                |> invokeIToken batch.end
                |> (\( endPTR, newTimeline ) ->
                        ( insertIBatch
                            { events = batch.events
                            , filter = batch.filter
                            , start = StartOfTimeline
                            , end = endPTR
                            }
                            newTimeline
                        , { start = StartOfTimeline, end = endPTR }
                        )
                   )


{-| Insert an internal batch into the timeline, and determine its result.
-}
insertIBatch : IBatch -> Timeline -> Timeline
insertIBatch ibatch (Timeline tl) =
    case Iddict.insert ibatch tl.batches of
        ( batchPTR, newBatches ) ->
            { tl
                | batches = newBatches
                , events =
                    List.foldl
                        (\event dict ->
                            Dict.update event
                                (\value ->
                                    case value of
                                        Nothing ->
                                            Just ( IBatchPTR batchPTR, [] )

                                        Just ( head, tail ) ->
                                            Just ( IBatchPTR batchPTR, head :: tail )
                                )
                                dict
                        )
                        tl.events
                        ibatch.events
                , filledBatches =
                    if List.isEmpty ibatch.events then
                        tl.filledBatches

                    else
                        tl.filledBatches + 1
            }
                |> Timeline
                |> connectITokenToIBatch ibatch.start (IBatchPTR batchPTR)
                |> connectIBatchToIToken (IBatchPTR batchPTR) ibatch.end


{-| Invoke an itoken to guarantee that it exists.
-}
invokeIToken : TokenValue -> Timeline -> ( ITokenPTR, Timeline )
invokeIToken value (Timeline tl) =
    ( ITokenPTR value
    , Timeline
        { tl
            | tokens =
                case Hashdict.get value tl.tokens of
                    Just _ ->
                        tl.tokens

                    Nothing ->
                        Hashdict.insert
                            { name = value
                            , starts = Set.empty
                            , ends = Set.empty
                            , inFrontOf = Set.empty
                            , behind = Set.empty
                            }
                            tl.tokens
        }
    )


{-| Under a given filter, find the most recent events.
-}
mostRecentEvents : Filter -> Timeline -> List (List String)
mostRecentEvents filter (Timeline timeline) =
    mostRecentFrom filter (Timeline timeline) timeline.mostRecentBatch


mostRecentEventsFrom : Filter -> ITokenPTRValue -> Timeline -> List (List String)
mostRecentEventsFrom filter tokenName timeline =
    mostRecentFrom filter timeline (ITokenPTR tokenName)


{-| Under a given filter, starting from a given ITokenPTR, find the most recent
events.
-}
mostRecentFrom : Filter -> Timeline -> ITokenPTR -> List (List String)
mostRecentFrom filter timeline ptr =
    Recursion.runRecursion
        (\p ->
            case getITokenFromPTR p.ptr timeline of
                Nothing ->
                    Recursion.base []

                Just token ->
                    if Set.member token.name p.visited then
                        Recursion.base []

                    else
                        token.ends
                            |> Set.toList
                            |> List.filterMap (\bptrv -> getIBatch (IBatchPTR bptrv) timeline)
                            |> List.filter (\ibatch -> Filter.subsetOf ibatch.filter filter)
                            |> Recursion.Traverse.traverseList
                                (\ibatch ->
                                    Recursion.recurseThen
                                        { ptr = ibatch.start, visited = Set.insert token.name p.visited }
                                        (\optionalTimelines ->
                                            case optionalTimelines of
                                                [] ->
                                                    List.singleton ibatch.events
                                                        |> Recursion.base

                                                _ :: _ ->
                                                    optionalTimelines
                                                        |> List.map
                                                            (\outTimeline ->
                                                                List.append outTimeline ibatch.events
                                                            )
                                                        |> Recursion.base
                                        )
                                )
                            |> Recursion.map List.concat
        )
        { ptr = ptr, visited = Set.empty }


{-| Recount the Timeline's amount of filled batches. Since the Timeline
automatically tracks the count on itself, this is generally exclusively used in
specific scenarios like decoding JSON values.
-}
recountFilledBatches : Timeline -> Timeline
recountFilledBatches (Timeline tl) =
    Timeline
        { tl
            | filledBatches =
                tl.batches
                    |> Iddict.values
                    |> List.filter (\v -> v.events /= [])
                    |> List.length
        }


{-| Create a timeline with a single batch inserted. This batch is considered the
most recent batch, as if created by a sync.
-}
singleton : Batch -> Timeline
singleton b =
    insert b empty
