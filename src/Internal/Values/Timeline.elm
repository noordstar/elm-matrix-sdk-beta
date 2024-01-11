module Internal.Values.Timeline exposing
    ( Batch, Timeline
    , empty, singleton
    , mostRecentEvents
    , addSync, insert
    , encode, decoder
    )

{-|


# Timeline

The Timeline data type represents a timeline in the Matrix room. The Matrix room
timeline is quite a complex data type, as it is constantly only partially known
by the Matrix client. This module exposes a data type that helps explore, track
and maintain this room state.


## Batch

@docs Batch, Timeline


## Create

@docs empty, singleton


## Query

@docs mostRecentEvents


## Manipulate

@docs addSync, insert


## JSON coder

@docs encode, decoder

-}

import FastDict as Dict exposing (Dict)
import Internal.Filter.Timeline as Filter exposing (Filter)
import Internal.Tools.Hashdict as Hashdict exposing (Hashdict)
import Internal.Tools.Iddict as Iddict exposing (Iddict)
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)


{-| A batch is a batch of events that is placed onto the Timeline. Functions
that require an insertion, generally require this data type.
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
        , mostRecentSync : ITokenPTR
        , tokens : Hashdict IToken
        }


{-| Opaque token value sent by the Matrix API
-}
type alias TokenValue =
    String


{-| When syncing a Matrix room to its most recent state, add the most recent
batch to the front of the Timeline.
-}
addSync : Batch -> Timeline -> Timeline
addSync batch timeline =
    case insertBatch batch timeline of
        ( Timeline tl, { start, end } ) ->
            let
                oldSync : ITokenPTR
                oldSync =
                    tl.mostRecentSync
            in
            Timeline { tl | mostRecentSync = end }
                |> connectITokenToIToken oldSync start


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
        , mostRecentSync = StartOfTimeline
        , tokens = Hashdict.empty .name
        }


{-| Decode a Timeline from a JSON value.
-}
decoder : D.Decoder Timeline
decoder =
    D.map5
        (\batches events filled sync tokens ->
            Timeline
                { batches = batches
                , events = events
                , filledBatches = filled
                , mostRecentSync = sync
                , tokens = tokens
                }
        )
        (D.field "batches" <| Iddict.decoder decoderIBatch)
        (D.map2 Tuple.pair
            (D.field "head" decoderIBatchPTR)
            (D.field "tail" <| D.list decoderIBatchPTR)
            |> D.keyValuePairs
            |> D.map Dict.fromList
            |> D.field "events"
        )
        (D.succeed 0)
        (D.field "mostRecentSync" decoderITokenPTR)
        (D.field "tokens" <| Hashdict.decoder .name decoderIToken)
        |> D.map recountFilledBatches


decoderIBatch : D.Decoder IBatch
decoderIBatch =
    D.map4 IBatch
        (D.field "events" <| D.list D.string)
        (D.field "filter" Filter.decoder)
        (D.field "start" decoderITokenPTR)
        (D.field "end" decoderITokenPTR)


decoderIBatchPTR : D.Decoder IBatchPTR
decoderIBatchPTR =
    D.map IBatchPTR decoderIBatchPTRValue


decoderIBatchPTRValue : D.Decoder IBatchPTRValue
decoderIBatchPTRValue =
    D.int


decoderIToken : D.Decoder IToken
decoderIToken =
    D.map5 IToken
        (D.field "name" decoderTokenValue)
        (D.field "starts" <| D.map Set.fromList <| D.list decoderIBatchPTRValue)
        (D.field "ends" <| D.map Set.fromList <| D.list decoderIBatchPTRValue)
        (D.field "inFrontOf" <| D.map Set.fromList <| D.list decoderITokenPTRValue)
        (D.field "behind" <| D.map Set.fromList <| D.list decoderITokenPTRValue)


decoderITokenPTR : D.Decoder ITokenPTR
decoderITokenPTR =
    D.oneOf
        [ D.map ITokenPTR decoderITokenPTRValue
        , D.null StartOfTimeline
        ]


decoderITokenPTRValue : D.Decoder ITokenPTRValue
decoderITokenPTRValue =
    D.string


decoderTokenValue : D.Decoder TokenValue
decoderTokenValue =
    D.string


{-| Encode a Timeline to a JSON value.
-}
encode : Timeline -> E.Value
encode (Timeline tl) =
    E.object
        [ ( "batches", Iddict.encode encodeIBatch tl.batches )
        , ( "events"
          , E.dict identity
                (\( head, tail ) ->
                    E.object
                        [ ( "head", encodeIBatchPTR head )
                        , ( "tail", E.list encodeIBatchPTR tail )
                        ]
                )
                (Dict.toCoreDict tl.events)
          )
        , ( "mostRecentSync", encodeITokenPTR tl.mostRecentSync )
        , ( "tokens", Hashdict.encode encodeIToken tl.tokens )
        ]


encodeIBatch : IBatch -> E.Value
encodeIBatch batch =
    E.object
        [ ( "events", E.list E.string batch.events )
        , ( "filter", Filter.encode batch.filter )
        , ( "start", encodeITokenPTR batch.start )
        , ( "end", encodeITokenPTR batch.end )
        ]


encodeIBatchPTR : IBatchPTR -> E.Value
encodeIBatchPTR (IBatchPTR value) =
    encodeIBatchPTRValue value


encodeIBatchPTRValue : IBatchPTRValue -> E.Value
encodeIBatchPTRValue =
    E.int


encodeIToken : IToken -> E.Value
encodeIToken itoken =
    E.object
        [ ( "name", encodeTokenValue itoken.name )
        , ( "starts", E.set encodeIBatchPTRValue itoken.starts )
        , ( "ends", E.set encodeIBatchPTRValue itoken.ends )
        , ( "inFrontOf", E.set encodeITokenPTRValue itoken.inFrontOf )
        , ( "behind", E.set encodeITokenPTRValue itoken.behind )
        ]


encodeITokenPTR : ITokenPTR -> E.Value
encodeITokenPTR token =
    case token of
        ITokenPTR value ->
            encodeITokenPTRValue value

        StartOfTimeline ->
            E.null


encodeITokenPTRValue : ITokenPTRValue -> E.Value
encodeITokenPTRValue =
    E.string


encodeTokenValue : TokenValue -> E.Value
encodeTokenValue =
    E.string


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
mostRecentEvents : Filter -> Timeline -> List String
mostRecentEvents _ _ =
    []


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
    addSync b empty
