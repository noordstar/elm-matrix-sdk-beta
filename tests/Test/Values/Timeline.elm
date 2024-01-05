module Test.Values.Timeline exposing (..)

import Fuzz exposing (Fuzzer)
import Internal.Filter.Timeline as Filter exposing (Filter)
import Internal.Values.Timeline as Timeline exposing (Batch, Timeline)
import Test exposing (..)
import Test.Filter.Timeline as TestFilter
import Expect


fuzzer : Fuzzer Timeline
fuzzer =
    Fuzz.map2
        (\makers filter ->
            case makers of
                [] ->
                    Timeline.empty

                head :: tail ->
                    List.foldl
                        (\maker ( prevToken, timeline ) ->
                            case maker of
                                Sync start events end ->
                                    ( end
                                    , Timeline.addSync
                                        (Timeline.fromSlice
                                            { start =
                                                start
                                                    |> Maybe.withDefault prevToken
                                                    |> Maybe.Just
                                            , events = events
                                            , filter = filter
                                            , end = end
                                            }
                                        )
                                        timeline
                                    )

                                Get start events efilter end ->
                                    ( prevToken
                                    , Timeline.insert
                                        (Timeline.fromSlice
                                            { start = start
                                            , events = events
                                            , filter = Filter.and filter efilter
                                            , end = end
                                            }
                                        )
                                        timeline
                                    )
                        )
                        (case head of
                            Sync start events end ->
                                ( end
                                , Timeline.addSync
                                    (Timeline.fromSlice
                                        { start = start
                                        , events = events
                                        , filter = filter
                                        , end = end
                                        }
                                    )
                                    Timeline.empty
                                )

                            Get start events efilter end ->
                                ( end
                                , Timeline.addSync
                                    (Timeline.fromSlice
                                        { start = start
                                        , events = events
                                        , filter = Filter.and filter efilter
                                        , end = end
                                        }
                                    )
                                    Timeline.empty
                                )
                        )
                        tail
                        |> Tuple.second
        )
        (Fuzz.list fuzzerMaker)
        TestFilter.fuzzer


fuzzerBatch : Fuzzer Batch
fuzzerBatch =
    Fuzz.oneOf
        [ Fuzz.map Timeline.fromToken Fuzz.string
        , Fuzz.map4
            (\start events filter end ->
                Timeline.fromSlice
                    { start = start
                    , events = events
                    , filter = filter
                    , end = end
                    }
            )
            (Fuzz.maybe Fuzz.string)
            (Fuzz.list Fuzz.string)
            TestFilter.fuzzer
            Fuzz.string
        ]


type FuzzMaker
    = Sync (Maybe String) (List String) String
    | Get (Maybe String) (List String) Filter String


fuzzerMaker : Fuzzer FuzzMaker
fuzzerMaker =
    Fuzz.frequency
        [ ( 30, Fuzz.map (Sync Nothing []) Fuzz.string )
        , ( 10
          , Fuzz.map2 (Sync Nothing)
                (Fuzz.listOfLengthBetween 1 32 Fuzz.string)
                Fuzz.string
          )
        , ( 1
          , Fuzz.map3 (\start events end -> Sync (Just start) events end)
                Fuzz.string
                (Fuzz.listOfLengthBetween 1 32 Fuzz.string)
                Fuzz.string
          )
        , ( 1
          , Fuzz.map4 Get
                (Fuzz.maybe Fuzz.string)
                (Fuzz.list Fuzz.string)
                TestFilter.fuzzer
                Fuzz.string
          )
        ]


fuzzerForBatch : Fuzzer { start : String, events : List String, filter : Filter, end : String }
fuzzerForBatch =
    Fuzz.map4
        (\start events filter end ->
            { start = start, events = events, filter = filter, end = end }
        )
        Fuzz.string
        (Fuzz.list Fuzz.string)
        TestFilter.fuzzer
        Fuzz.string


suite : Test
suite =
    describe "Timeline"
        [ describe "Most recent events"
            [ fuzz fuzzerForBatch "Singleton is most recent"
                (\batch ->
                    { start = Just batch.start
                    , events = batch.events
                    , filter = batch.filter
                    , end = batch.end
                    }
                        |> Timeline.fromSlice
                        |> Timeline.singleton
                        |> Timeline.mostRecentEvents batch.filter
                        |> Expect.equal batch.events
                )
            , fuzz2 fuzzerForBatch fuzzerForBatch "Double batch connects"
                (\batch1 batch2 ->
                    [ { start = Just batch1.start
                      , events = batch1.events
                      , filter = batch1.filter
                      , end = batch2.start
                      }
                    , { start = Just batch2.start
                      , events = batch2.events
                      , filter = batch2.filter
                      , end = batch2.end
                      }
                    ]
                        |> List.map Timeline.fromSlice
                        |> List.foldl Timeline.addSync Timeline.empty
                        |> Timeline.mostRecentEvents (Filter.and batch1.filter batch2.filter)
                        |> (\outcome ->
                                if batch2.start == batch2.end then
                                    Expect.equal [] outcome
                                else if batch1.start == batch2.start then
                                    Expect.equal batch2.events outcome
                                else
                                    Expect.equal
                                        (List.append batch1.events batch2.events)
                                        outcome
                           )
                )
            , fuzz2 fuzzerForBatch fuzzerForBatch "Disconnected double batch does not connect"
                (\batch1 batch2 ->
                    [ { start = Just batch1.start
                      , events = batch1.events
                      , filter = batch1.filter
                      , end = batch1.start
                      }
                    , { start = Just batch2.start
                      , events = batch2.events
                      , filter = batch2.filter
                      , end = batch2.end
                      }
                    ]
                        |> List.map Timeline.fromSlice
                        |> List.foldl Timeline.addSync Timeline.empty
                        |> Timeline.mostRecentEvents (Filter.and batch1.filter batch2.filter)
                        |> (\outcome ->
                                if batch2.start == batch2.end then
                                    Expect.equal [] outcome
                                else if batch1.start == batch2.start then
                                    Expect.equal batch2.events outcome
                                else if batch1.end == batch2.start then
                                    Expect.equal
                                        (List.append batch1.events batch2.events)
                                        outcome
                                else
                                    Expect.equal batch2.events outcome
                           )
                )
            , fuzz
                ( Fuzz.pair Fuzz.int (Fuzz.list Fuzz.string)
                    |> (\f -> Fuzz.triple f f f)
                    |> (\f -> Fuzz.triple f f f)
                )
                "Connect 8 batches"
                (\(((i1, e1), (i2, e2), (i3, e3)), ((i4, e4), (i5, e5), (i6, e6)), ((i7, e7), (i8, e8), (_, e9))) ->
                            [ ( i1
                              , { start = Just <| String.fromInt 1
                                , events = e1
                                , filter = Filter.pass
                                , end = String.fromInt (1 + 1)
                                }
                              )
                            , ( i2
                              , { start = Just <| String.fromInt 2
                                , events = e2
                                , filter = Filter.pass
                                , end = String.fromInt (2 + 1)
                                }
                              )
                            , ( i3
                              , { start = Just <| String.fromInt 3
                                , events = e3
                                , filter = Filter.pass
                                , end = String.fromInt (3 + 1)
                                }
                              )
                            , ( i4
                              , { start = Just <| String.fromInt 4
                                , events = e4
                                , filter = Filter.pass
                                , end = String.fromInt (4 + 1)
                                }
                              )
                            , ( i5
                              , { start = Just <| String.fromInt 5
                                , events = e5
                                , filter = Filter.pass
                                , end = String.fromInt (5 + 1)
                                }
                              )
                            , ( i6
                              , { start = Just <| String.fromInt 6
                                , events = e6
                                , filter = Filter.pass
                                , end = String.fromInt (6 + 1)
                                }
                              )
                            , ( i7
                              , { start = Just <| String.fromInt 7
                                , events = e7
                                , filter = Filter.pass
                                , end = String.fromInt (7 + 1)
                                }
                              )
                            , ( i8
                              , { start = Just <| String.fromInt 8
                                , events = e8
                                , filter = Filter.pass
                                , end = String.fromInt (8 + 1)
                                }
                              )
                            ]
                            |> List.sortBy Tuple.first
                            |> List.map Tuple.second
                            |> List.map Timeline.fromSlice
                            |> List.foldl
                                Timeline.insert
                                (Timeline.singleton
                                    ( Timeline.fromSlice
                                        { start = Just <| String.fromInt 9
                                        , events = e9
                                        , filter = Filter.pass
                                        , end = String.fromInt (9 + 1)
                                        }
                                    )
                                )
                            |> Timeline.mostRecentEvents Filter.pass
                            |> Expect.equal
                                ( e1 ++ e2 ++ e3 ++ e4 ++ e5 ++ e6 ++ e7 ++ e8 ++ e9 )
                        )
            ]
        ]
