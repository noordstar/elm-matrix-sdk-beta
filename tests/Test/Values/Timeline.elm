module Test.Values.Timeline exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Internal.Filter.Timeline as Filter exposing (Filter)
import Internal.Tools.Json as Json
import Internal.Values.Timeline as Timeline exposing (Batch, Timeline)
import Json.Decode as D
import Test exposing (..)
import Test.Filter.Timeline as TestFilter


fuzzer : Fuzzer Timeline
fuzzer =
    TestFilter.fuzzer
        |> Fuzz.andThen
            (\globalFilter ->
                Fuzz.oneOf
                    [ Fuzz.map2
                        (\start batches ->
                            List.foldl
                                (\b ( s, f ) ->
                                    ( b.end
                                    , f >> Timeline.insert { b | start = Just s, filter = globalFilter }
                                    )
                                )
                                ( start, identity )
                                batches
                                |> Tuple.second
                        )
                        Fuzz.string
                        (Fuzz.listOfLengthBetween 0 10 fuzzerBatch)
                    , Fuzz.map2
                        (\start batches ->
                            List.foldl
                                (\b ( s, f ) ->
                                    ( b.end
                                    , f >> Timeline.insert { b | start = Just s, filter = Filter.and globalFilter b.filter }
                                    )
                                )
                                ( start, identity )
                                batches
                                |> Tuple.second
                        )
                        Fuzz.string
                        (Fuzz.listOfLengthBetween 0 4 fuzzerBatch)
                    , Fuzz.map2
                        (\start batches ->
                            List.foldl
                                (\b ( s, f ) ->
                                    ( b.end
                                    , f >> Timeline.addSync { b | start = Just s, filter = globalFilter }
                                    )
                                )
                                ( start, identity )
                                batches
                                |> Tuple.second
                        )
                        Fuzz.string
                        (Fuzz.listOfLengthBetween 0 10 fuzzerBatch)
                    , Fuzz.map2
                        (\start batches ->
                            List.foldl
                                (\b ( s, f ) ->
                                    ( b.end
                                    , f >> Timeline.addSync { b | start = Just s, filter = Filter.and globalFilter b.filter }
                                    )
                                )
                                ( start, identity )
                                batches
                                |> Tuple.second
                        )
                        Fuzz.string
                        (Fuzz.listOfLengthBetween 0 4 fuzzerBatch)
                    ]
                    |> Fuzz.listOfLengthBetween 0 10
                    |> Fuzz.map (List.foldl (<|) Timeline.empty)
            )


fuzzerBatch : Fuzzer Batch
fuzzerBatch =
    Fuzz.map4 Batch
        (Fuzz.list Fuzz.string)
        TestFilter.fuzzer
        (Fuzz.maybe Fuzz.string)
        Fuzz.string


suite : Test
suite =
    describe "Timeline"
        [ describe "most recent events with filters"
            [ fuzz TestFilter.fuzzer
                "Events are returned properly"
                (\filter ->
                    Timeline.empty
                        |> Timeline.insert
                            { events = [ "a", "b", "c" ]
                            , filter = filter
                            , start = Just "token_1"
                            , end = "token_2"
                            }
                        |> Timeline.insert
                            { events = [ "d", "e", "f" ]
                            , filter = filter
                            , start = Just "token_2"
                            , end = "token_3"
                            }
                        |> Timeline.mostRecentEventsFrom filter "token_3"
                        |> Expect.equal
                            [ [ "a", "b", "c", "d", "e", "f" ] ]
                )
            , fuzz2 TestFilter.fuzzer
                TestFilter.fuzzer
                "Sub-events get the same results"
                (\f1 f2 ->
                    let
                        subFilter =
                            Filter.and f1 f2
                    in
                    Timeline.empty
                        |> Timeline.insert
                            { events = [ "a", "b", "c" ]
                            , filter = f1
                            , start = Just "token_1"
                            , end = "token_2"
                            }
                        |> Timeline.insert
                            { events = [ "d", "e", "f" ]
                            , filter = f1
                            , start = Just "token_2"
                            , end = "token_3"
                            }
                        |> Timeline.mostRecentEventsFrom subFilter "token_3"
                        |> Expect.equal
                            [ [ "a", "b", "c", "d", "e", "f" ] ]
                )
            , fuzz2 TestFilter.fuzzer
                TestFilter.fuzzer
                "ONLY same result if sub-filter"
                (\f1 f2 ->
                    Timeline.empty
                        |> Timeline.insert
                            { events = [ "a", "b", "c" ]
                            , filter = f1
                            , start = Just "token_1"
                            , end = "token_2"
                            }
                        |> Timeline.insert
                            { events = [ "d", "e", "f" ]
                            , filter = f1
                            , start = Just "token_2"
                            , end = "token_3"
                            }
                        |> Timeline.mostRecentEventsFrom f2 "token_3"
                        |> (\events ->
                                Expect.equal
                                    (Filter.subsetOf f1 f2)
                                    (events == [ [ "a", "b", "c", "d", "e", "f" ] ])
                           )
                )
            ]
        , describe "Forks in the road"
            [ fuzz2 TestFilter.fuzzer
                TestFilter.fuzzer
                "Two options returned"
                (\f1 f2 ->
                    let
                        subFilter =
                            Filter.and f1 f2
                    in
                    Timeline.empty
                        |> Timeline.insert
                            { events = [ "a", "b", "c" ]
                            , filter = f1
                            , start = Just "token_1"
                            , end = "token_2"
                            }
                        |> Timeline.insert
                            { events = [ "d", "e", "f" ]
                            , filter = f2
                            , start = Just "token_3"
                            , end = "token_2"
                            }
                        |> Timeline.insert
                            { events = [ "g", "h", "i" ]
                            , filter = subFilter
                            , start = Just "token_2"
                            , end = "token_4"
                            }
                        |> Timeline.mostRecentEventsFrom subFilter "token_4"
                        |> Expect.equal
                            [ [ "a", "b", "c", "g", "h", "i" ]
                            , [ "d", "e", "f", "g", "h", "i" ]
                            ]
                )
            ]
        , describe "Gaps"
            [ fuzz TestFilter.fuzzer
                "Gaps leave behind old events"
                (\filter ->
                    Timeline.empty
                        |> Timeline.insert
                            { events = [ "a", "b", "c" ]
                            , filter = filter
                            , start = Just "token_1"
                            , end = "token_2"
                            }
                        |> Timeline.insert
                            { events = [ "d", "e", "f" ]
                            , filter = filter
                            , start = Just "token_3"
                            , end = "token_4"
                            }
                        |> Timeline.mostRecentEventsFrom filter "token_4"
                        |> Expect.equal [ [ "d", "e", "f" ] ]
                )
            , fuzz3 TestFilter.fuzzer
                (Fuzz.list Fuzz.string)
                (Fuzz.pair (Fuzz.list Fuzz.string) (Fuzz.list Fuzz.string))
                "Gaps can be bridged"
                (\filter l1 ( l2, l3 ) ->
                    Timeline.empty
                        |> Timeline.insert
                            { events = l1
                            , filter = filter
                            , start = Just "token_1"
                            , end = "token_2"
                            }
                        |> Timeline.insert
                            { events = l3
                            , filter = filter
                            , start = Just "token_3"
                            , end = "token_4"
                            }
                        |> Timeline.insert
                            { events = l2
                            , filter = filter
                            , start = Just "token_2"
                            , end = "token_3"
                            }
                        |> Timeline.mostRecentEventsFrom filter "token_4"
                        |> Expect.equal [ List.concat [ l1, l2, l3 ] ]
                )
            ]
        , describe "JSON"
            [ fuzz fuzzer
                "Encode + Decode gives same output"
                (\timeline ->
                    timeline
                        |> Json.encode Timeline.coder
                        |> D.decodeValue (Json.decode Timeline.coder)
                        |> Result.map Tuple.first
                        |> Result.map (Timeline.mostRecentEvents Filter.pass)
                        |> Expect.equal (Ok <| Timeline.mostRecentEvents Filter.pass timeline)
                )
            ]
        , describe "Weird loops"
            [ fuzz TestFilter.fuzzer
                "Weird loops stop looping"
                (\filter ->
                    Timeline.empty
                        |> Timeline.insert
                            { events = [ "a", "b", "c" ]
                            , filter = filter
                            , start = Just "token_1"
                            , end = "token_2"
                            }
                        |> Timeline.insert
                            { events = [ "d", "e", "f" ]
                            , filter = filter
                            , start = Just "token_2"
                            , end = "token_3"
                            }
                        |> Timeline.insert
                            { events = [ "g", "h", "i" ]
                            , filter = filter
                            , start = Just "token_3"
                            , end = "token_2"
                            }
                        |> Timeline.mostRecentEventsFrom filter "token_2"
                        |> Expect.equal
                            [ [ "a", "b", "c" ]
                            , [ "d", "e", "f", "g", "h", "i" ]
                            ]
                )
            ]
        , describe "Sync"
            [ fuzz TestFilter.fuzzer
                "Sync fills gaps"
                (\filter ->
                    Timeline.empty
                        |> Timeline.addSync
                            { events = [ "a", "b", "c" ]
                            , filter = filter
                            , start = Just "token_1"
                            , end = "token_2"
                            }
                        |> Timeline.addSync
                            { events = [ "f", "g", "h" ]
                            , filter = filter
                            , start = Just "token_3"
                            , end = "token_4"
                            }
                        |> Timeline.insert
                            { events = [ "d", "e" ]
                            , filter = filter
                            , start = Just "token_2"
                            , end = "token_3"
                            }
                        |> Timeline.mostRecentEvents filter
                        |> Expect.equal [ [ "a", "b", "c", "d", "e", "f", "g", "h" ] ]
                )
            , fuzz TestFilter.fuzzer
                "Sync doesn't fill open gaps"
                (\filter ->
                    Timeline.empty
                        |> Timeline.addSync
                            { events = [ "a", "b", "c" ]
                            , filter = filter
                            , start = Just "token_1"
                            , end = "token_2"
                            }
                        |> Timeline.addSync
                            { events = [ "f", "g", "h" ]
                            , filter = filter
                            , start = Just "token_3"
                            , end = "token_4"
                            }
                        |> Timeline.mostRecentEvents filter
                        |> Expect.equal [ [ "f", "g", "h" ] ]
                )
            , fuzz3 (Fuzz.pair Fuzz.string Fuzz.string)
                fuzzer
                TestFilter.fuzzer
                "Getting /sync is the same as getting from the token"
                (\( start, end ) timeline filter ->
                    let
                        t : Timeline
                        t =
                            Timeline.addSync
                                { events = [ "a", "b", "c" ]
                                , filter = filter
                                , start = Just start
                                , end = end
                                }
                                timeline
                    in
                    Expect.equal
                        (Timeline.mostRecentEvents filter t)
                        (Timeline.mostRecentEventsFrom filter end t)
                )
            , fuzz TestFilter.fuzzer
                "Weird loops stop looping"
                (\filter ->
                    Timeline.empty
                        |> Timeline.insert
                            { events = [ "a", "b", "c" ]
                            , filter = filter
                            , start = Just "token_1"
                            , end = "token_2"
                            }
                        |> Timeline.insert
                            { events = [ "d", "e", "f" ]
                            , filter = filter
                            , start = Just "token_2"
                            , end = "token_3"
                            }
                        |> Timeline.insert
                            { events = [ "g", "h", "i" ]
                            , filter = filter
                            , start = Just "token_3"
                            , end = "token_2"
                            }
                        |> Timeline.mostRecentEventsFrom filter "token_2"
                        |> Expect.equal
                            [ [ "a", "b", "c" ]
                            , [ "d", "e", "f", "g", "h", "i" ]
                            ]
                )
            ]
        ]
