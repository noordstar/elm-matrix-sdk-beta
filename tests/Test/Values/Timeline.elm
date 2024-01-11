module Test.Values.Timeline exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Internal.Filter.Timeline as Filter exposing (Filter)
import Internal.Values.Timeline as Timeline exposing (Batch, Timeline)
import Json.Decode as D
import Json.Encode as E
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
                                    , f >> Timeline.insert { b | start = Just s, filter = Filter.and globalFilter b.filter }
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


isEqual : Timeline -> Timeline -> Expect.Expectation
isEqual t1 t2 =
    Expect.equal
        (E.encode 0 <| Timeline.encode t1)
        (E.encode 0 <| Timeline.encode t2)


suite : Test
suite =
    describe "Timeline"
        [ describe "empty"
            [ fuzz fuzzerBatch
                "singleton = empty + sync"
                (\batch ->
                    isEqual
                        (Timeline.singleton batch)
                        (Timeline.addSync batch Timeline.empty)
                )
            ]
        , describe "JSON"
            [ fuzz fuzzer
                "encode -> decode is same"
                (\timeline ->
                    timeline
                        |> Timeline.encode
                        |> E.encode 0
                        |> D.decodeString Timeline.decoder
                        |> (\t ->
                                case t of
                                    Ok v ->
                                        isEqual v timeline

                                    Err e ->
                                        Expect.fail (D.errorToString e)
                           )
                )
            ]
        ]
