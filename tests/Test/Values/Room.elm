module Test.Values.Room exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Internal.Values.Room as Room exposing (Room)
import Json.Decode as D
import Json.Encode as E
import Test exposing (..)
import Test.Filter.Timeline as TestFilter
import Test.Values.Event as TestEvent


placeholderValue : E.Value
placeholderValue =
    E.string "foo bar baz"


fuzzer : Fuzzer Room
fuzzer =
    Fuzz.string
        |> Fuzz.map Room.init
        |> addAFewTimes Fuzz.string (\key -> Room.setAccountData key placeholderValue)
        |> addAFewTimes (Fuzz.list TestEvent.fuzzer) Room.addEvents
        |> add4AFewTimes (Fuzz.list TestEvent.fuzzer)
            TestFilter.fuzzer
            (Fuzz.maybe Fuzz.string)
            Fuzz.string
            (\a b c d ->
                Room.Batch a b c d
                    |> Room.addBatch
            )
        |> add4AFewTimes (Fuzz.list TestEvent.fuzzer)
            TestFilter.fuzzer
            (Fuzz.maybe Fuzz.string)
            Fuzz.string
            (\a b c d ->
                Room.Batch a b c d
                    |> Room.addSync
            )


addAFewTimes : Fuzzer a -> (a -> Room -> Room) -> Fuzzer Room -> Fuzzer Room
addAFewTimes fuzz f roomFuzzer =
    Fuzz.map2
        (\items room -> List.foldl f room items)
        (Fuzz.list fuzz)
        roomFuzzer


add2AFewTimes : Fuzzer a -> Fuzzer b -> (a -> b -> Room -> Room) -> Fuzzer Room -> Fuzzer Room
add2AFewTimes fuzz1 fuzz2 f roomFuzzer =
    Fuzz.map2
        (\items room -> List.foldl (\( a, b ) -> f a b) room items)
        (Fuzz.list <| Fuzz.pair fuzz1 fuzz2)
        roomFuzzer


add3AFewTimes : Fuzzer a -> Fuzzer b -> Fuzzer c -> (a -> b -> c -> Room -> Room) -> Fuzzer Room -> Fuzzer Room
add3AFewTimes fuzz1 fuzz2 fuzz3 f roomFuzzer =
    Fuzz.map2
        (\items room -> List.foldl (\( a, b, c ) -> f a b c) room items)
        (Fuzz.list <| Fuzz.triple fuzz1 fuzz2 fuzz3)
        roomFuzzer


add4AFewTimes : Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d -> (a -> b -> c -> d -> Room -> Room) -> Fuzzer Room -> Fuzzer Room
add4AFewTimes fuzz1 fuzz2 fuzz3 fuzz4 f roomFuzzer =
    Fuzz.map2
        (\items room -> List.foldl (\( ( a, b ), ( c, d ) ) -> f a b c d) room items)
        (Fuzz.list <| Fuzz.pair (Fuzz.pair fuzz1 fuzz2) (Fuzz.pair fuzz3 fuzz4))
        roomFuzzer


suite : Test
suite =
    describe "Room"
        [ fuzz3 fuzzer
            Fuzz.string
            Fuzz.string
            "JSON Account Data can be overridden"
            (\room key text ->
                room
                    |> Room.setAccountData key (E.string text)
                    |> Room.getAccountData key
                    |> Maybe.map (D.decodeValue D.string)
                    |> Maybe.andThen Result.toMaybe
                    |> Expect.equal (Just text)
            )
        , fuzz fuzzer
            "Room -> JSON -> Room is equal"
            (\room ->
                let
                    value : E.Value
                    value =
                        Room.encode room
                in
                value
                    |> D.decodeValue Room.decode
                    |> Result.toMaybe
                    |> Maybe.map Tuple.first
                    |> Maybe.map Room.encode
                    |> Maybe.map (E.encode 0)
                    |> Expect.equal (Just <| E.encode 0 value)
            )
        ]
