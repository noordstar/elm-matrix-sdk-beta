module Test.Tools.Hashdict exposing (..)

import Test exposing (..)
import Fuzz exposing (Fuzzer)
import Internal.Tools.Hashdict as Hashdict exposing (Hashdict)
import Test.Values.Event as TestEvent
import Internal.Values.Event as Event
import Json.Encode as E
import Json.Decode as D
import Expect

fuzzer : (a -> String) -> Fuzzer a -> Fuzzer (Hashdict a)
fuzzer toHash fuz =
    Fuzz.map (Hashdict.fromList toHash) (Fuzz.list fuz)

eventFuzzer : Fuzzer (Hashdict Event.Event)
eventFuzzer =
    fuzzer .eventId TestEvent.fuzzer

suite : Test
suite =
    describe "Hashdict"
        [ describe "empty"
            [ test "empty isEmpty"
                ( Hashdict.empty identity
                    |> Hashdict.isEmpty
                    |> Expect.equal True
                    |> always
                )
            , fuzz TestEvent.fuzzer "Nothing is member"
                (\event ->
                    Hashdict.empty .eventId
                        |> Hashdict.member event
                        |> Expect.equal False
                )
            , fuzz Fuzz.string "No key is member"
                (\key ->
                    Hashdict.empty identity
                        |> Hashdict.memberKey key
                        |> Expect.equal False
                )
            , fuzz Fuzz.string "Get gets Nothing"
                (\key ->
                    Hashdict.empty identity
                        |> Hashdict.get key
                        |> Expect.equal Nothing
                )
            , test "Size is zero"
                ( Hashdict.empty identity
                    |> Hashdict.size
                    |> Expect.equal 0
                    |> always
                )
            , test "No keys"
                ( Hashdict.empty identity
                    |> Hashdict.keys
                    |> Expect.equal []
                    |> always
                )
            , test "No values"
                ( Hashdict.empty identity
                    |> Hashdict.values
                    |> Expect.equal []
                    |> always
                )
            , test "To list is []"
                ( Hashdict.empty identity
                    |> Hashdict.toList
                    |> Expect.equal []
                    |> always
                )
            , test "From list is empty"
                ( []
                    |> Hashdict.fromList (\x -> x)
                    |> Hashdict.isEqual (Hashdict.empty identity)
                    |> Expect.equal True
                    |> always
                )
            , test "Empty + empty == empty"
                ( Hashdict.empty identity
                    |> Hashdict.union (Hashdict.empty String.toUpper)
                    |> Hashdict.isEqual (Hashdict.empty String.toLower)
                    |> Expect.equal True
                    |> always
                )
            , fuzz (Fuzz.intRange 0 10) "JSON encode -> JSON decode"
                (\indent ->
                    Hashdict.empty identity
                        |> Hashdict.encode E.string
                        |> E.encode indent
                        |> D.decodeString (Hashdict.decoder identity D.string)
                        |> Result.map (Hashdict.isEqual (Hashdict.empty String.toUpper))
                        |> Expect.equal (Ok True)
                )
            ]
        , describe "JSON"
            [ fuzz2 eventFuzzer (Fuzz.intRange 0 10) "JSON encode -> JSON decode"
                (\hashdict indent ->
                    hashdict
                        |> Hashdict.encode Event.encode
                        |> E.encode indent
                        |> D.decodeString (Hashdict.decoder .eventId Event.decoder)
                        |> Result.map Hashdict.toList
                        |> Expect.equal ( Ok <| Hashdict.toList hashdict )
                )
            ]
        ]
