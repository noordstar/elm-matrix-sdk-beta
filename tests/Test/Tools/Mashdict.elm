module Test.Tools.Mashdict exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Internal.Tools.Mashdict as Mashdict exposing (Mashdict)
import Internal.Values.Event as Event
import Json.Decode as D
import Json.Encode as E
import Test exposing (..)
import Test.Values.Event as TestEvent


fuzzer : (a -> Maybe String) -> Fuzzer a -> Fuzzer (Mashdict a)
fuzzer toHash fuz =
    Fuzz.map (Mashdict.fromList toHash) (Fuzz.list fuz)


eventFuzzer : Fuzzer (Mashdict Event.Event)
eventFuzzer =
    fuzzer .stateKey TestEvent.fuzzer


suite : Test
suite =
    describe "Mashdict"
        [ describe "empty"
            [ test "empty isEmpty"
                (Mashdict.empty identity
                    |> Mashdict.isEmpty
                    |> Expect.equal True
                    |> always
                )
            , fuzz TestEvent.fuzzer
                "Nothing is member"
                (\event ->
                    Mashdict.empty .stateKey
                        |> Mashdict.member event
                        |> Expect.equal False
                )
            , fuzz Fuzz.string
                "No key is member"
                (\key ->
                    Mashdict.empty identity
                        |> Mashdict.memberKey key
                        |> Expect.equal False
                )
            , fuzz Fuzz.string
                "Get gets Nothing"
                (\key ->
                    Mashdict.empty identity
                        |> Mashdict.get key
                        |> Expect.equal Nothing
                )
            , test "Size is zero"
                (Mashdict.empty identity
                    |> Mashdict.size
                    |> Expect.equal 0
                    |> always
                )
            , test "No keys"
                (Mashdict.empty identity
                    |> Mashdict.keys
                    |> Expect.equal []
                    |> always
                )
            , test "No values"
                (Mashdict.empty identity
                    |> Mashdict.values
                    |> Expect.equal []
                    |> always
                )
            , test "To list is []"
                (Mashdict.empty identity
                    |> Mashdict.toList
                    |> Expect.equal []
                    |> always
                )
            , test "From list is empty"
                ([]
                    |> Mashdict.fromList (\x -> x)
                    |> Mashdict.isEqual (Mashdict.empty identity)
                    |> Expect.equal True
                    |> always
                )
            , test "Empty + empty == empty"
                (Mashdict.empty Maybe.Just
                    |> Mashdict.union (Mashdict.empty Maybe.Just)
                    |> Mashdict.isEqual (Mashdict.empty Maybe.Just)
                    |> Expect.equal True
                    |> always
                )
            , fuzz (Fuzz.intRange 0 10)
                "JSON encode -> JSON decode"
                (\indent ->
                    Mashdict.empty Just
                        |> Mashdict.encode E.string
                        |> E.encode indent
                        |> D.decodeString (Mashdict.decoder Just D.string)
                        |> Result.map (Mashdict.isEqual (Mashdict.empty Just))
                        |> Expect.equal (Ok True)
                )
            ]
        , describe "singleton"
            [ fuzz TestEvent.fuzzer
                "singleton = empty + insert"
                (\event ->
                    Mashdict.empty .stateKey
                        |> Mashdict.insert event
                        |> Mashdict.isEqual (Mashdict.singleton .stateKey event)
                        |> Expect.equal True
                )
            , fuzz TestEvent.fuzzer
                "singleton - event = empty"
                (\event ->
                    Mashdict.singleton .stateKey event
                        |> Mashdict.remove event
                        |> Mashdict.isEqual (Mashdict.empty (always Nothing))
                        |> Expect.equal True
                )
            , fuzz TestEvent.fuzzer
                "singleton - event (key) = empty"
                (\event ->
                    case event.stateKey of
                        Just key ->
                            Mashdict.singleton .stateKey event
                                |> Mashdict.removeKey key
                                |> Mashdict.isEqual (Mashdict.empty .stateKey)
                                |> Expect.equal True

                        Nothing ->
                            Expect.pass
                )
            , fuzz TestEvent.fuzzer
                "Only isEmpty when not Nothing"
                (\event ->
                    Expect.equal
                        (case event.stateKey of
                            Just _ ->
                                False

                            Nothing ->
                                True
                        )
                        (event
                            |> Mashdict.singleton .stateKey
                            |> Mashdict.isEmpty
                        )
                )
            , fuzz TestEvent.fuzzer
                "member"
                (\event ->
                    Expect.equal
                        (case event.stateKey of
                            Just _ ->
                                True

                            Nothing ->
                                False
                        )
                        (Mashdict.singleton .stateKey event
                            |> Mashdict.member event
                        )
                )
            , fuzz2 TestEvent.fuzzer
                Fuzz.string
                "memberKey"
                (\event rkey ->
                    case event.stateKey of
                        Just key ->
                            Mashdict.singleton .stateKey event
                                |> Mashdict.memberKey key
                                |> Expect.equal True

                        Nothing ->
                            Mashdict.singleton .stateKey event
                                |> Mashdict.memberKey rkey
                                |> Expect.equal False
                )
            , fuzz TestEvent.fuzzer
                "False memberKey"
                (\event ->
                    if event.stateKey == Just event.roomId then
                        Expect.pass

                    else
                        Mashdict.singleton .stateKey event
                            |> Mashdict.memberKey event.roomId
                            |> Expect.equal False
                )
            ]
        , describe "JSON"
            [ fuzz2 eventFuzzer
                (Fuzz.intRange 0 10)
                "JSON encode -> JSON decode"
                (\hashdict indent ->
                    hashdict
                        |> Mashdict.encode Event.encode
                        |> E.encode indent
                        |> D.decodeString (Mashdict.decoder .stateKey Event.decoder)
                        |> Result.map Mashdict.toList
                        |> Expect.equal (Ok <| Mashdict.toList hashdict)
                )
            ]
        ]
