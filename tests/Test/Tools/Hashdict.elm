module Test.Tools.Hashdict exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Internal.Tools.Hashdict as Hashdict exposing (Hashdict)
import Internal.Tools.Json as Json
import Internal.Values.Event as Event
import Json.Decode as D
import Json.Encode as E
import Test exposing (..)
import Test.Values.Event as TestEvent


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
                (Hashdict.empty identity
                    |> Hashdict.isEmpty
                    |> Expect.equal True
                    |> always
                )
            , fuzz TestEvent.fuzzer
                "Nothing is member"
                (\event ->
                    Hashdict.empty .eventId
                        |> Hashdict.member event
                        |> Expect.equal False
                )
            , fuzz Fuzz.string
                "No key is member"
                (\key ->
                    Hashdict.empty identity
                        |> Hashdict.memberKey key
                        |> Expect.equal False
                )
            , fuzz Fuzz.string
                "Get gets Nothing"
                (\key ->
                    Hashdict.empty identity
                        |> Hashdict.get key
                        |> Expect.equal Nothing
                )
            , test "Size is zero"
                (Hashdict.empty identity
                    |> Hashdict.size
                    |> Expect.equal 0
                    |> always
                )
            , test "No keys"
                (Hashdict.empty identity
                    |> Hashdict.keys
                    |> Expect.equal []
                    |> always
                )
            , test "No values"
                (Hashdict.empty identity
                    |> Hashdict.values
                    |> Expect.equal []
                    |> always
                )
            , test "To list is []"
                (Hashdict.empty identity
                    |> Hashdict.toList
                    |> Expect.equal []
                    |> always
                )
            , test "From list is empty"
                ([]
                    |> Hashdict.fromList (\x -> x)
                    |> Hashdict.isEqual (Hashdict.empty identity)
                    |> Expect.equal True
                    |> always
                )
            , test "Empty + empty == empty"
                (Hashdict.empty identity
                    |> Hashdict.union (Hashdict.empty String.toUpper)
                    |> Hashdict.isEqual (Hashdict.empty String.toLower)
                    |> Expect.equal True
                    |> always
                )
            , fuzz (Fuzz.intRange 0 10)
                "JSON encode -> JSON decode"
                (\indent ->
                    Hashdict.empty identity
                        |> Json.encode (Hashdict.coder identity Json.string)
                        |> E.encode indent
                        |> D.decodeString (Json.decode <| Hashdict.coder identity Json.string)
                        |> Result.map (Tuple.mapFirst (Hashdict.isEqual (Hashdict.empty String.toUpper)))
                        |> Expect.equal (Ok ( True, [] ))
                )
            ]
        , describe "singleton"
            [ fuzz TestEvent.fuzzer
                "singleton = empty + insert"
                (\event ->
                    Hashdict.empty .eventId
                        |> Hashdict.insert event
                        |> Hashdict.isEqual (Hashdict.singleton .eventId event)
                        |> Expect.equal True
                )
            , fuzz TestEvent.fuzzer
                "Singleton - event = empty"
                (\event ->
                    Hashdict.singleton .eventId event
                        |> Hashdict.remove event
                        |> Hashdict.isEqual (Hashdict.empty .roomId)
                        |> Expect.equal True
                )
            , fuzz TestEvent.fuzzer
                "Singletong - event (key) = empty"
                (\event ->
                    Hashdict.singleton .eventId event
                        |> Hashdict.removeKey event.eventId
                        |> Hashdict.isEqual (Hashdict.empty .roomId)
                        |> Expect.equal True
                )
            , fuzz TestEvent.fuzzer
                "not isEmpty"
                (\event ->
                    Hashdict.singleton .eventId event
                        |> Hashdict.isEmpty
                        |> Expect.equal False
                )
            , fuzz TestEvent.fuzzer
                "member"
                (\event ->
                    Hashdict.singleton .eventId event
                        |> Hashdict.member event
                        |> Expect.equal True
                )
            , fuzz TestEvent.fuzzer
                "memberKey"
                (\event ->
                    Hashdict.singleton .eventId event
                        |> Hashdict.memberKey event.eventId
                        |> Expect.equal True
                )
            , fuzz TestEvent.fuzzer
                "False memberKey"
                (\event ->
                    if event.eventId == event.roomId then
                        Expect.pass

                    else
                        Hashdict.singleton .eventId event
                            |> Hashdict.memberKey event.roomId
                            |> Expect.equal False
                )
            ]
        , describe "update"
            [ fuzz2 (fuzzer identity Fuzz.string)
                Fuzz.string
                "add = insert"
                (\hashdict value ->
                    Hashdict.isEqual
                        (Hashdict.insert value hashdict)
                        (Hashdict.update value (always (Just value)) hashdict)
                        |> Expect.equal True
                )
            , fuzz2 (fuzzer identity Fuzz.string)
                Fuzz.string
                "remove = removeKey"
                (\hashdict value ->
                    Hashdict.isEqual
                        (Hashdict.removeKey value hashdict)
                        (Hashdict.update value (always Nothing) hashdict)
                        |> Expect.equal True
                )
            ]
        , describe "JSON"
            [ fuzz2 eventFuzzer
                (Fuzz.intRange 0 10)
                "JSON encode -> JSON decode"
                (\hashdict indent ->
                    hashdict
                        |> Json.encode (Hashdict.coder .eventId Event.coder)
                        |> E.encode indent
                        |> D.decodeString (Json.decode <| Hashdict.coder .eventId Event.coder)
                        |> Result.map (Tuple.first >> Hashdict.toList)
                        |> Expect.equal (Ok (Hashdict.toList hashdict))
                )
            ]
        ]
