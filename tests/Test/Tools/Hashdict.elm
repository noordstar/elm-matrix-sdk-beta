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

suite : Test
suite =
    describe "Hashdict"
        [ describe "init"
            [ test "init isEmpty"
                ( Hashdict.empty identity
                    |> Hashdict.isEmpty
                    |> Expect.equal True
                    |> always
                )
            ]
        , describe "JSON"
            [ fuzz2 (fuzzer .eventId TestEvent.fuzzer) (Fuzz.intRange 0 10) "JSON encode -> JSON decode"
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
