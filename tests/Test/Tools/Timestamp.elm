module Test.Tools.Timestamp exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Internal.Tools.Timestamp as Timestamp exposing (Timestamp)
import Json.Decode as D
import Json.Encode as E
import Test exposing (..)
import Time


fuzzer : Fuzzer Timestamp
fuzzer =
    Fuzz.map Time.millisToPosix Fuzz.int


suite : Test
suite =
    describe "Timestamp"
        [ describe "JSON"
            [ fuzz2 fuzzer
                Fuzz.int
                "JSON encode -> JSON decode"
                (\time indent ->
                    time
                        |> Timestamp.encode
                        |> E.encode indent
                        |> D.decodeString Timestamp.decoder
                        |> Expect.equal (Ok time)
                )
            , fuzz fuzzer
                "JSON decode -> millis"
                (\time ->
                    time
                        |> Timestamp.encode
                        |> D.decodeValue D.int
                        |> Expect.equal (Ok <| Time.posixToMillis time)
                )
            , fuzz Fuzz.int
                "JSON decode -> time"
                (\n ->
                    n
                        |> E.int
                        |> D.decodeValue Timestamp.decoder
                        |> Expect.equal (Ok <| Time.millisToPosix n)
                )
            ]
        , describe "Identity"
            [ fuzz fuzzer
                "Posix -> int -> Posix"
                (\time ->
                    time
                        |> Time.posixToMillis
                        |> Time.millisToPosix
                        |> Expect.equal time
                )
            , fuzz Fuzz.int
                "int -> Posix -> int"
                (\n ->
                    n
                        |> Time.millisToPosix
                        |> Time.posixToMillis
                        |> Expect.equal n
                )
            ]
        ]
