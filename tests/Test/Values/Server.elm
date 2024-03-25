module Test.Values.Server exposing (..)

import Test exposing (..)
import Fuzz exposing (Fuzzer)
import Expect

digits : String
digits = "0123456789"

alpha : String
alpha = "abcdefghijklmnopqrstuvwxyz"

hex : String
hex = "0123456789abcdef"

dns : String
dns =
    digits ++ alpha ++ (String.toUpper alpha) ++ "-."

dnsCharFuzzer : Fuzzer Char
dnsCharFuzzer =
    dns
        |> String.toList
        |> Fuzz.oneOfValues

dnsNameFuzzer : Fuzzer String
dnsNameFuzzer =
    dnsCharFuzzer
        |> Fuzz.listOfLengthBetween 1 255
        |> Fuzz.map String.fromList

byteNumFuzzer : Fuzzer String
byteNumFuzzer =
    Fuzz.intRange 0 255
        |> Fuzz.map String.fromInt

portFuzzer : Fuzzer String
portFuzzer =
    Fuzz.intRange 0 (2^16 - 1)
        |> Fuzz.map String.fromInt

ipv4Fuzzer : Fuzzer String
ipv4Fuzzer =
    Fuzz.map4
        (\a b c d ->
            [ a, b, c, d ]
                |> List.intersperse "."
                |> String.concat
        )
        byteNumFuzzer
        byteNumFuzzer
        byteNumFuzzer
        byteNumFuzzer

ipv6CharFuzzer : Fuzzer Char
ipv6CharFuzzer =
    hex
        |> String.toList
        |> Fuzz.oneOfValues

ipv6PartFuzzer : Fuzzer String
ipv6PartFuzzer =
    ipv6CharFuzzer
        |> Fuzz.listOfLengthBetween 1 4
        |> Fuzz.map String.fromList

ipv6Sides : Fuzzer (Int, Int)
ipv6Sides =
    Fuzz.intRange 0 7
        |> Fuzz.andThen
            (\a ->
                Fuzz.intRange 0 (7-a)
                    |> Fuzz.map (\b -> (a, b))
            )

ipv6Fuzzer : Fuzzer String
ipv6Fuzzer =
    Fuzz.oneOf
        [ ipv6PartFuzzer
            |> Fuzz.listOfLength 8
            |> Fuzz.map (List.intersperse ":")
            |> Fuzz.map String.concat
        , ipv6Sides
            |> Fuzz.andThen
                (\(a, b) ->
                    Fuzz.pair
                        (Fuzz.listOfLength a ipv6PartFuzzer)
                        (Fuzz.listOfLength b ipv6PartFuzzer)
                )
            |> Fuzz.map
                (\(la, lb) ->
                    [ List.intersperse ":" la
                    , [ "::" ]
                    , List.intersperse ":" lb
                    ]
                        |> List.concat
                        |> String.concat
                )
        ]

hostnameFuzzer : Fuzzer String
hostnameFuzzer =
    Fuzz.oneOf
        [ ipv4Fuzzer
        , Fuzz.map (\ip -> "[" ++ ip ++ "]") ipv6Fuzzer
        , dnsNameFuzzer
        ]

serverNameFuzzer : Fuzzer String
serverNameFuzzer =
    Fuzz.map2 (++)
        hostnameFuzzer
        ( Fuzz.oneOf
            [ Fuzz.constant ""
            , Fuzz.map (\p -> ":" ++ p) portFuzzer
            ]
        )
        |> Fuzz.map (Debug.log "Server")

suite : Test
suite =
    describe "Server name tester"
        [ fuzz serverNameFuzzer "IPv6 test"
            (\_ ->
                Expect.pass
            )
        ]