module Test.Grammar.ServerName exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Internal.Grammar.ServerName as SN
import Test exposing (..)


dnsFuzzer : Fuzzer String
dnsFuzzer =
    Fuzz.map2
        (\head tail ->
            String.fromList (head :: tail)
        )
        ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
            |> String.toList
            |> Fuzz.oneOfValues
        )
        ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-."
            |> String.toList
            |> Fuzz.oneOfValues
            |> Fuzz.listOfLengthBetween 0 (255 - 1)
        )


hostnameFuzzer : Fuzzer String
hostnameFuzzer =
    Fuzz.oneOf
        [ dnsFuzzer
        , ipv4Fuzzer
        , Fuzz.map (\x -> "[" ++ x ++ "]") ipv6Fuzzer
        ]


ipv4Fuzzer : Fuzzer String
ipv4Fuzzer =
    Fuzz.intRange 0 255
        |> Fuzz.listOfLength 4
        |> Fuzz.map
            (List.map String.fromInt
                >> List.intersperse "."
                >> String.concat
            )


ipv6Fuzzer : Fuzzer String
ipv6Fuzzer =
    let
        num : Fuzzer String
        num =
            "0123456789abcdefABCDEF"
                |> String.toList
                |> Fuzz.oneOfValues
                |> Fuzz.listOfLength 4
                |> Fuzz.map String.fromList
    in
    Fuzz.oneOf
        [ Fuzz.listOfLength 8 num
            |> Fuzz.map (List.intersperse ":")
            |> Fuzz.map String.concat
        , Fuzz.listOfLengthBetween 0 7 num
            |> Fuzz.andThen
                (\front ->
                    num
                        |> Fuzz.listOfLengthBetween 0 (8 - List.length front)
                        |> Fuzz.map
                            (\back ->
                                [ front
                                    |> List.intersperse ":"
                                , [ "::" ]
                                , back
                                    |> List.intersperse ":"
                                ]
                                    |> List.concat
                                    |> String.concat
                            )
                )
        ]


portFuzzer : Fuzzer String
portFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant ""
        , Fuzz.intRange 0 65535
            |> Fuzz.map (\p -> ":" ++ String.fromInt p)
        ]


serverNameFuzzer : Fuzzer String
serverNameFuzzer =
    Fuzz.map2 (++) hostnameFuzzer portFuzzer


suite : Test
suite =
    describe "Server name tests"
        [ describe "Checking correct values"
            [ fuzz serverNameFuzzer
                "Correct server names validate"
                (\server ->
                    SN.fromString server
                        |> Maybe.map SN.toString
                        |> Expect.equal (Just server)
                )
            , test "Checking spec examples"
                (\() ->
                    let
                        examples : List String
                        examples =
                            [ "matrix.org"
                            , "matrix.org:8888"
                            , "1.2.3.4"
                            , "1.2.3.4:1234"
                            , "[1234:5678::abcd]"
                            , "[1234:5678::abcd]:5678"
                            ]
                    in
                    examples
                        |> List.map SN.fromString
                        |> List.map ((/=) Nothing)
                        |> Expect.equalLists
                            (List.repeat (List.length examples) True)
                )
            ]
        ]
