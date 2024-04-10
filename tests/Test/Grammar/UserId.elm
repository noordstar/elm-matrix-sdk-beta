module Test.Grammar.UserId exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Internal.Grammar.UserId as U
import Test exposing (..)
import Test.Grammar.ServerName as ServerName


modernUserCharFuzzer : Fuzzer Char
modernUserCharFuzzer =
    Fuzz.oneOf
        [ Fuzz.intRange 0x61 0x7A
            |> Fuzz.map Char.fromCode
        , "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
            |> String.toList
            |> Fuzz.oneOfValues
        ]


historicalUserCharFuzzer : Fuzzer Char
historicalUserCharFuzzer =
    [ ( 0x21, 0x39 ), ( 0x3B, 0x7E ) ]
        |> List.map (\( low, high ) -> Fuzz.intRange low high)
        |> Fuzz.oneOf
        |> Fuzz.map Char.fromCode


modernUserFuzzer : Fuzzer String
modernUserFuzzer =
    Fuzz.map2
        (\localpart domain ->
            let
                maxLocalSize : Int
                maxLocalSize =
                    255 - String.length domain - 2
            in
            localpart
                |> List.take maxLocalSize
                |> String.fromList
                |> (\l -> "@" ++ l ++ ":" ++ domain)
        )
        (Fuzz.listOfLengthBetween 1 255 modernUserCharFuzzer)
        (ServerName.serverNameFuzzer
            |> Fuzz.filter
                (\name ->
                    String.length name < 255 - 2
                )
        )


historicalUserFuzzer : Fuzzer String
historicalUserFuzzer =
    Fuzz.map2
        (\localpart domain ->
            let
                maxLocalSize : Int
                maxLocalSize =
                    255 - String.length domain - 2
            in
            localpart
                |> List.take maxLocalSize
                |> String.fromList
                |> (\l -> "@" ++ l ++ ":" ++ domain)
        )
        (Fuzz.listOfLengthBetween 1 255 historicalUserCharFuzzer)
        (ServerName.serverNameFuzzer
            |> Fuzz.filter
                (\name ->
                    String.length name < 255 - 2
                )
        )


userFuzzer : Fuzzer String
userFuzzer =
    Fuzz.oneOf [ modernUserFuzzer, historicalUserFuzzer ]


suite : Test
suite =
    describe "UserId"
        [ describe "Size"
            [ fuzz ServerName.serverNameFuzzer
                "Username cannot be length 0"
                (\domain ->
                    "@"
                        ++ ":"
                        ++ domain
                        |> U.fromString
                        |> Expect.equal Nothing
                )
            , fuzz2 (Fuzz.listOfLengthBetween 1 255 historicalUserCharFuzzer)
                ServerName.serverNameFuzzer
                "Username length cannot exceed 255"
                (\localpart domain ->
                    let
                        username : String
                        username =
                            "@"
                                ++ String.fromList localpart
                                ++ ":"
                                ++ domain
                    in
                    Expect.equal
                        (U.fromString username == Nothing)
                        (String.length username > 255)
                )
            , fuzz modernUserFuzzer
                "Modern fuzzer has appropriate size"
                (String.length >> Expect.lessThan 256)
            , fuzz historicalUserFuzzer
                "Historical fuzzer has appropriate size"
                (String.length >> Expect.lessThan 256)
            ]
        , describe "From string evaluation"
            [ fuzz userFuzzer
                "fromString always returns a value on fuzzer"
                (U.fromString >> Expect.notEqual Nothing)
            , fuzz userFuzzer
                "fromString -> toString returns the same value"
                (\username ->
                    username
                        |> U.fromString
                        |> Maybe.map U.toString
                        |> Expect.equal (Just username)
                )
            , fuzz historicalUserFuzzer
                "Historical users are historical"
                (\username ->
                    username
                        |> U.fromString
                        |> Maybe.map U.isHistorical
                        |> Expect.equal (Just True)
                )
            , fuzz modernUserFuzzer
                "Modern users are not historical"
                (\username ->
                    username
                        |> U.fromString
                        |> Maybe.map U.isHistorical
                        |> Expect.equal (Just False)
                )
            ]
        ]
