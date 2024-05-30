module Test.Values.Settings exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Internal.Config.Default as Default
import Internal.Values.Settings as Settings exposing (Settings)
import Json.Decode as D
import Json.Encode as E
import Test exposing (..)


fuzzer : Fuzzer Settings
fuzzer =
    Fuzz.map4 Settings
        (Fuzz.oneOf
            [ Fuzz.constant Default.currentVersion
            , Fuzz.string
            ]
        )
        (Fuzz.oneOf
            [ Fuzz.constant Default.deviceName
            , Fuzz.string
            ]
        )
        (Fuzz.oneOf
            [ Fuzz.constant Default.removePasswordOnLogin
            , Fuzz.bool
            ]
        )
        (Fuzz.oneOf
            [ Fuzz.constant Default.syncTime
            , Fuzz.int
            ]
        )


suite : Test
suite =
    describe "Settings"
        [ describe "init"
            [ test "Current version"
                (Settings.init
                    |> .currentVersion
                    |> Expect.equal Default.currentVersion
                    |> always
                )
            , test "Device name"
                (Settings.init
                    |> .deviceName
                    |> Expect.equal Default.deviceName
                    |> always
                )
            , test "Remove password on login"
                (Settings.init
                    |> .removePasswordOnLogin
                    |> Expect.equal Default.removePasswordOnLogin
                    |> always
                )
            , test "Sync time"
                (Settings.init
                    |> .syncTime
                    |> Expect.equal Default.syncTime
                    |> always
                )
            , test "JSON encode init is {}"
                (Settings.init
                    |> Settings.encode
                    |> E.encode 0
                    |> Expect.equal "{}"
                    |> always
                )
            , test "JSON decode {} is init"
                ("{}"
                    |> D.decodeString Settings.decoder
                    |> Expect.equal (Ok ( Settings.init, [] ))
                    |> always
                )
            ]
        , describe "JSON"
            [ fuzz2 fuzzer
                Fuzz.int
                "JSON encode -> JSON decode -> identical"
                (\settings indent ->
                    settings
                        |> Settings.encode
                        |> E.encode indent
                        |> D.decodeString Settings.decoder
                        |> Expect.equal (Ok ( settings, [] ))
                )
            ]
        ]
