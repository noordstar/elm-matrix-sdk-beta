module Test.Values.Envelope exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Internal.Config.Default as Default
import Internal.Tools.Json as Json
import Internal.Values.Envelope as Envelope exposing (Envelope)
import Json.Decode as D
import Json.Encode as E
import Test exposing (..)
import Test.Values.Context as TestContext
import Test.Values.Settings as TestSettings


fuzzer : Fuzzer a -> Fuzzer (Envelope a)
fuzzer fuz =
    Fuzz.map3 Envelope
        fuz
        TestContext.fuzzer
        TestSettings.fuzzer


suite : Test
suite =
    describe "Envelope value"
        [ describe "init"
            [ describe "Default settings"
                [ fuzz Fuzz.string
                    "currentVersion"
                    (\s ->
                        { content = s, serverName = "" }
                            |> Envelope.init
                            |> Envelope.extractSettings .currentVersion
                            |> Expect.equal Default.currentVersion
                    )
                , fuzz Fuzz.string
                    "deviceName"
                    (\s ->
                        { content = s, serverName = "" }
                            |> Envelope.init
                            |> Envelope.extractSettings .deviceName
                            |> Expect.equal Default.deviceName
                    )
                , fuzz Fuzz.string
                    "syncTime"
                    (\s ->
                        { content = s, serverName = "" }
                            |> Envelope.init
                            |> Envelope.extractSettings .syncTime
                            |> Expect.equal Default.syncTime
                    )
                ]
            ]

        -- , describe "JSON"
        --     [ fuzz2 (fuzzer Fuzz.string)
        --         Fuzz.int
        --         "JSON encode -> JSON decode"
        --         (\envelope indent ->
        --             envelope
        --                 |> Envelope.encode Json.string
        --                 |> E.encode indent
        --                 |> D.decodeString (Envelope.decoder Json.string)
        --                 |> Expect.equal (Ok ( envelope, [] ))
        --         )
        --     ]
        ]
