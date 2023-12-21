module Vault exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Internal.Config.Default as Default
import Internal.Values.Envelope as Envelope
import Matrix
import Matrix.Settings
import Test exposing (..)
import Types


fuzzer : Fuzzer Matrix.Vault
fuzzer =
    Fuzz.constant <| Types.Vault <| Envelope.init {}


settings : Test
settings =
    describe "Edit settings"
        [ fuzz fuzzer
            "Default device name"
            (\vault ->
                vault
                    |> Matrix.Settings.getDeviceName
                    |> Expect.equal Default.deviceName
            )
        , fuzz2 fuzzer
            Fuzz.string
            "Set device name"
            (\vault name ->
                vault
                    |> Matrix.Settings.setDeviceName name
                    |> Matrix.Settings.getDeviceName
                    |> Expect.equal name
            )
        , fuzz fuzzer
            "Default sync time"
            (\vault ->
                vault
                    |> Matrix.Settings.getSyncTime
                    |> Expect.equal Default.syncTime
            )
        , fuzz2 fuzzer
            Fuzz.int
            "Set sync time"
            (\vault sync ->
                vault
                    |> Matrix.Settings.setSyncTime sync
                    |> Matrix.Settings.getSyncTime
                    |> Expect.equal sync
            )
        ]
