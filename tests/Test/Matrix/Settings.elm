module Test.Matrix.Settings exposing (..)

import Expect
import Fuzz
import Matrix.Settings
import Test exposing (..)
import Test.Types as TestTypes


settings : Test
settings =
    describe "Exposed Matrix.Settings"
        [ describe "Set values"
            [ fuzz2 TestTypes.vault
                Fuzz.string
                "Set device name"
                (\vault name ->
                    vault
                        |> Matrix.Settings.setDeviceName name
                        |> Matrix.Settings.getDeviceName
                        |> Expect.equal name
                )
            , fuzz2 TestTypes.vault
                Fuzz.int
                "Set sync time"
                (\vault sync ->
                    vault
                        |> Matrix.Settings.setSyncTime sync
                        |> Matrix.Settings.getSyncTime
                        |> Expect.equal sync
                )
            ]

        -- , describe "Read values" []
        ]
