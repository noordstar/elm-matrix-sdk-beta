module Envelope exposing (..)

import Context as TestContext
import Fuzz exposing (Fuzzer)
import Internal.Config.Default as Default
import Internal.Values.Envelope exposing (Envelope(..), Settings)
import Test exposing (..)


fuzzer : Fuzzer a -> Fuzzer (Envelope a)
fuzzer fuzz =
    Fuzz.map3
        (\content context settings ->
            Envelope
                { content = content
                , context = context
                , settings = settings
                }
        )
        fuzz
        TestContext.fuzzer
        fuzzerSettings


fuzzerSettings : Fuzzer Settings
fuzzerSettings =
    Fuzz.map3 Settings
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
            [ Fuzz.constant Default.syncTime
            , Fuzz.int
            ]
        )
