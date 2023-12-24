module Test.Values.Event exposing (..)

import Fuzz exposing (Fuzzer)
import Internal.Values.Event as Event exposing (Event)
import Json.Encode as E
import Test exposing (..)
import Test.Tools.Timestamp as TestTimestamp


fuzzer : Fuzzer Event
fuzzer =
    Fuzz.map8 Event
        valueFuzzer
        Fuzz.string
        TestTimestamp.fuzzer
        Fuzz.string
        Fuzz.string
        (Fuzz.maybe Fuzz.string)
        Fuzz.string
        (Fuzz.maybe unsignedDataFuzzer)


unsignedDataFuzzer : Fuzzer Event.UnsignedData
unsignedDataFuzzer =
    Fuzz.map4
        (\age prev redact trans ->
            Event.UnsignedData
                { age = age
                , prevContent = prev
                , redactedBecause = redact
                , transactionId = trans
                }
        )
        (Fuzz.maybe Fuzz.int)
        (Fuzz.maybe valueFuzzer)
        (Fuzz.maybe <| Fuzz.lazy (\_ -> fuzzer))
        (Fuzz.maybe Fuzz.string)


{-| Example values that can be used for arbitrary JSON values
-}
valueFuzzer : Fuzzer E.Value
valueFuzzer =
    Fuzz.oneOf
        [ Fuzz.map E.int Fuzz.int
        , Fuzz.map E.string Fuzz.string
        , Fuzz.map (E.list E.int) (Fuzz.list Fuzz.int)
        , Fuzz.map (E.list E.string) (Fuzz.list Fuzz.string)
        , Fuzz.map Event.encode (Fuzz.lazy (\_ -> fuzzer))
        ]
