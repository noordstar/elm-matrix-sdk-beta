module Event exposing (..)

import Envelope as TestEnvelope
import Expect
import Fuzz exposing (Fuzzer)
import Iddict as TestIddict
import Internal.Tools.Iddict as Iddict
import Internal.Tools.Timestamp as Timestamp
import Internal.Values.Envelope as Envelope
import Internal.Values.Event as Event
import Json.Decode as D
import Json.Encode as E
import Test exposing (..)
import Timestamp as TestTimestamp


{-| Example values that can be used for arbitrary JSON values
-}
valueFuzzer : Fuzzer E.Value
valueFuzzer =
    Fuzz.oneOf
        [ Fuzz.map (Iddict.encode E.int) (TestIddict.fuzzer Fuzz.int)
        , Fuzz.map Timestamp.encode TestTimestamp.fuzzer
        , Fuzz.map E.int Fuzz.int
        , Fuzz.map E.string Fuzz.string
        , Fuzz.map (E.list E.int) (Fuzz.list Fuzz.int)
        , Fuzz.map (E.list E.string) (Fuzz.list Fuzz.string)
        , Fuzz.map Event.encode (Fuzz.lazy (\_ -> TestEnvelope.fuzzer fuzzer))
        ]


fuzzer : Fuzzer Event.IEvent
fuzzer =
    Fuzz.map8
        (\c ei et o r se sk u ->
            { content = c
            , eventId = ei
            , eventType = et
            , originServerTs = o
            , roomId = r
            , sender = se
            , stateKey = sk
            , unsigned = u
            }
        )
        valueFuzzer
        Fuzz.string
        Fuzz.string
        TestTimestamp.fuzzer
        Fuzz.string
        Fuzz.string
        (Fuzz.maybe Fuzz.string)
        (Fuzz.maybe unsignedDataFuzzer)


fuzzerFull : Fuzzer Event.Event
fuzzerFull =
    TestEnvelope.fuzzer fuzzer


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


json : Test
json =
    describe "JSON tests"
        [ fuzz fuzzerFull
            "JSON encode + JSON decode"
            (\event ->
                event
                    |> Event.encode
                    |> D.decodeValue Event.decoder
                    |> Expect.equal (Ok event)
            )
        ]
