module Test.Types exposing (..)

import Fuzz exposing (Fuzzer)
import Test.Values.Envelope as TestEnvelope
import Test.Values.Event as TestEvent
import Test.Values.Vault as TestVault
import Types exposing (..)


event : Fuzzer Event
event =
    Fuzz.map Event (TestEnvelope.fuzzer TestEvent.fuzzer)


vault : Fuzzer Vault
vault =
    Fuzz.map Vault (TestEnvelope.fuzzer TestVault.vault)
