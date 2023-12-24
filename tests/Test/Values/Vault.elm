module Test.Values.Vault exposing (..)

import Fuzz exposing (Fuzzer)
import Internal.Values.Vault exposing (Vault)
import Test exposing (..)


vault : Fuzzer Vault
vault =
    Fuzz.unit
