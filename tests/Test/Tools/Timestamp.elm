module Test.Tools.Timestamp exposing (..)

import Fuzz exposing (Fuzzer)
import Internal.Tools.Timestamp exposing (Timestamp)
import Test exposing (..)
import Time


fuzzer : Fuzzer Timestamp
fuzzer =
    Fuzz.map Time.millisToPosix Fuzz.int
