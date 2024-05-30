module Test.Values.User exposing (..)

import Fuzz exposing (Fuzzer)
import Internal.Grammar.ServerName as SN
import Internal.Values.User exposing (User)


fuzzer : Fuzzer User
fuzzer =
    Fuzz.constant
        { localpart = "temporary"
        , domain = { host = SN.DNS "matrix.org", port_ = Nothing }
        }
