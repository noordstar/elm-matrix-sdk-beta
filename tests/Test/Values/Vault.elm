module Test.Values.Vault exposing (..)

import FastDict as Dict
import Fuzz exposing (Fuzzer)
import Internal.Tools.Json as Json
import Internal.Values.Vault exposing (Vault)
import Test exposing (..)
import Test.Tools.Hashdict as TestHashdict
import Test.Values.Room as TestRoom
import Test.Values.User as TestUser


vault : Fuzzer Vault
vault =
    Fuzz.map3 Vault
        (Fuzz.string
            |> Fuzz.map (\k -> ( k, Json.encode Json.int 0 ))
            |> Fuzz.list
            |> Fuzz.map Dict.fromList
        )
        (TestHashdict.fuzzer .roomId TestRoom.fuzzer)
        TestUser.fuzzer
