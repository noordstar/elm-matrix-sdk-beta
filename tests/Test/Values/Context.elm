module Test.Values.Context exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Internal.Config.Leaks as Leaks
import Internal.Values.Context as Context exposing (Context)
import Json.Decode as D
import Json.Encode as E
import Test exposing (..)


fuzzer : Fuzzer Context
fuzzer =
    let
        maybeString : Fuzzer (Maybe String)
        maybeString =
            Fuzz.maybe Fuzz.string
    in
    Fuzz.map7 Context
        maybeString
        maybeString
        maybeString
        maybeString
        maybeString
        maybeString
        (Fuzz.maybe <| Fuzz.list Fuzz.string)


{-| If a leak is spotted, make sure to change the leaking value and then test
with the same seed to ensure it is not a (tiny) coincidence and a leak is in
fact coming through.
-}
leaks : Test
leaks =
    describe "No leaks allowed"
        [ fuzz2 fuzzer
            Fuzz.string
            "Access token"
            (\context value ->
                context
                    |> Context.apiFormat
                    |> Context.setAccessToken value
                    |> Context.getAccessToken
                    |> Expect.notEqual Leaks.accessToken
            )
        , fuzz2 fuzzer
            Fuzz.string
            "Base URL"
            (\context value ->
                context
                    |> Context.apiFormat
                    |> Context.setBaseUrl value
                    |> Context.getBaseUrl
                    |> Expect.notEqual Leaks.baseUrl
            )
        , fuzz2 fuzzer
            Fuzz.string
            "Transaction"
            (\context value ->
                context
                    |> Context.apiFormat
                    |> Context.setTransaction value
                    |> Context.getTransaction
                    |> Expect.notEqual Leaks.transaction
            )
        , fuzz2 fuzzer
            (Fuzz.list Fuzz.string)
            "Versions"
            (\context value ->
                context
                    |> Context.apiFormat
                    |> Context.setVersions value
                    |> Context.getVersions
                    |> Expect.notEqual Leaks.versions
            )
        ]


apiContext : Test
apiContext =
    describe "Verify writing info"
        [ fuzz2 fuzzer
            Fuzz.string
            "Access token"
            (\context value ->
                context
                    |> Context.apiFormat
                    |> Context.setAccessToken value
                    |> Context.getAccessToken
                    |> Expect.equal value
            )
        , fuzz2 fuzzer
            Fuzz.string
            "Base URL"
            (\context value ->
                context
                    |> Context.apiFormat
                    |> Context.setBaseUrl value
                    |> Context.getBaseUrl
                    |> Expect.equal value
            )
        , fuzz2 fuzzer
            Fuzz.string
            "Transaction"
            (\context value ->
                context
                    |> Context.apiFormat
                    |> Context.setTransaction value
                    |> Context.getTransaction
                    |> Expect.equal value
            )
        , fuzz2 fuzzer
            (Fuzz.list Fuzz.string)
            "Versions"
            (\context value ->
                context
                    |> Context.apiFormat
                    |> Context.setVersions value
                    |> Context.getVersions
                    |> Expect.equal value
            )
        ]


json : Test
json =
    describe "JSON encode + JSON decode"
        [ test "Empty is {}"
            (Context.init
                |> Context.encode
                |> E.encode 0
                |> Expect.equal "{}"
                |> always
            )
        , fuzz fuzzer
            "JSON recode"
            (\context ->
                context
                    |> Context.encode
                    |> D.decodeValue Context.decoder
                    |> Expect.equal (Ok ( context, [] ))
            )
        ]
