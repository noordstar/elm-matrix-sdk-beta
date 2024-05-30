module Test.Values.Context exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Internal.Config.Leaks as Leaks
import Internal.Tools.Hashdict as Hashdict
import Internal.Values.Context as Context exposing (Context, Versions)
import Json.Decode as D
import Json.Encode as E
import Set
import Test exposing (..)
import Test.Tools.Timestamp as TestTimestamp


fuzzer : Fuzzer Context
fuzzer =
    let
        maybeString : Fuzzer (Maybe String)
        maybeString =
            Fuzz.maybe Fuzz.string
    in
    Fuzz.map8 (\a b c d e ( f, g ) ( h, i ) ( j, k ) -> Context a b c d e f g h i j k)
        (Fuzz.constant <| Hashdict.empty .value)
        maybeString
        maybeString
        (Fuzz.maybe TestTimestamp.fuzzer)
        maybeString
        (Fuzz.pair
            maybeString
            Fuzz.string
        )
        (Fuzz.pair
            maybeString
            maybeString
        )
        (Fuzz.pair
            maybeString
            (Fuzz.maybe <| versionsFuzzer)
        )


versionsFuzzer : Fuzzer Versions
versionsFuzzer =
    Fuzz.map2 Versions
        (Fuzz.list Fuzz.string)
        (Fuzz.map Set.fromList <| Fuzz.list Fuzz.string)


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
            versionsFuzzer
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
            versionsFuzzer
            "Versions"
            (\context value ->
                context
                    |> Context.apiFormat
                    |> Context.setVersions value
                    |> Context.getVersions
                    |> Expect.equal value
            )
        ]



-- json : Test
-- json =
--     describe "JSON encode + JSON decode"
--         [ fuzz fuzzer
--             "JSON recode"
--             (\context ->
--                 context
--                     |> Context.encode
--                     |> D.decodeValue Context.decoder
--                     |> Expect.equal (Ok ( context, [] ))
--             )
--         ]
