module Test.Types.HomeserverUrl exposing (suite)

{-| Tests for building Matrix API URLs from a homeserver base URL.

@docs suite

-}

import Expect
import Matrix.Types.HomeserverUrl as HomeserverUrl
import Test exposing (Test, describe, test)


{-| Build a URL for `path` and `query` on the homeserver at `base`, or explain
why the base URL couldn't be parsed.
-}
build : String -> List String -> List ( String, Maybe String ) -> Result String String
build base path query =
    HomeserverUrl.fromString base
        |> Maybe.map (HomeserverUrl.atPath path query)
        |> Result.fromMaybe ("Invalid base URL: " ++ base)


{-| All tests in this module.
-}
suite : Test
suite =
    describe "HomeserverUrl"
        [ describe "atPath"
            [ test "builds a path at the domain root" <|
                \() ->
                    build "https://example.org" [ "_matrix", "client", "versions" ] []
                        |> Expect.equal (Ok "https://example.org/_matrix/client/versions")
            , test "keeps a path prefix" <|
                \() ->
                    build "https://example.org/matrix" [ "_matrix", "client", "versions" ] []
                        |> Expect.equal (Ok "https://example.org/matrix/_matrix/client/versions")
            , test "ignores trailing slashes on the prefix" <|
                \() ->
                    build "https://example.org/matrix//" [ "_matrix", "client", "versions" ] []
                        |> Expect.equal (Ok "https://example.org/matrix/_matrix/client/versions")
            , test "drops query and fragment from the base URL" <|
                \() ->
                    build "https://example.org/?a=b#c" [ "_matrix", "client", "versions" ] []
                        |> Expect.equal (Ok "https://example.org/_matrix/client/versions")
            , test "keeps the port" <|
                \() ->
                    build "http://localhost:8008" [ "_matrix", "client", "versions" ] []
                        |> Expect.equal (Ok "http://localhost:8008/_matrix/client/versions")
            , test "percent-encodes path segments" <|
                \() ->
                    build "https://example.org" [ "_matrix", "client", "v3", "rooms", "!abc:example.org", "event", "$ev/1" ] []
                        |> Expect.equal (Ok "https://example.org/_matrix/client/v3/rooms/!abc%3Aexample.org/event/%24ev%2F1")
            , test "adds a single question mark before the query" <|
                \() ->
                    build "https://example.org" [ "_matrix", "client", "v3", "sync" ] [ ( "timeout", Just "30000" ), ( "since", Nothing ), ( "full_state", Just "true" ) ]
                        |> Expect.equal (Ok "https://example.org/_matrix/client/v3/sync?timeout=30000&full_state=true")
            ]
        , describe "toString"
            [ test "includes the path prefix" <|
                \() ->
                    HomeserverUrl.fromString "https://example.org/matrix/"
                        |> Maybe.map HomeserverUrl.toString
                        |> Expect.equal (Just "https://example.org/matrix")
            , test "uses a single slash without a prefix" <|
                \() ->
                    HomeserverUrl.fromString "https://example.org"
                        |> Maybe.map HomeserverUrl.toString
                        |> Expect.equal (Just "https://example.org/")
            ]
        ]
