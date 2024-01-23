module Dev.Route exposing (..)

{-| This module tests whether routes are translated correctly.
-}

import Expect
import Fuzz exposing (Fuzzer)
import Route exposing (Route(..), toRoute)
import Test exposing (..)
import Url


customPath : String -> Fuzzer Url.Url
customPath path =
    Fuzz.map (\url -> { url | path = path }) fuzzer



-- TODO: Create a more valid URL fuzzer


fuzzer : Fuzzer Url.Url
fuzzer =
    Fuzz.map6 Url.Url
        (Fuzz.oneOfValues [ Url.Http, Url.Https ])
        Fuzz.string
        (Fuzz.maybe Fuzz.int)
        Fuzz.string
        (Fuzz.maybe Fuzz.string)
        (Fuzz.maybe Fuzz.string)



-- urlCheck : Test
-- urlCheck =
--     describe "URL fuzzer tests"
--         [ fuzz fuzzer"Index always parses to url"
--             (\url ->
--                 url
--                     |> Url.toString
--                     |> Url.fromString
--                     |> Expect.equal (Just url)
--             )
--         ]


suite : Test
suite =
    describe "Route conversion"
        [ fuzz (customPath "/")
            "/ --> Home"
            (\url ->
                url
                    |> toRoute
                    |> Expect.equal Home
            )
        , fuzz (customPath "/home")
            "/home --> Home"
            (\url ->
                url
                    |> toRoute
                    |> Expect.equal Home
            )
        , fuzz (customPath "/index")
            "/index --> Home"
            (\url ->
                url
                    |> toRoute
                    |> Expect.equal Home
            )
        , fuzz
            (Fuzz.asciiString
                |> Fuzz.filter (not << String.contains "/")
                |> Fuzz.andThen
                    (\o ->
                        Fuzz.pair
                            (Fuzz.constant o)
                            (customPath ("/object/" ++ o))
                    )
            )
            "Object can be seen"
            (\( o, url ) ->
                url
                    |> toRoute
                    |> Expect.equal (ViewObject o)
            )
        ]
