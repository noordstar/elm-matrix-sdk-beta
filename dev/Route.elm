module Route exposing (..)

{-| This module helps parse the URL route into explicable data.
-}

import Url
import Url.Parser as P exposing ((</>))


type Route
    = Home
    | NotFound
    | ViewObject String


toRoute : Url.Url -> Route
toRoute url =
    P.parse routeParser url |> Maybe.withDefault NotFound


toString : Route -> String
toString route =
    case route of
        Home ->
            "Home"

        NotFound ->
            "404"

        ViewObject o ->
            o


routeParser : P.Parser (Route -> a) a
routeParser =
    P.oneOf
        [ P.top
            |> P.map Home
        , P.s "home"
            |> P.map Home
        , P.s "index"
            |> P.map Home
        , P.s "dev"
            </> (P.s "Main.elm")
            |> P.map Home
        , P.s "object"
            </> P.string
            |> P.map ViewObject
        , P.s "object"
            </> P.top
            |> P.map (ViewObject "")
        ]
