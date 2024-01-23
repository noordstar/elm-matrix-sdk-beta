module Main exposing (main)

{-| This module creates a browser document that allows users to look at various
documentation elements of the Elm Matrix SDK.
-}

import Browser
import Browser.Navigation as Navigation
import Route exposing (Route(..))
import Url
import FastDict as Dict
import DocsDisplay as Display
import Internal.Values.StateManager
import Element
import Internal.Tools.Json as Json


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }


type alias Model =
    { key : Navigation.Key
    , page : Route.Route
    }


type Msg
    = OnTableSwitch ( String, Bool )
    | OnUrlChange Url.Url
    | OnUrlRequest Browser.UrlRequest



-- INIT


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init () url key =
    ( { key = key
      , page = Route.toRoute url
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTableSwitch _ ->
            ( model, Cmd.none )

        OnUrlChange url ->
            init () url model.key

        OnUrlRequest (Browser.Internal url) ->
            ( model, Navigation.pushUrl model.key (Url.toString url) )

        OnUrlRequest (Browser.External url) ->
            ( model, Navigation.load url )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = Route.toString model.page ++ " | Elm Matrix SDK Docs"
    , body =
        case model.page of
            _ ->
                Internal.Values.StateManager.coder
                    |> Json.toDocs
                    |> Display.render Dict.empty
                    |> Element.map OnTableSwitch
                    |> Element.layout []
                    |> List.singleton
    }
