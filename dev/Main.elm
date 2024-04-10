module Main exposing (main)

{-| This module creates a browser document that allows users to look at various
documentation elements of the Elm Matrix SDK.
-}

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Navigation
import Colors as C
import Dict exposing (Dict)
import DocsDisplay as Display
import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import Internal.Tools.Hashdict
import Internal.Tools.Json as Json
import Internal.Tools.Mashdict
import Internal.Tools.Timestamp
import Internal.Values.Context
import Internal.Values.Envelope
import Internal.Values.Event
import Internal.Values.Settings
import Internal.Values.StateManager
import Internal.Values.Timeline
import Json.Decode as D
import Route exposing (Route(..))
import Task
import Url
import Widget
import Widget.Material as Material


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


coders : Dict String ( Json.Docs, String -> Result String String )
coders =
    Dict.fromList
        [ ( "Context", shapeCoder <| Internal.Values.Context.coder )
        , ( "Envelope", shapeCoder <| Internal.Values.Envelope.coder Json.value )
        , ( "Event", shapeCoder <| Internal.Values.Event.coder )
        , ( "Hashdict", shapeCoder <| Internal.Tools.Hashdict.coder .eventId Internal.Values.Event.coder )
        , ( "Mashdict", shapeCoder <| Internal.Tools.Mashdict.coder .stateKey Internal.Values.Event.coder )
        , ( "Settings", shapeCoder <| Internal.Values.Settings.coder )
        , ( "StateManager", shapeCoder <| Internal.Values.StateManager.coder )
        , ( "Timeline", shapeCoder <| Internal.Values.Timeline.coder )
        , ( "Timestamp", shapeCoder <| Internal.Tools.Timestamp.coder )
        ]


shapeCoder : Json.Coder a -> ( Json.Docs, String -> Result String String )
shapeCoder coder =
    ( Json.toDocs coder
    , decoder coder
    )


decoder : Json.Coder a -> String -> Result String String
decoder coder value =
    D.decodeString (Json.decode coder) value
        |> Result.mapError D.errorToString
        |> Result.map Debug.toString
        |> Result.map ((++) "Success! JSON decoded to Elm value: ")


type alias Model =
    { key : Navigation.Key
    , page : Route.Route
    , input : String
    , valid : Maybe (Result String String)
    }


type Msg
    = OnDecodeString
    | OnObjectClick String
    | OnScreenMoved Bool
    | OnUrlChange Url.Url
    | OnUrlRequest Browser.UrlRequest
    | OnWriteJSON String



-- INIT


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init () url key =
    ( { key = key
      , page = Route.toRoute url
      , input = ""
      , valid = Nothing
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnDecodeString ->
            case model.page of
                ViewObject o ->
                    ( { model
                        | valid =
                            coders
                                |> Dict.get o
                                |> Maybe.map Tuple.second
                                |> Maybe.map ((|>) model.input)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        OnObjectClick name ->
            ( model
            , Dom.getElement name
                |> Task.andThen
                    (\data ->
                        Dom.setViewport
                            data.element.x
                            data.element.y
                    )
                |> Task.map (always True)
                |> Task.onError (\_ -> Task.succeed False)
                |> Task.perform OnScreenMoved
            )

        OnScreenMoved _ ->
            ( model, Cmd.none )

        OnUrlChange url ->
            init () url model.key

        OnUrlRequest (Browser.Internal url) ->
            ( model, Navigation.pushUrl model.key (Url.toString url) )

        OnUrlRequest (Browser.External url) ->
            ( model, Navigation.load url )

        OnWriteJSON text ->
            ( { model | input = text }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = Route.toString model.page ++ " | Elm Matrix SDK Docs"
    , body =
        [ Widget.menuBar (Material.menuBar <| C.defaultPalette C.stdPicker)
            { title =
                Element.link []
                    { url = "/"
                    , label =
                        "Elm Matrix SDK Docs"
                            |> Element.text
                            |> Element.el
                                [ C.font C.stdPicker.light.white
                                ]
                    }
            , deviceClass = Element.Phone
            , openLeftSheet = Nothing
            , openRightSheet = Nothing
            , openTopSheet = Nothing
            , primaryActions = []
            , search = Nothing
            }
        , (case model.page of
            ViewObject o ->
                Element.column [ Element.spacing 10 ]
                    [ coders
                        |> Dict.get o
                        |> Maybe.map (Tuple.first >> showDocs)
                        |> Maybe.withDefault (Element.text "This object doesn't exist!")
                    , Element.column
                        [ Element.width Element.fill
                        , Element.height <| Element.minimum 100 <| Element.fill
                        ]
                        [ Input.multiline
                            [ case model.valid of
                                Just (Err _) ->
                                    C.background C.stdPicker.light.secondary

                                Just (Ok _) ->
                                    C.background C.stdPicker.light.quaternary

                                Nothing ->
                                    Element.width Element.fill
                            , Element.width Element.fill
                            ]
                            { onChange = OnWriteJSON
                            , text = model.input
                            , placeholder =
                                "Insert a test JSON object..."
                                    |> Element.text
                                    |> Input.placeholder []
                                    |> Just
                            , label = Input.labelHidden "Test input JSON"
                            , spellcheck = False
                            }
                        , Widget.button
                            (Material.outlinedButton <| C.defaultPalette C.stdPicker)
                            { text = "Check JSON"
                            , icon = always Element.none
                            , onPress = Just OnDecodeString
                            }
                        , case model.valid of
                            Nothing ->
                                Element.none

                            Just (Ok msg) ->
                                msg
                                    |> Element.text
                                    |> List.singleton
                                    |> Element.paragraph
                                        [ C.font <| C.stdPicker.dark.quaternary ]

                            Just (Err msg) ->
                                msg
                                    |> Element.text
                                    |> List.singleton
                                    |> Element.paragraph
                                        [ C.font <| C.stdPicker.dark.secondary ]
                        ]
                    ]

            Home ->
                Element.column [ Element.spacing 10 ]
                    (List.append
                        [ Element.paragraph []
                            [ Element.text "This is the Home of the Elm Matrix SDK JSON documentation tool."
                            ]
                        , Element.paragraph []
                            [ Element.text "This tool helps you debug and explore JSON data types that are used by the Elm Matrix SDK."
                            ]
                        ]
                        (coders
                            |> Dict.keys
                            |> List.map
                                (\name ->
                                    Element.link
                                        [ C.font <| C.stdPicker.medium.primary
                                        , Font.underline
                                        ]
                                        { url = "/object/" ++ name
                                        , label = Element.text name
                                        }
                                )
                        )
                    )

            _ ->
                Internal.Values.StateManager.coder
                    |> Json.toDocs
                    |> showDocs
          )
            |> Element.el
                [ Element.paddingXY 120 120
                , Element.height Element.fill
                ]
        ]
            |> Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.alignTop
                ]
            |> Element.layout
                [ C.background <| C.stdPicker.dark.white
                , C.font <| C.stdPicker.dark.black
                ]
            |> List.singleton
    }


showDocs : Json.Docs -> Element Msg
showDocs =
    Display.render >> Element.map OnObjectClick
