module DocsDisplay exposing (..)

import Colors as C
import Element exposing (Element)
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Region as Region
import Html.Attributes
import Internal.Tools.Json as Json exposing (Docs(..))
import Widget.Material.Typography as Typography


type alias DObject =
    { name : String
    , description : List String
    , keys :
        List
            { field : String
            , description : List String
            , required : Json.RequiredField
            , content : Docs
            }
    }


render : Docs -> Element String
render docs =
    docs
        |> findObjects
        |> List.map
            (\dobject ->
                Element.column
                    [ Element.width Element.fill
                    , Element.spacing 12
                    ]
                    [ Element.el
                        (List.append
                            [ Region.heading 3
                            , Element.htmlAttribute <| Html.Attributes.id dobject.name
                            ]
                            Typography.h3
                        )
                        (Element.text dobject.name)
                    , dobject.description
                        |> List.map (Element.text >> List.singleton >> Element.paragraph [])
                        |> Element.column []
                    , toTable dobject
                    ]
            )
        |> List.append
            [ Element.paragraph []
                [ Element.text "This coder decodes to "
                , toString docs
                ]
            , showFunctions (getFunctions docs)
            ]
        |> Element.column
            [ Element.spacing 20
            , Element.width Element.fill
            ]


findObjects : Docs -> List DObject
findObjects docs =
    bfs [ docs ] []


bfs : List Docs -> List DObject -> List DObject
bfs queue acc =
    case queue of
        [] ->
            acc

        head :: tail ->
            case head of
                DocsBool ->
                    bfs tail acc

                DocsDict d ->
                    bfs (d :: tail) acc

                DocsFloat ->
                    bfs tail acc

                DocsInt ->
                    bfs tail acc

                DocsLazy f ->
                    bfs (f () :: tail) acc

                DocsList d ->
                    bfs (d :: tail) acc

                DocsMap { content } ->
                    bfs (content :: tail) acc

                DocsObject dobject ->
                    if List.any (\item -> item.name == dobject.name) acc then
                        bfs tail acc

                    else
                        bfs
                            (List.append tail (List.map .content dobject.keys))
                            (List.append acc [ dobject ])

                DocsOptional d ->
                    bfs (d :: tail) acc

                DocsRiskyMap { content } ->
                    bfs (content :: tail) acc

                DocsString ->
                    bfs tail acc

                DocsValue ->
                    bfs tail acc


toTable : DObject -> Element String
toTable dobject =
    let
        toCell : Element String -> Int -> Element String
        toCell content i =
            Element.el
                [ if (i |> modBy 2) == 0 then
                    C.background C.stdPicker.light.white

                  else
                    C.background C.stdPicker.medium.white
                , Element.padding 3
                ]
                content

        header : String -> Element msg
        header t =
            t
                |> Element.text
                |> Element.el
                    [ Element.height Element.fill
                    , Element.width Element.fill
                    , Font.bold
                    ]
    in
    Element.indexedTable
        [ C.background <| C.stdPicker.light.white ]
        { data = dobject.keys
        , columns =
            [ { header = header "Field"
              , width = Element.fillPortion 1
              , view = \i item -> toCell (Element.text item.field) i
              }
            , { header = header "Type"
              , width = Element.fillPortion 1
              , view = \i item -> toCell (toString item.content) i
              }
            , { header = header "Description"
              , width = Element.fillPortion 3
              , view = \i item -> showDescription i item
              }
            ]
        }


{-| Show the description of a field in a table column.
-}
showDescription : Int -> { a | content : Docs, description : List String, required : Json.RequiredField } -> Element msg
showDescription i { content, description, required } =
    Element.column
        [ if (i |> modBy 2) == 0 then
            C.background C.stdPicker.light.white

          else
            C.background C.stdPicker.medium.white
        , Element.padding 3
        ]
        -- Field description
        [ case description of
            [] ->
                Element.paragraph []
                    [ Element.el [ Font.bold ] <| Element.text "WARNING: "
                    , Element.text "This field has no documentation yet!"
                    ]

            head :: tail ->
                Element.column [ Element.width Element.fill ]
                    (List.append
                        [ Element.paragraph []
                            [ Element.el [ Font.bold ] <|
                                Element.text
                                    (case required of
                                        Json.RequiredField ->
                                            "Required: "

                                        _ ->
                                            ""
                                    )
                            , Element.text head
                            ]
                        ]
                        (List.map
                            (Element.text
                                >> List.singleton
                                >> Element.paragraph []
                            )
                            tail
                        )
                    )

        -- Additional function descriptions
        , showFunctions (getFunctions content)
        ]


showFunctions : List { name : String, description : List String } -> Element msg
showFunctions functions =
    functions
        |> List.indexedMap
            (\i f ->
                let
                    name : C.AllNames C.Color -> C.Color
                    name =
                        case modBy 5 i of
                            0 ->
                                .primary

                            1 ->
                                .secondary

                            2 ->
                                .tertiary

                            3 ->
                                .quaternary

                            _ ->
                                .extra
                in
                Element.column
                    [ Border.rounded 15
                    , C.background (name <| C.stdPicker.light)
                    , C.border <| name <| C.stdPicker.dark
                    , Border.width 2
                    , Element.padding 5
                    ]
                    ((f.name
                        |> (++) "Function "
                        |> Element.text
                        |> Element.el [ Font.bold ]
                     )
                        :: List.map
                            (Element.text
                                >> List.singleton
                                >> Element.paragraph []
                            )
                            f.description
                    )
            )
        |> Element.column
            [ Element.padding 5
            , Element.spacing 5
            , Element.width Element.fill
            ]


{-| Gather all the untranslatable functions that are hidden in the coders
-}
getFunctions : Docs -> List { name : String, description : List String }
getFunctions docs =
    getFunctionBFS docs []


getFunctionBFS : Docs -> List { name : String, description : List String } -> List { name : String, description : List String }
getFunctionBFS docs acc =
    case docs of
        DocsBool ->
            acc

        DocsDict d ->
            getFunctionBFS d acc

        DocsFloat ->
            acc

        DocsInt ->
            acc

        DocsLazy f ->
            getFunctionBFS (f ()) acc

        DocsList d ->
            getFunctionBFS d acc

        DocsMap { name, description, content } ->
            getFunctionBFS
                content
                (List.append acc [ { name = name, description = description } ])

        DocsObject _ ->
            acc

        DocsOptional d ->
            getFunctionBFS d acc

        DocsRiskyMap { name, description, content } ->
            getFunctionBFS
                content
                (List.append acc [ { name = name, description = description } ])

        DocsString ->
            acc

        DocsValue ->
            acc


{-| Write JSON type as a string.
-}
toString : Docs -> Element String
toString =
    let
        go : Docs -> List (Element String)
        go docs =
            case docs of
                DocsBool ->
                    [ Element.text "bool" ]

                DocsDict d ->
                    List.concat
                        [ [ Element.text "{string:" ]
                        , go d
                        , [ Element.text "}" ]
                        ]

                DocsFloat ->
                    [ Element.text "float" ]

                DocsInt ->
                    [ Element.text "int" ]

                DocsLazy f ->
                    go (f ())

                DocsList d ->
                    List.concat
                        [ [ Element.text "[" ]
                        , go d
                        , [ Element.text "]" ]
                        ]

                DocsMap { name, content } ->
                    List.concat
                        [ [ Element.text name, Element.text "(" ]
                        , go content
                        , [ Element.text ")" ]
                        ]

                DocsObject { name } ->
                    name
                        |> Element.text
                        |> Element.el
                            [ Events.onClick name ]
                        |> List.singleton

                DocsOptional d ->
                    go d

                DocsRiskyMap { name, content } ->
                    List.concat
                        [ [ Element.text name, Element.text "(" ]
                        , go content
                        , [ Element.text ")" ]
                        ]

                DocsString ->
                    [ Element.text "string" ]

                DocsValue ->
                    [ Element.text "JSON" ]
    in
    go
        >> Element.paragraph
            [ Font.family [ Font.monospace ]
            , C.background C.stdPicker.dark.white
            ]
