module DocsDisplay exposing (..)

import Internal.Tools.Json as Json exposing (Docs(..))
import Element exposing (Element)
import Element.Font as Font
import Widget
import Widget.Material as Material
import Widget.Material.Typography as Typography
import Colors as C
import Internal.Tools.Json as Json
import Internal.Tools.Json as Json
import Internal.Tools.Json as Json
import FastDict as Dict exposing (Dict)

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

render : Dict String Bool -> Docs -> Element (String, Bool)
render dict docs =
    docs
        |> findObjects
        |> List.map
            (\dobject ->
                Element.column []
                    [ Element.el Typography.h3
                        ( Element.text dobject.name )
                    , dobject.description
                        |> List.map (Element.text >> List.singleton >> Element.paragraph [])
                        |> Element.column []
                    , toTable (Dict.get dobject.name dict |> Maybe.withDefault True) dobject
                        |> Element.map (Tuple.pair dobject.name)
                    ]
            )
        |> List.append
            [ Element.paragraph []
                [ Element.text "This coder decodes to "
                , Element.el
                    [ Font.family [ Font.monospace ]
                    , C.background C.stdPicker.medium.white
                    ]
                    ( Element.text <| toString docs )
                ]
            ]
        |> Element.column []

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

toTable : Bool -> DObject -> Element Bool
toTable asc dobject =
    Widget.sortTableV2 (Material.sortTable <| C.defaultPalette C.stdPicker)
        { content = dobject.keys
        , columns =
            [ Widget.stringColumnV2
                { title = "Field"
                , value = .field
                , toString = identity
                , width = Element.fillPortion 1
                }
            , Widget.customColumnV2
                { title = "Type"
                , value =
                    (\item ->
                        item.content
                            |> toString
                            |> Element.text
                            |> Element.el
                                [ Font.family [Font.monospace]
                                , C.background C.stdPicker.dark.white
                                , Element.padding 3
                                , Element.centerX
                                ]
                    )
                , width = Element.fillPortion 1
                }
            , Widget.customColumnV2
                { title = "Description"
                , value = showDescription
                , width = Element.fillPortion 5
                }
            ]
        , asc = asc
        , sortBy = "Field"
        , onChange =
            (\f ->
                if f == "Field" then
                    not asc
                else
                    asc
            )
        }

{-| Show the description of a field in a table column.
-}
showDescription : { a | description : List String, required : Json.RequiredField } -> Element msg
showDescription { description, required } =
    case description of
        [] ->
            Element.column []
                [ Element.paragraph []
                    [ "WARNING: "
                        |> Element.text
                        |> Element.el [ Font.bold ]
                    , "This field has no documentation yet!"
                        |> Element.text
                    ]
                , case required of
                    Json.RequiredField ->
                        Element.paragraph []
                            [ "This field is required."
                                |> Element.text
                            ]
                    
                    Json.OptionalField ->
                        Element.paragraph []
                            [ "This field is optional."
                                |> Element.text
                            ]
                    
                    Json.OptionalFieldWithDefault default ->
                        Element.paragraph []
                            [ "This field is optional. If it is not there, a default value of \"" ++ default ++ "\" will be taken."
                                |> Element.text
                            ]
                ]
        
        head :: tail ->
            case required of
                Json.RequiredField ->
                    ( Element.paragraph []
                        [ Element.el [ Font.bold ] (Element.text "Required: ")
                        , Element.text head
                        ]
                    )
                    ::
                    ( List.map (Element.text >> List.singleton >> Element.paragraph []) tail)
                        |> Element.column []
                
                Json.OptionalField ->
                    description
                        |> List.map (Element.text >> List.singleton >> Element.paragraph [])
                        |> Element.column []
                
                Json.OptionalFieldWithDefault default ->
                    Element.paragraph []
                        [ Element.el [ Font.bold] (Element.text "Defaults to: ")
                        , Element.text default
                        ]
                        |> List.singleton
                        |> List.append (List.map (Element.text >> List.singleton >> Element.paragraph []) description)
                        |> Element.column []

{-| Write JSON type as a string.
-}
toString : Docs -> String
toString docs =
    case docs of
        DocsBool ->
            "bool"
        
        DocsDict d ->
            "{string:" ++ (toString d) ++ "}"
        
        DocsFloat ->
            "float"
        
        DocsInt ->
            "int"
        
        DocsLazy f ->
            toString (f ())
        
        DocsList d ->
            "[" ++ (toString d) ++ "]"
        
        DocsMap { content } ->
            "f(" ++ (toString content) ++ ")"
        
        DocsObject { name } ->
            name
        
        DocsOptional d ->
            toString d

        DocsRiskyMap { content } ->
            "f(" ++ (toString content) ++ ")"
        
        DocsString ->
            "string"
        
        DocsValue ->
            "<json>"
