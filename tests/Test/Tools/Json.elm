module Test.Tools.Json exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Internal.Tools.Json as Json
import Json.Decode as D
import Json.Encode as E
import Test exposing (..)


type alias Human2 =
    { name : String, age : Maybe Int }


type alias Human3 =
    { name : String, age : Maybe Int, hobbies : List String }


type alias Human4 =
    { name : String
    , age : Maybe Int
    , hobbies : List String
    , weight : Maybe Float
    }


type alias Human5 =
    { name : String
    , age : Maybe Int
    , hobbies : List String
    , weight : Maybe Float
    , height : Float
    }


ageField : Json.Field (Maybe Int) { a | age : Maybe Int }
ageField =
    Json.field.optional.value
        { fieldName = "age"
        , toField = .age
        , description = []
        , coder = Json.int
        }


ageFuzzer : Fuzzer (Maybe Int)
ageFuzzer =
    Fuzz.maybe Fuzz.int


heightField : Json.Field Float { a | height : Float }
heightField =
    Json.field.required
        { fieldName = "height"
        , toField = .height
        , description = []
        , coder = Json.float
        }


heightFuzzer : Fuzzer Float
heightFuzzer =
    Fuzz.niceFloat


hobbiesField : Json.Field (List String) { a | hobbies : List String }
hobbiesField =
    Json.field.optional.withDefault
        { fieldName = "hobbies"
        , toField = .hobbies
        , description = []
        , coder = Json.list Json.string
        , default = ( [], [] )
        , defaultToString = always "[]"
        }


hobbiesFuzzer : Fuzzer (List String)
hobbiesFuzzer =
    Fuzz.list Fuzz.string


nameField : Json.Field String { a | name : String }
nameField =
    Json.field.required
        { fieldName = "name"
        , toField = .name
        , description = []
        , coder = Json.string
        }


nameFuzzer : Fuzzer String
nameFuzzer =
    Fuzz.string


weightField : Json.Field (Maybe Float) { a | weight : Maybe Float }
weightField =
    Json.field.optional.value
        { fieldName = "weight"
        , toField = .weight
        , description = []
        , coder = Json.float
        }


weightFuzzer : Fuzzer (Maybe Float)
weightFuzzer =
    -- TODO: Maybe make Float not so nice?
    Fuzz.maybe Fuzz.niceFloat


human2Coder : Json.Coder Human2
human2Coder =
    Json.object2
        { name = "Human2"
        , description = []
        , init = Human2
        }
        nameField
        ageField


human2Fuzzer : Fuzzer Human2
human2Fuzzer =
    Fuzz.map2 Human2
        nameFuzzer
        ageFuzzer


human3Coder : Json.Coder Human3
human3Coder =
    Json.object3
        { name = "Human3"
        , description = []
        , init = Human3
        }
        nameField
        ageField
        hobbiesField


human3Fuzzer : Fuzzer Human3
human3Fuzzer =
    Fuzz.map3 Human3
        nameFuzzer
        ageFuzzer
        hobbiesFuzzer


human4Coder : Json.Coder Human4
human4Coder =
    Json.object4
        { name = "Human4"
        , description = []
        , init = Human4
        }
        nameField
        ageField
        hobbiesField
        weightField


human4Fuzzer : Fuzzer Human4
human4Fuzzer =
    Fuzz.map4 Human4
        nameFuzzer
        ageFuzzer
        hobbiesFuzzer
        weightFuzzer


human5Coder : Json.Coder Human5
human5Coder =
    Json.object5
        { name = "Human5"
        , description = []
        , init = Human5
        }
        nameField
        ageField
        hobbiesField
        weightField
        heightField


human5Fuzzer : Fuzzer Human5
human5Fuzzer =
    Fuzz.map5 Human5
        nameFuzzer
        ageFuzzer
        hobbiesFuzzer
        weightFuzzer
        heightFuzzer


suite : Test
suite =
    describe "JSON module"
        [ describe "Human2"
            [ fuzz human2Fuzzer
                "Recoding succeeds"
                (\human ->
                    human
                        |> Json.encode human2Coder
                        |> E.encode 0
                        |> D.decodeString (Json.decode human2Coder)
                        |> Result.map Tuple.first
                        |> Expect.equal (Ok human)
                )
            ]
        , describe "Human3"
            [ fuzz human3Fuzzer
                "Recoding succeeds"
                (\human ->
                    human
                        |> Json.encode human3Coder
                        |> E.encode 0
                        |> D.decodeString (Json.decode human3Coder)
                        |> Result.map Tuple.first
                        |> Expect.equal (Ok human)
                )
            ]
        , describe "Human4"
            [ fuzz human4Fuzzer
                "Recoding succeeds"
                (\human ->
                    human
                        |> Json.encode human4Coder
                        |> E.encode 0
                        |> D.decodeString (Json.decode human4Coder)
                        |> Result.map Tuple.first
                        |> Expect.equal (Ok human)
                )
            ]
        , describe "Human5"
            [ fuzz human5Fuzzer
                "Recoding succeeds"
                (\human ->
                    human
                        |> Json.encode human5Coder
                        |> E.encode 0
                        |> D.decodeString (Json.decode human5Coder)
                        |> Result.map Tuple.first
                        |> Expect.equal (Ok human)
                )
            ]
        ]
