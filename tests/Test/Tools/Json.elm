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


type alias Human6 =
    { name : String
    , age : Maybe Int
    , hobbies : List String
    , weight : Maybe Float
    , height : Float
    , invitedToParty : Bool
    }


type alias Human7 =
    { name : String
    , age : Maybe Int
    , hobbies : List String
    , weight : Maybe Float
    , height : Float
    , invitedToParty : Bool
    , presentGiven : Maybe String
    }


type alias Human8 =
    { name : String
    , age : Maybe Int
    , hobbies : List String
    , weight : Maybe Float
    , height : Float
    , invitedToParty : Bool
    , presentGiven : Maybe String
    , grid : List (List Int)
    }


type alias MegaHuman =
    { human2 : Human2
    , human3 : Human3
    , human4 : Human4
    , human5 : Human5
    , human6 : Human6
    , human7 : Human7
    , human8 : Human8
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


gridField : Json.Field (List (List Int)) { a | grid : List (List Int) }
gridField =
    Json.field.optional.withDefault
        { fieldName = "grid"
        , toField = .grid
        , description = []
        , coder = Json.list (Json.list Json.int)
        , default = ( [], [] )
        }


gridFuzzer : Fuzzer (List (List Int))
gridFuzzer =
    Fuzz.list (Fuzz.list Fuzz.int)


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
        }


hobbiesFuzzer : Fuzzer (List String)
hobbiesFuzzer =
    Fuzz.list Fuzz.string


invitedToPartyField : Json.Field Bool { a | invitedToParty : Bool }
invitedToPartyField =
    Json.field.optional.withDefault
        { fieldName = "invitedToParty"
        , toField = .invitedToParty
        , description = []
        , coder = Json.bool
        , default = ( False, [] )
        }


invitedToPartyFuzzer : Fuzzer Bool
invitedToPartyFuzzer =
    Fuzz.bool


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


presentGivenField : Json.Field (Maybe String) { a | presentGiven : Maybe String }
presentGivenField =
    Json.field.required
        { fieldName = "presentGiven"
        , toField = .presentGiven
        , description = []
        , coder = Json.maybe Json.string
        }


presentGivenFuzzer : Fuzzer (Maybe String)
presentGivenFuzzer =
    Fuzz.maybe Fuzz.string


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


human6Coder : Json.Coder Human6
human6Coder =
    Json.object6
        { name = "Human6"
        , description = []
        , init = Human6
        }
        nameField
        ageField
        hobbiesField
        weightField
        heightField
        invitedToPartyField


human6Fuzzer : Fuzzer Human6
human6Fuzzer =
    Fuzz.map6 Human6
        nameFuzzer
        ageFuzzer
        hobbiesFuzzer
        weightFuzzer
        heightFuzzer
        invitedToPartyFuzzer


human7Coder : Json.Coder Human7
human7Coder =
    Json.object7
        { name = "Human7"
        , description = []
        , init = Human7
        }
        nameField
        ageField
        hobbiesField
        weightField
        heightField
        invitedToPartyField
        presentGivenField


human7Fuzzer : Fuzzer Human7
human7Fuzzer =
    Fuzz.map7 Human7
        nameFuzzer
        ageFuzzer
        hobbiesFuzzer
        weightFuzzer
        heightFuzzer
        invitedToPartyFuzzer
        presentGivenFuzzer


human8Coder : Json.Coder Human8
human8Coder =
    Json.object8
        { name = "Human8"
        , description = []
        , init = Human8
        }
        nameField
        ageField
        hobbiesField
        weightField
        heightField
        invitedToPartyField
        presentGivenField
        gridField


human8Fuzzer : Fuzzer Human8
human8Fuzzer =
    Fuzz.map8 Human8
        nameFuzzer
        ageFuzzer
        hobbiesFuzzer
        weightFuzzer
        heightFuzzer
        invitedToPartyFuzzer
        presentGivenFuzzer
        gridFuzzer


megaHumanCoder : Json.Coder MegaHuman
megaHumanCoder =
    Json.object7
        { name = "MegaHuman"
        , description = []
        , init = MegaHuman
        }
        (Json.field.required { fieldName = "h2", toField = .human2, description = [], coder = human2Coder })
        (Json.field.required { fieldName = "h3", toField = .human3, description = [], coder = human3Coder })
        (Json.field.required { fieldName = "h4", toField = .human4, description = [], coder = human4Coder })
        (Json.field.required { fieldName = "h5", toField = .human5, description = [], coder = human5Coder })
        (Json.field.required { fieldName = "h6", toField = .human6, description = [], coder = human6Coder })
        (Json.field.required { fieldName = "h7", toField = .human7, description = [], coder = human7Coder })
        (Json.field.required { fieldName = "h8", toField = .human8, description = [], coder = human8Coder })


megahumanFuzzer : Fuzzer MegaHuman
megahumanFuzzer =
    Fuzz.map7 MegaHuman
        human2Fuzzer
        human3Fuzzer
        human4Fuzzer
        human5Fuzzer
        human6Fuzzer
        human7Fuzzer
        human8Fuzzer


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
        , describe "Human6"
            [ fuzz human6Fuzzer
                "Recoding succeeds"
                (\human ->
                    human
                        |> Json.encode human6Coder
                        |> E.encode 0
                        |> D.decodeString (Json.decode human6Coder)
                        |> Result.map Tuple.first
                        |> Expect.equal (Ok human)
                )
            ]
        , describe "Human7"
            [ fuzz human7Fuzzer
                "Recoding succeeds"
                (\human ->
                    human
                        |> Json.encode human7Coder
                        |> E.encode 0
                        |> D.decodeString (Json.decode human7Coder)
                        |> Result.map Tuple.first
                        |> Expect.equal (Ok human)
                )
            ]
        , describe "Human8"
            [ fuzz human8Fuzzer
                "Recoding succeeds"
                (\human ->
                    human
                        |> Json.encode human8Coder
                        |> E.encode 0
                        |> D.decodeString (Json.decode human8Coder)
                        |> Result.map Tuple.first
                        |> Expect.equal (Ok human)
                )
            ]
        , describe "MegaHuman"
            [ fuzz megahumanFuzzer
                "Recoding succeeds"
                (\megahuman ->
                    megahuman
                        |> Json.encode megaHumanCoder
                        |> E.encode 0
                        |> D.decodeString (Json.decode megaHumanCoder)
                        |> Result.map Tuple.first
                        |> Expect.equal (Ok megahuman)
                )
            ]
        ]
