module Iddict exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Internal.Tools.Iddict as Iddict exposing (Iddict)
import Json.Decode as D
import Test exposing (..)
import Json.Encode as E
import Internal.Tools.Iddict as Iddict


fuzzer : Fuzzer a -> Fuzzer (Iddict a)
fuzzer fuz =
    fuz
        |> Fuzz.pair Fuzz.bool
        |> Fuzz.list
        |> Fuzz.map
            (\items ->
                List.foldl
                    (\( rm, item ) dict ->
                        case Iddict.insert item dict of
                            ( key, d ) ->
                                if rm then
                                    Iddict.remove key d

                                else
                                    d
                    )
                    Iddict.empty
                    items
            )


empty : Test
empty =
    describe "empty"
        [ test "isEmpty"
            (Iddict.empty
                |> Iddict.isEmpty
                |> Expect.equal True
                |> always
            )
        , fuzz Fuzz.int
            "No members"
            (\i ->
                Iddict.empty
                    |> Iddict.member i
                    |> Expect.equal False
            )
        , fuzz Fuzz.int
            "Get gets Nothing"
            (\i ->
                Iddict.empty
                    |> Iddict.get i
                    |> Expect.equal Nothing
            )
        , test "Size = 0"
            (Iddict.empty
                |> Iddict.size
                |> Expect.equal 0
                |> always
            )
        , test "No keys"
            (Iddict.empty
                |> Iddict.keys
                |> Expect.equal []
                |> always
            )
        , test "No values"
            (Iddict.empty
                |> Iddict.values
                |> Expect.equal []
                |> always
            )
        , test "JSON encode -> decode -> empty"
            (Iddict.empty
                |> Iddict.encode identity
                |> D.decodeValue (Iddict.decoder D.value)
                |> Expect.equal (Ok Iddict.empty)
                |> always
            )
        , test "JSON encode"
            (Iddict.empty
                |> Iddict.encode identity
                |> E.encode 0
                |> Expect.equal "{\"cursor\":0,\"dict\":{}}"
                |> always
            )
        , test "JSON decode"
            ("{\"cursor\":0,\"dict\":{}}"
                |> D.decodeString (Iddict.decoder D.value)
                |> Expect.equal (Ok Iddict.empty)
                |> always
            )
        ]

singleton : Test
singleton =
    let
        singleFuzzer : Fuzzer (Iddict Int)
        singleFuzzer =
            Fuzz.map
                (\i ->
                    Iddict.singleton i
                        |> Tuple.second
                )
                Fuzz.int
    in
        describe "singleton"
            [ fuzz singleFuzzer "not isEmpty"
                (\single ->
                    single
                        |> Iddict.isEmpty
                        |> Expect.equal False
                )
            , fuzz Fuzz.int "singleton == insert empty"
                (\i ->
                    Iddict.empty
                        |> Iddict.insert i
                        |> Expect.equal (Iddict.singleton i)
                )
            , fuzz Fuzz.int "First item is key 0"
                (\i ->
                    Iddict.singleton i
                        |> Tuple.first
                        |> Expect.equal 0
                )
            , fuzz singleFuzzer "Key 0 is member"
                (\single ->
                    single
                        |> Iddict.member 0
                        |> Expect.equal True
                )
            , fuzz Fuzz.int "Key 0 get returns Just value"
                (\i ->
                    Iddict.singleton i
                        |> Tuple.second
                        |> Iddict.get 0
                        |> Expect.equal (Just i)
                )
            , fuzz singleFuzzer "Size == 1"
                (\single ->
                    single
                        |> Iddict.size
                        |> Expect.equal 1
                )
            , fuzz Fuzz.int "Only key 0"
                (\i ->
                    Iddict.singleton i
                        |> Tuple.second
                        |> Iddict.keys
                        |> Expect.equal [ 0 ]
                )
            , fuzz Fuzz.int "Only value value"
                (\i ->
                    Iddict.singleton i
                        |> Tuple.second
                        |> Iddict.values
                        |> Expect.equal [ i ]
                )
            , fuzz singleFuzzer "JSON encode -> decode -> singleton"
                (\single ->
                    single
                        |> Iddict.encode E.int
                        |> D.decodeValue (Iddict.decoder D.int)
                        |> Expect.equal (Ok single)
                )
            , fuzz Fuzz.int "JSON encode"
                (\i ->
                    Iddict.singleton i
                        |> Tuple.second
                        |> Iddict.encode E.int
                        |> E.encode 0
                        |> Expect.equal ("{\"cursor\":1,\"dict\":{\"0\":" ++ (String.fromInt i) ++ "}}")
                )
            , fuzz Fuzz.int "JSON decode"
                (\i ->
                    ("{\"cursor\":1,\"dict\":{\"0\":" ++ (String.fromInt i) ++ "}}")
                        |> D.decodeString (Iddict.decoder D.int)
                        |> Tuple.pair 0
                        |> Expect.equal (Iddict.singleton i |> Tuple.mapSecond Ok)
                )
            ]

insert : Test
insert =
    describe "insert"
        [ fuzz2 (fuzzer Fuzz.int) Fuzz.int "Add something"
            (\d i ->
                case Iddict.insert i d of
                    ( key, dict ) ->
                        dict
                            |> Iddict.get key
                            |> Expect.equal (Just i)
            )
        , fuzz2 (fuzzer Fuzz.int) Fuzz.int "Never isEmpty"
            (\d i ->
                Iddict.insert i d
                    |> Tuple.second
                    |> Iddict.isEmpty
                    |> Expect.equal False
            )
        , fuzz2 (fuzzer Fuzz.int) Fuzz.int "New key"
            (\d i ->
                case Iddict.insert i d of
                    ( key, dict ) ->
                        dict
                            |> Iddict.remove key
                            |> Iddict.insert i
                            |> (\( newKey, _ ) ->
                                    Expect.notEqual key newKey
                               )
            )
        , fuzz2 (fuzzer Fuzz.int) Fuzz.int "New dict"
            (\d i ->
                case Iddict.insert i d of
                    ( key, dict ) ->
                        dict
                            |> Iddict.remove key
                            |> Iddict.insert i
                            |> (\( _, newDict ) ->
                                    Expect.notEqual dict newDict
                               )
            )
        , fuzz2 (fuzzer Fuzz.int) Fuzz.int "Inserted value is member"
            (\d i ->
                case Iddict.insert i d of
                    ( key, dict ) ->
                        dict
                            |> Iddict.member key
                            |> Expect.equal True
            )
        , fuzz2 (fuzzer Fuzz.int) Fuzz.int "Get inserted value"
            (\d i ->
                case Iddict.insert i d of
                    ( key, dict ) ->
                        dict
                            |> Iddict.get key
                            |> Expect.equal (Just i)
            )
        , fuzz2 (fuzzer Fuzz.int) Fuzz.int "size = size + 1"
            (\d i ->
                case Iddict.insert i d of
                    ( _, dict ) ->
                        Expect.equal
                            ( Iddict.size dict )
                            ( Iddict.size d + 1 )
            )
        ]
