module Test.Tools.RationalOrder exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Internal.Tools.RationalOrder as RO exposing (RationalOrder(..))
import Test exposing (..)


fuzzer : Fuzzer RationalOrder
fuzzer =
    Fuzz.map2 With Fuzz.int (Fuzz.lazy (\_ -> Fuzz.maybe fuzzer))


twoUnequal : Fuzzer ( RationalOrder, RationalOrder )
twoUnequal =
    fuzzer
        |> Fuzz.andThen
            (\o ->
                Fuzz.map2
                    (\o1 o2 ->
                        if RO.compare o1 o2 == LT then
                            ( o1, o2 )

                        else
                            ( o2, o1 )
                    )
                    (Fuzz.constant o)
                    (Fuzz.filter ((/=) o) fuzzer)
            )


suite : Test
suite =
    describe "RationalOrder"
        [ describe "Semantic truths"
            [ describe "After is always greater"
                [ fuzz fuzzer
                    "Forwards"
                    (\o ->
                        Expect.equal LT (RO.compare o (RO.after o))
                    )
                , fuzz fuzzer
                    "Backwards"
                    (\o ->
                        Expect.equal GT (RO.compare (RO.after o) o)
                    )
                ]
            , describe "Before is always lesser"
                [ fuzz fuzzer
                    "Forwards"
                    (\o ->
                        Expect.equal GT (RO.compare o (RO.before o))
                    )
                , fuzz fuzzer
                    "Backwards"
                    (\o ->
                        Expect.equal LT (RO.compare (RO.before o) o)
                    )
                ]
            , describe "Two unequal == two unequal"
                [ fuzz twoUnequal
                    "Forwards"
                    (\( small, big ) ->
                        Expect.equal LT (RO.compare small big)
                    )
                , fuzz twoUnequal
                    "Backwards"
                    (\( small, big ) ->
                        Expect.equal GT (RO.compare big small)
                    )
                ]
            , describe "compare"
                [ fuzz2 fuzzer
                    fuzzer
                    "EQ iff same value"
                    (\o1 o2 ->
                        Expect.equal
                            (o1 == o2)
                            (RO.compare o1 o2 == EQ)
                    )
                , fuzz2 fuzzer
                    fuzzer
                    "LT iff opposite GT"
                    (\o1 o2 ->
                        Expect.equal
                            (RO.compare o1 o2 == LT)
                            (RO.compare o2 o1 == GT)
                    )
                ]
            , describe "Between is always between"
                [ fuzz twoUnequal
                    "Less than first - forwards"
                    (\( small, big ) ->
                        RO.between small big
                            |> RO.compare small
                            |> Expect.equal LT
                    )
                , fuzz twoUnequal
                    "Less than first - backwards"
                    (\( small, big ) ->
                        small
                            |> RO.compare (RO.between small big)
                            |> Expect.equal GT
                    )
                , fuzz twoUnequal
                    "Less than second - forwards"
                    (\( small, big ) ->
                        RO.between small big
                            |> RO.compare big
                            |> Expect.equal GT
                    )
                , fuzz twoUnequal
                    "Less than second - backwards"
                    (\( small, big ) ->
                        big
                            |> RO.compare (RO.between small big)
                            |> Expect.equal LT
                    )
                ]
            ]
        , describe "Between creates between"
            [ test "With 0 Nothing <--> With 1 Nothing"
                (\() ->
                    RO.between (With 0 Nothing) (With 1 Nothing)
                        |> Expect.equal (With 0 (Just (With 0 Nothing)))
                )
            , test "With 1 Nothing <--> With 0 Nothing"
                (\() ->
                    RO.between (With 1 Nothing) (With 0 Nothing)
                        |> Expect.equal (With 0 (Just (With 0 Nothing)))
                )
            , test "With 0 is filled between With 1 Nothing"
                (\() ->
                    With 0 Nothing
                        |> RO.between (With 1 Nothing)
                        |> RO.between (With 1 Nothing)
                        |> RO.between (With 1 Nothing)
                        |> RO.between (With 1 Nothing)
                        |> RO.between (With 1 Nothing)
                        |> RO.between (With 1 Nothing)
                        |> Expect.equal (With 0 (Just (With 5 Nothing)))
                )
            , test "Will start counting high level as soon as possible"
                (\() ->
                    With 0 Nothing
                        |> RO.between (With 1 Nothing)
                        |> RO.between (With 1 Nothing)
                        |> RO.between (With 1 Nothing)
                        |> RO.between (With 1 Nothing)
                        |> RO.between (With 1 Nothing)
                        |> RO.between (With 1 Nothing)
                        |> RO.between (With 5 Nothing)
                        |> RO.between (With 5 Nothing)
                        |> Expect.equal (With 2 Nothing)
                )
            , test "Will start counting high level, then return lower level"
                (\() ->
                    With 0 Nothing
                        |> RO.between (With 1 Nothing)
                        |> RO.between (With 1 Nothing)
                        |> RO.between (With 1 Nothing)
                        |> RO.between (With 1 Nothing)
                        |> RO.between (With 1 Nothing)
                        |> RO.between (With 1 Nothing)
                        |> RO.between (With 5 Nothing)
                        |> RO.between (With 5 Nothing)
                        |> RO.between (With 5 Nothing)
                        |> RO.between (With 5 Nothing)
                        |> RO.between (With 5 Nothing)
                        |> RO.between (With 5 Nothing)
                        |> RO.between (With 5 Nothing)
                        |> RO.between (With 5 Nothing)
                        |> RO.between (With 5 Nothing)
                        |> RO.between (With 5 Nothing)
                        |> RO.between (With 5 Nothing)
                        |> Expect.equal (With 4 (Just (With 6 Nothing)))
                )
            , fuzz2 fuzzer
                fuzzer
                "Between is commutative"
                (\o1 o2 ->
                    Expect.equal (RO.between o1 o2) (RO.between o2 o1)
                )
            ]
        , describe "After"
            [ fuzz Fuzz.int
                "One more - level 1"
                (\a ->
                    Expect.equal
                        (RO.after <| With a Nothing)
                        (With (a + 1) Nothing)
                )
            , fuzz2 Fuzz.int
                Fuzz.int
                "One more - level 2"
                (\a b ->
                    Expect.equal
                        (RO.after <| With a <| Just <| With b Nothing)
                        (With (a + 1) Nothing)
                )
            , fuzz3 Fuzz.int
                Fuzz.int
                Fuzz.int
                "One more - level 3"
                (\a b c ->
                    Expect.equal
                        (RO.after <| With a <| Just <| With b <| Just <| With c Nothing)
                        (With (a + 1) Nothing)
                )
            ]
        , describe "Before"
            [ fuzz Fuzz.int
                "One less - level 1"
                (\a ->
                    Expect.equal
                        (RO.before <| With a Nothing)
                        (With (a - 1) Nothing)
                )
            , fuzz2 Fuzz.int
                Fuzz.int
                "One less - level 2"
                (\a b ->
                    Expect.equal
                        (RO.before <| With a <| Just <| With b Nothing)
                        (With (a - 1) Nothing)
                )
            , fuzz3 Fuzz.int
                Fuzz.int
                Fuzz.int
                "One less - level 3"
                (\a b c ->
                    Expect.equal
                        (RO.before <| With a <| Just <| With b <| Just <| With c Nothing)
                        (With (a - 1) Nothing)
                )
            ]
        , describe "Compare vs. list compare"
            [ fuzz2
                (Fuzz.listOfLengthBetween 1 32 Fuzz.int)
                (Fuzz.listOfLengthBetween 1 32 Fuzz.int)
                "Compares the same between normal lists and orders"
                (\l1 l2 ->
                    Expect.equal
                        (Just <| Basics.compare l1 l2)
                        (Maybe.map2 RO.compare (RO.fromList l1) (RO.fromList l2))
                )
            , fuzz2 fuzzer
                fuzzer
                "Compares the same when converted to list"
                (\o1 o2 ->
                    Expect.equal
                        (RO.compare o1 o2)
                        (Basics.compare (RO.toList o1) (RO.toList o2))
                )
            ]
        ]
