module Internal.Tools.RationalOrder exposing (..)

{-|


# Rational order

The rational order module simulates the properties of the order of rational
numbers: all values have a clear direct ordering, but one can always gain a
new number in-between two existing numbers.

While this property is similarly achievable with floats, the Float type has a
precision limit and it is therefor more desirable to achieve the same property
using an Elm type that uses Int types for comparison.

Given the design of the order, the best case comparison design is O(1), and the
worst case comparison is O(log(n)). The worst case relies on recursively
creating two values a and b, create two new numbers in-between, and repeat.

-}

import Recursion exposing (base, recurse, recurseThen)


{-| The RationalOrder consists of two items: a number for ordering and a
tie-breaking next RationalOrder type for when two RationalOrders have the same
number.

When the next RationalOrder is Nothing, it should be considered -infinite.

-}
type RationalOrder
    = With Int (Maybe RationalOrder)


{-| Find a new value that comes after a given value. For optimization reasons,
this will find the nearest number at the highest level.
-}
after : RationalOrder -> RationalOrder
after (With i _) =
    With (i + 1) Nothing


{-| Find a new value that comes before a given value. For optimization reasons,
this will find the nearest number at the highest level.
-}
before : RationalOrder -> RationalOrder
before (With i _) =
    With (i - 1) Nothing


{-| Find a new value in-between two existing values. The inputs don't need to be
ordered.
-}
between : RationalOrder -> RationalOrder -> RationalOrder
between x y =
    Recursion.runRecursion
        (\orders ->
            case orders of
                ( Nothing, Nothing ) ->
                    base (With 0 Nothing)

                ( Just o1, Nothing ) ->
                    base (before o1)

                ( Nothing, Just o2 ) ->
                    base (before o2)

                ( Just ((With i1 n1) as o1), Just ((With i2 n2) as o2) ) ->
                    case Basics.compare i1 i2 of
                        EQ ->
                            recurseThen ( n1, n2 )
                                (base << With i1 << Maybe.Just)

                        LT ->
                            case compare (after o1) o2 of
                                LT ->
                                    base (after o1)

                                _ ->
                                    Maybe.map after n1
                                        |> Maybe.withDefault (With 0 Nothing)
                                        |> Maybe.Just
                                        |> With i1
                                        |> base

                        GT ->
                            case compare (after o2) o1 of
                                LT ->
                                    base (after o2)

                                _ ->
                                    Maybe.map after n2
                                        |> Maybe.withDefault (With 0 Nothing)
                                        |> Maybe.Just
                                        |> With i2
                                        |> base
        )
        ( Just x, Just y )


compare : RationalOrder -> RationalOrder -> Basics.Order
compare x y =
    Recursion.runRecursion
        (\( With i1 n1, With i2 n2 ) ->
            case ( Basics.compare i1 i2, n1, n2 ) of
                ( EQ, Just o1, Just o2 ) ->
                    recurse ( o1, o2 )

                ( EQ, Just _, Nothing ) ->
                    base GT

                ( EQ, Nothing, Just _ ) ->
                    base LT

                ( EQ, Nothing, Nothing ) ->
                    base EQ

                ( LT, _, _ ) ->
                    base LT

                ( GT, _, _ ) ->
                    base GT
        )
        ( x, y )


fromList : List Int -> Maybe RationalOrder
fromList =
    Recursion.runRecursion
        (\items ->
            case items of
                [] ->
                    base Nothing

                head :: tail ->
                    recurseThen tail (With head >> Maybe.Just >> base)
        )


toList : RationalOrder -> List Int
toList =
    Recursion.runRecursion
        (\(With i next) ->
            case next of
                Nothing ->
                    base [ i ]

                Just n ->
                    recurseThen n ((::) i >> base)
        )
