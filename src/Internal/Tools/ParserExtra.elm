module Internal.Tools.ParserExtra exposing (zeroOrMore, oneOrMore, exactly, atLeast, atMost, times, maxLength)

{-|


# Extra parsers

To help the Elm SDK with parsing complex text values, this modules offers a few functions.

@docs zeroOrMore, oneOrMore, exactly, atLeast, atMost, times, maxLength

-}

import Parser as P exposing ((|.), (|=), Parser)


{-| Parses an item zero or more times. The result is combined into a list.
-}
zeroOrMore : Parser a -> Parser (List a)
zeroOrMore parser =
    P.loop []
        (\tail ->
            P.oneOf
                [ P.succeed (\head -> P.Loop (head :: tail))
                    |= parser
                , P.succeed (P.Done (List.reverse tail))
                ]
        )


{-| Parses an item at least once, but up to any number of times.
The result is combined into a list.
-}
oneOrMore : Parser a -> Parser (List a)
oneOrMore parser =
    P.succeed (::)
        |= parser
        |= zeroOrMore parser


{-| Parses an item at least a given number of times, but up to any number.
The result is combined into a list.
-}
atLeast : Int -> Parser a -> Parser (List a)
atLeast n parser =
    P.loop []
        (\tail ->
            if List.length tail < n then
                P.succeed (\head -> P.Loop (head :: tail))
                    |= parser

            else
                P.oneOf
                    [ P.succeed (\head -> P.Loop (head :: tail))
                        |= parser
                    , P.succeed (P.Done (List.reverse tail))
                    ]
        )


{-| Parses an item any number of times (can be zero), but does not exceed a
given number of times.
The result is combined into a list.
-}
atMost : Int -> Parser a -> Parser (List a)
atMost n parser =
    P.loop []
        (\tail ->
            if List.length tail < n then
                P.oneOf
                    [ P.succeed (\head -> P.Loop (head :: tail))
                        |= parser
                    , P.succeed (P.Done (List.reverse tail))
                    ]

            else
                P.succeed (P.Done (List.reverse tail))
        )


{-| Parses an item a given number of times, ranging from the given minimum up
to the given maximum.
The result is combined into a list.
-}
times : Int -> Int -> Parser a -> Parser (List a)
times inf sup parser =
    let
        low : Int
        low =
            max 0 (min inf sup)

        high : Int
        high =
            max 0 sup
    in
    P.loop []
        (\tail ->
            if List.length tail < low then
                P.succeed (\head -> P.Loop (head :: tail))
                    |= parser

            else if List.length tail < high then
                P.oneOf
                    [ P.succeed (\head -> P.Loop (head :: tail))
                        |= parser
                    , P.succeed (P.Done (List.reverse tail))
                    ]

            else
                P.succeed (P.Done (List.reverse tail))
        )


{-| Repeat pasing an item an exact number of times.
The result is combined into a list.
-}
exactly : Int -> Parser a -> Parser (List a)
exactly n =
    times n n


{-| After having parsed the item, make sure that the parsed text has not
exceeded a given length. If so, the parser fails.

This modification can be useful if a text has a maximum length requirement -
for example, usernames on Matrix cannot have a length of over 255 characters.

-}
maxLength : Int -> Parser a -> Parser a
maxLength n parser =
    P.succeed
        (\start value end ->
            if abs (end - start) > n then
                P.problem "Parsed too much text!"

            else
                P.succeed value
        )
        |= P.getOffset
        |= parser
        |= P.getOffset
        |> P.andThen identity
