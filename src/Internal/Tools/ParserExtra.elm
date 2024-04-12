module Internal.Tools.ParserExtra exposing (..)

import Parser as P exposing ((|.), (|=), Parser)


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


oneOrMore : Parser a -> Parser (List a)
oneOrMore parser =
    P.succeed (::)
        |= parser
        |= zeroOrMore parser


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


exactly : Int -> Parser a -> Parser (List a)
exactly n =
    times n n


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
