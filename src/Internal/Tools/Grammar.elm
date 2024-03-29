module Internal.Tools.Grammar exposing (..)

{-|


# Identifier Grammar

The specification defines
[some identifiers](https://spec.matrix.org/v1.9/appendices/#identifier-grammar)
to use the Common Namespaced Identifier Grammar. This is a common grammar
intended for non-user-visible identifiers, with a defined mechanism for
implementations to create new identifiers.

This module documents those identifiers, allowing the Elm SDK to use them.

-}

import Parser as P exposing (Parser)


{-| Parse an IPv6 address
-}
ipv6addressParser : Parser String
ipv6addressParser =
    P.chompWhile validIPv6Char
        |> P.getChompedString
        |> P.andThen
            (\out ->
                if String.length out > 45 then
                    P.problem "an ipv6 address has no more than 45 digits"

                else if String.length out < 2 then
                    P.problem "an ipv6 address has at least 2 digits"

                else
                    -- TODO: ipv6 has more specific rules
                    -- https://datatracker.ietf.org/doc/html/rfc3513#section-2.2
                    P.succeed out
            )


{-| Parse a port value
-}
portParser : Parser Int
portParser =
    P.chompWhile Char.isDigit
        |> P.getChompedString
        |> P.andThen
            (\out ->
                if String.length out > 5 then
                    P.problem "a port has no more than 5 digits"

                else if String.length out < 1 then
                    P.problem "a port has at least 1 digit"

                else
                    case String.toInt out of
                        Nothing ->
                            P.problem "Expected port int"

                        Just i ->
                            P.succeed i
            )


{-| Check whether a char is a valid IPv6char
-}
validIPv6char : Char -> Bool


validIPv6Char c =
    "0123456789ABCDEFabcdef:."
        |> String.toList
        |> List.member c
