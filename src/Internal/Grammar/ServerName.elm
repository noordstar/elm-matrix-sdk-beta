module Internal.Grammar.ServerName exposing
    ( ServerName, toString, fromString
    , servernameParser
    )

{-|


# Server name

A homeserver is uniquely identified by its server name. The server name
represents the address at which the homeserver in question can be reached by
other homeservers.

@docs ServerName, toString, fromString


## Parser

@docs serverNameParser

-}

import Internal.Tools.ParserExtra as PE
import Parser as P exposing ((|.), (|=), Parser)


{-| The hostname is the location where the server can be found.

Notice how the Matrix spec specifies that the hostname can either be a DNS name,
an IPv4Address or an IPv6Address. Since the IPv4Address is compatible with the
specification of DNS names, however, and RFC1123 (section 2.1) does not require
a client to distinguish them, we treat IPv4Addresses like DNS names.

-}
type HostName
    = DNS String
    | IPv6 IPv6Address


{-| The IPv6Address is represented by a list of items BEFORE and AFTER the
double colons (::).
-}
type alias IPv6Address =
    { front : List String, back : List String }


{-| The server name is a combination of a hostname and an optional port.
-}
type ServerName
    = ServerName { host : HostName, port_ : Maybe Int }


{-| Parser for the DNS name record. The Matrix spec bases its grammar on the
standard for internet host names, as specified by RFC1123, section 2.1, with an
extension IPv6 literals.

    [RFC-1123 §2.2]

    The syntax of a legal Internet host name was specified in RFC-952
      [DNS:4].  One aspect of host name syntax is hereby changed: the
      restriction on the first character is relaxed to allow either a
      letter or a digit.  Host software MUST support this more liberal
      syntax.

    Host software MUST handle host names of up to 63 characters and
      SHOULD handle host names of up to 255 characters.

    [RFC-952 §Assumptions-1]

    A "name" (Net, Host, Gateway, or Domain name) is a text string up
      to 24 characters drawn from the alphabet (A-Z), digits (0-9), minus
      sign (-), and period (.).  Note that periods are only allowed when
      they serve to delimit components of "domain style names". (See
      RFC-921, "Domain Name System Implementation Schedule", for
      background).

-}
dnsNameParser : Parser String
dnsNameParser =
    P.chompIf Char.isAlphaNum
        |. P.chompWhile (\c -> Char.isAlphaNum c || c == '-' || c == '.')
        |> P.getChompedString


fromString : String -> Maybe ServerName
fromString s =
    P.run (servernameParser |. P.end) s
        |> (\out ->
                case out of
                    Ok _ ->
                        out

                    Err e ->
                        Debug.log "No parse" e
                            |> always (Debug.log "original" s)
                            |> always out
           )
        |> Result.toMaybe


{-| Parse a Hostname.
-}
hostnameParser : Parser HostName
hostnameParser =
    P.oneOf
        [ P.succeed IPv6
            |. P.symbol "["
            |= ipv6Parser
            |. P.symbol "]"
        , P.succeed DNS
            |= dnsNameParser
        ]


{-| Parse all values to the left of the double colon (::)
-}
ipv6LeftParser : Parser (List String)
ipv6LeftParser =
    P.oneOf
        [ P.succeed []
            |. P.symbol ":"
        , P.succeed (|>)
            |= PE.times 1 7 (ipv6NumParser |. P.symbol ":")
            |= P.oneOf
                [ P.succeed (\bottom tail -> tail ++ [ bottom ])
                    |= ipv6NumParser
                , P.succeed identity
                ]
        ]


{-| Parse an ordinary IPv6 number
-}
ipv6NumParser : Parser String
ipv6NumParser =
    P.chompIf Char.isHexDigit
        |> P.getChompedString
        |> PE.times 1 4
        |> P.map String.concat


{-| Parse an IPv6 Address
-}
ipv6Parser : Parser IPv6Address
ipv6Parser =
    ipv6LeftParser
        |> P.andThen
            (\front ->
                P.succeed (IPv6Address front)
                    |= ipv6RightParser (8 - List.length front)
            )


{-| Parse all values to the right of the double colon (::)
-}
ipv6RightParser : Int -> Parser (List String)
ipv6RightParser n =
    if n > 0 then
        P.succeed identity
            |. P.symbol ":"
            |= P.oneOf
                [ P.succeed (::)
                    |= ipv6NumParser
                    |= PE.times 0
                        (n - 1)
                        (P.succeed identity
                            |. P.symbol ":"
                            |= ipv6NumParser
                        )
                , P.succeed []
                ]

    else
        P.succeed []


{-| Convert an IPv6 address to a readable string format
-}
ipv6ToString : IPv6Address -> String
ipv6ToString { front, back } =
    (if List.length front == 8 then
        front

     else if List.length back == 8 then
        back

     else
        List.concat [ front, [ "" ], back ]
    )
        |> List.intersperse ":"
        |> String.concat


portParser : Parser Int
portParser =
    P.chompIf Char.isDigit
        |. P.chompWhile Char.isDigit
        |> P.getChompedString
        |> P.andThen
            (\v ->
                case String.toInt v of
                    Just i ->
                        if 0 <= i && i <= 2 ^ 16 - 1 then
                            P.succeed i

                        else
                            P.problem ("Port out of range: " ++ v)

                    Nothing ->
                        P.problem "Not a port number"
            )


servernameParser : Parser ServerName
servernameParser =
    P.succeed (\h p -> ServerName { host = h, port_ = p })
        |= hostnameParser
        |= P.oneOf
            [ P.succeed Just
                |. P.symbol ":"
                |= portParser
            , P.succeed Nothing
            ]


toString : ServerName -> String
toString (ServerName { host, port_ }) =
    let
        hostString : String
        hostString =
            case host of
                DNS name ->
                    name

                IPv6 { front, back } ->
                    (if List.length front == 8 then
                        List.intersperse ":" front

                     else if List.length back == 8 then
                        List.intersperse ":" back

                     else
                        List.concat
                            [ List.intersperse ":" front
                            , [ "::" ]
                            , List.intersperse ":" back
                            ]
                    )
                        |> String.concat
                        |> (\i -> "[" ++ i ++ "]")

        portString : String
        portString =
            port_
                |> Maybe.map String.fromInt
                |> Maybe.map ((++) ":")
                |> Maybe.withDefault ""
    in
    hostString ++ portString
