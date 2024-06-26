module Internal.Grammar.UserId exposing
    ( UserID, toString, fromString
    , userIdParser, isHistorical
    )

{-|


# User ids

Users within Matrix are uniquely identified by their Matrix user ID. The user
ID is namespaced to the homeserver which allocated the account and has the form:

    @localpart:domain

The localpart of a user ID is an opaque identifier for that user. It MUST NOT
be empty, and MUST contain only the characters a-z, 0-9, ., \_, =, -, /, and +.

The domain of a user ID is the server name of the homeserver which allocated
the account.

The length of a user ID, including the @ sigil and the domain, MUST NOT exceed
255 characters.

The complete grammar for a legal user ID is:

    user_id = "@" user_id_localpart ":" server_name
    user_id_localpart = 1*user_id_char
    user_id_char = DIGIT
                 / %x61-7A                   ; a-z
                 / "-" / "." / "=" / "_" / "/" / "+"

Older versions of this specification were more tolerant of the characters
permitted in user ID localparts. There are currently active users whose user
IDs do not conform to the permitted character set, and a number of rooms whose
history includes events with a sender which does not conform. In order to
handle these rooms successfully, clients and servers MUST accept user IDs with
localparts from the expanded character set:

    extended_user_id_char = %x21-39 / %x3B-7E  ; all ASCII printing chars except :


## User ID

@docs UserID, toString, fromString


## Extra

@docs userIdParser, isHistorical

-}

import Internal.Grammar.ServerName as ServerName exposing (ServerName)
import Internal.Tools.ParserExtra as PE
import Parser as P exposing ((|.), (|=), Parser)


{-| The User ID type defining a user.
-}
type alias UserID =
    { localpart : String, domain : ServerName }


{-| Convert a Matrix User ID back into its uniquely identifying string.
-}
fromString : String -> Maybe UserID
fromString =
    P.run (userIdParser |. P.end) >> Result.toMaybe


{-| Return a boolean on whether a Matrix user has a historical user ID.
Since this user ID is not SUPPOSED to be legal but clients are nevertheless
forced to support them due to backwards compatibility, clients may occasionally
attempt to break the rules in an attempt to find undefined behaviour.

As a result, an explicit method to spot historical users is added to the SDK.

-}
isHistorical : UserID -> Bool
isHistorical { localpart } =
    String.any
        (\c ->
            let
                i : Int
                i =
                    Char.toCode c
            in
            not ((0x61 <= i && i <= 0x7A) || Char.isAlpha c)
        )
        localpart


localpartParser : Parser String
localpartParser =
    P.chompIf validHistoricalUsernameChar
        |> P.getChompedString
        |> PE.times 1 255
        |> P.map String.concat


{-| Convert a parsed User ID to a string.
-}
toString : UserID -> String
toString { localpart, domain } =
    String.concat [ "@", localpart, ":", ServerName.toString domain ]


{-| Parse a UserID from a string.
-}
userIdParser : Parser UserID
userIdParser =
    P.succeed UserID
        |. P.symbol "@"
        |= localpartParser
        |. P.symbol ":"
        |= ServerName.serverNameParser
        |> PE.maxLength 255


validHistoricalUsernameChar : Char -> Bool
validHistoricalUsernameChar c =
    let
        i : Int
        i =
            Char.toCode c
    in
    (0x21 <= i && i <= 0x39) || (0x3B <= i && i <= 0x7E)
