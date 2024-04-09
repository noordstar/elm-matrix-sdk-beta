module Internal.Grammar.UserId exposing (..)

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

-}

import Internal.Grammar.ServerName as ServerName exposing (ServerName)
import Internal.Tools.ParserExtra as PE
import Parser as P exposing ((|.), (|=), Parser)


type UserID
    = UserID { localpart : String, domain : ServerName }


fromString : String -> Maybe UserID
fromString =
    P.run userIdParser >> Result.toMaybe


localpartParser : Parser String
localpartParser =
    P.chompIf validHistoricalUsernameChar
        |> P.getChompedString
        |> PE.times 1 255
        |> P.map String.concat


toString : UserID -> String
toString (UserID { localpart, domain }) =
    String.concat [ "@", localpart, ":", ServerName.toString domain ]


userIdParser : Parser UserID
userIdParser =
    P.succeed (\l d -> UserID { localpart = l, domain = d })
        |. P.symbol "@"
        |= localpartParser
        |. P.symbol ":"
        |= ServerName.servernameParser


validHistoricalUsernameChar : Char -> Bool
validHistoricalUsernameChar c =
    let
        i : Int
        i =
            Char.toCode c
    in
    (0x21 <= i && i <= 0x39) || (0x3B <= i && i <= 0x7E)
