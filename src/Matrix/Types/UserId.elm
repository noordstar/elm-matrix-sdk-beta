module Matrix.Types.UserId exposing
    ( UserId, fromString, toString
    , encode, decoder
    , localpart, domain, isHistorical
    )

{-| Matrix user identifiers.

A Matrix user ID uniquely identifies a user across the whole Matrix network.
It is a string that looks like this:

    @alice:example.org
     \---/ \---------/
       |        |
       |        |
    localpart  domain

You will usually receive user IDs from JSON or from user input. This module
helps you validate and split them without dealing with the details every time.


## UserId

@docs UserId, fromString, toString


## JSON

@docs encode, decoder


## Info

@docs localpart, domain, isHistorical

-}

import Internal.Grammar.ServerName as ServerName
import Internal.Grammar.UserId as Grammar
import Json.Decode as D
import Json.Encode as E


{-| The ID of a Matrix user.
-}
type UserId
    = UserId Grammar.UserID


{-| Encode a user ID as a JSON string.

This is handy when you are building request bodies.

-}
encode : UserId -> E.Value
encode =
    toString >> E.string


{-| Decode a user ID from a JSON string.

This decoder validates the string, so malformed IDs fail immediately.

    import Json.Decode as D
    import Matrix.Types.UserId as UserId

    D.decodeString UserId.decoder "\"@alice:example.org\""
    -- Ok <userId>

    D.decodeString UserId.decoder "\"alice\""
    -- Err ...

If you need custom errors, decode as a `String` first and call `fromString`.

-}
decoder : D.Decoder UserId
decoder =
    D.string
        |> D.andThen
            (\raw ->
                case fromString raw of
                    Just userId ->
                        D.succeed userId

                    Nothing ->
                        D.fail "Invalid Matrix user ID"
            )


{-| Create a user ID from its raw string value.

This is the safest entry point for user input.

    fromString "@alice:example.org"
    -- Just <userId>

    fromString "alice"
    -- Nothing

You can convert back to a string with `toString` when you need to send it to
the Matrix API.

-}
fromString : String -> Maybe UserId
fromString =
    Grammar.fromString >> Maybe.map UserId


{-| Extract the raw string value from a user ID.

    toString userId
    -- "@alice:example.org"

Use this when you must include the ID in a URL or JSON payload.

-}
toString : UserId -> String
toString (UserId userId) =
    Grammar.toString userId


{-| Get the localpart of a user ID.

The localpart is the user name within a homeserver. Two users can have the
same localpart if they are on different servers.

    localpart userId
    -- "alice"

-}
localpart : UserId -> String
localpart (UserId userId) =
    userId.localpart


{-| Get the domain of a user ID.

The domain is the homeserver that hosts the user account. Server names are
case-sensitive, so exact equality is meaningful.

    domain userId
    -- "example.org"

-}
domain : UserId -> String
domain (UserId userId) =
    ServerName.toString userId.domain


{-| Report whether this user ID uses a historical character set.

Older Matrix user IDs allowed a wider set of ASCII characters in the localpart.
Clients still accept these user IDs for compatibility.

    isHistorical userId
    -- False

Note: You probably want to accept historical IDs, but you may wish to show a
warning or extra confirmation.

-}
isHistorical : UserId -> Bool
isHistorical (UserId userId) =
    Grammar.isHistorical userId
