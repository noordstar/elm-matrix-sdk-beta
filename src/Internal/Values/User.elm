module Internal.Values.User exposing
    ( User, toString, fromString
    , localpart, domain
    , coder
    )

{-| The Matrix user is uniquely identified by their identifier. This User type
helps identify and safely handle these strings to transform them into meaningful
data types.


## User

@docs User, toString, fromString


## Divide

Matrix users are identified by their unique ID. In the Matrix API, this is a
string that looks as follows:

    @alice:example.org
     \---/ \---------/
       |        |
       |        |
    localpart  domain

Since the username is safely parsed, one can get these parts of the username.

@docs localpart, domain


## JSON

@docs coder

-}

import Internal.Config.Log exposing (log)
import Internal.Grammar.ServerName as ServerName
import Internal.Grammar.UserId as UserId
import Internal.Tools.Json as Json
import Parser as P


{-| The Matrix user represents a user across multiple Matrix rooms.
-}
type alias User =
    UserId.UserID


{-| Define a method to encode/decode Matrix users.
-}
coder : Json.Coder User
coder =
    Json.parser
        { name = "Username"
        , p =
            P.andThen
                (\name ->
                    P.succeed
                        ( name
                        , if UserId.isHistorical name then
                            [ log.warn "Historical user found"
                            ]

                          else
                            []
                        )
                )
                UserId.userIdParser
        , toString = UserId.toString
        }


{-| The domain represents the Matrix homeserver controlling this user. It also
offers other Matrix homeservers an indication of where to look if you wish to
send a message to this user.
-}
domain : User -> String
domain =
    .domain >> ServerName.toString


{-| Parse a string and convert it into a User, if formatted properly.
-}
fromString : String -> Maybe User
fromString =
    UserId.fromString


{-| The localpart is similar to a username, in the sense that every user has
their own localpart. The localpart is not unique across multiple servers,
however! There can be a user @alice:example.com and a user @alice:example.org in
a room at the same time.
-}
localpart : User -> String
localpart =
    .localpart


{-| Convert a user into its unique identifier string value.
-}
toString : User -> String
toString =
    UserId.toString
