module Matrix.User exposing
    ( User, toString
    , localpart, domain
    , get
    )

{-| Matrix users are identified by their unique ID. In the Matrix API, this is a
string that looks as follows:

    @alice:example.org
     \---/ \---------/
       |        |
       |        |
    localpart  domain

Since it is very easy to abuse Matrix user IDs to sneak in arbitrary values,
the Elm SDK parses them and makes sure they are safe. As a result, you might
need this module to get the right information from a user!


## User

@docs User, toString


## Info

Sometimes, you are more interested in the username itself. These functions can
help you decipher, disambiguate and categorize users based on their username.

@docs localpart, domain


## Manipulate

@docs get

-}

import Internal.Values.Envelope as Envelope
import Internal.Values.User as Internal
import Types exposing (User(..))


{-| The User type represents a Matrix user.

It contains information like:

  - Their username on Matrix
  - The server that hosts their account
  - Access tokens needed to talk to the server

It does **NOT** contain information like:

  - Their nickname
  - Their profile picture
  - Your private room with them

You can get all that information by looking it up in the [Vault](Matrix#Vault).

**Note:** Please do not store this user type as a variable in your model! You
should always maintain a single source of truth in Elm, and the User type
contains various credentials and API tokens that might expire if you don't
update them from the Vault.

If you need to remember specific users, you can best compare their identifying
string using [toString](Matrix-User#toString) or you can use
[get](Matrix-User#get) with the Vault to get the user type.

-}
type alias User =
    Types.User


{-| The domain is the name of the server that the user connects to. Server names
are case-sensitive, so if the strings are equal, the users are on the same
server!

As a result, you can use the user domain for:

  - When multiple users in a room have the same localpart on different servers
  - Finding other users from a potentially malicious homeserver
  - Counting homeservers in a room

See the following examples:

    domain (get vault "@alice:example.org") -- "example.org"

    domain (get vault "@bob:127.0.0.1") -- "127.0.0.1"

    domain (get vault "@charlie:[2001:db8::]") -- "[2001:db8::]"

-}
domain : User -> String
domain (User user) =
    Envelope.extract Internal.domain user


{-| Get a specific user by their unique identifier.

The Vault is needed as an input because the `User` type also stores various
credentials needed to talk to the Matrix API.

    get vault "@alice:example.org" -- Just (User "alice" "example.org")

    get vault "@bob:127.0.0.1" -- Just (User "bob" "127.0.0.1")

    get vault "@charlie:[2001:db8::]" -- Just (User "charlie" "2001:db8::")

    get vault "@evil:#mp#ss#bl#.c#m" -- Nothing

    get vault "" -- Nothing

-}
get : Types.Vault -> String -> Maybe User
get (Types.Vault vault) username =
    Envelope.mapMaybe (\_ -> Internal.fromString username) vault
        |> Maybe.map Types.User


{-| The localpart is the user's unique username. Every homeserver has their own
username registry, so you might occasionally find distinct users with the same
localpart.

The localpart is often used as a user's name in a room if they haven't set up
a custom name.

See the following examples:

    localpart (get vault "@alice:example.org") -- "alice"

    localpart (get vault "@bob:127.0.0.1") -- "bob"

    localpart (get vault "@charlie:[2001:db8::]") -- "charlie"

-}
localpart : User -> String
localpart (User user) =
    Envelope.extract Internal.localpart user


{-| Get the uniquely identifying string for this user. Since the strings are
case-sensitive, you can run a simple string comparison to compare usernames.
-}
toString : User -> String
toString (User user) =
    Envelope.extract Internal.toString user
