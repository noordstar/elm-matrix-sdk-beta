module Matrix.Types.AccessToken exposing
    ( AccessToken, fromString, toString
    , decoder
    )

{-| Access tokens.

An access token identifies your client as a logged-in Matrix user when talking
to a homeserver. Treat it like a password.


## AccessToken

@docs AccessToken, fromString, toString


## JSON

@docs decoder

-}

import Json.Decode as D


{-| A token that lets a client act on behalf of a user account.

You usually get this from the login API or local secure storage.

-}
type AccessToken
    = AccessToken String


{-| Decode an access token from JSON.

    import Json.Decode as D
    import Matrix.Types.AccessToken as AccessToken

    D.decodeString AccessToken.decoder "\"s3cr3t\""
    -- Ok <accessToken>

-}
decoder : D.Decoder AccessToken
decoder =
    D.map fromString D.string


{-| Create an access token from a raw string.

    AccessToken.fromString "s3cr3t"

This is useful when you load a token from secure local storage.

-}
fromString : String -> AccessToken
fromString =
    AccessToken


{-| Convert an access token back to a raw string.

Only use this when you must send it in an HTTP `Authorization` header.

Note: Avoid logging access tokens.

-}
toString : AccessToken -> String
toString (AccessToken s) =
    s
