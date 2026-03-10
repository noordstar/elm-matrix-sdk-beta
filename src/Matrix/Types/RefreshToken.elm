module Matrix.Types.RefreshToken exposing
    ( RefreshToken, fromString, toString
    , encode, decoder
    )

{-| Refresh tokens.

Refresh tokens can be exchanged for new access tokens without asking the user
to log in again. Treat them as secrets.


## RefreshToken

@docs RefreshToken, fromString, toString


## JSON

@docs encode, decoder

-}

import Json.Decode as D
import Json.Encode as E


{-| A token used to request a new access token.
-}
type RefreshToken
    = RefreshToken String


{-| Encode a refresh token as a JSON string.

    RefreshToken.encode token

-}
encode : RefreshToken -> E.Value
encode =
    toString >> E.string


{-| Decode a refresh token from a JSON string.

    import Json.Decode as D
    import Matrix.Types.RefreshToken as RefreshToken

    D.decodeString RefreshToken.decoder "\"refresh\""
    -- Ok <refreshToken>

-}
decoder : D.Decoder RefreshToken
decoder =
    D.map fromString D.string


{-| Create a refresh token from its raw string value.

    RefreshToken.fromString "refresh"

-}
fromString : String -> RefreshToken
fromString =
    RefreshToken


{-| Extract the raw string value from a refresh token.

Avoid logging this value.

-}
toString : RefreshToken -> String
toString (RefreshToken s) =
    s
