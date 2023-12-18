module Internal.Config.Leaks exposing (accessToken)

{-|


# Leaks module

The Elm compiler is quite picky when it comes to handling edge cases, which may
occasionally result in requiring us to insert values in impossible states.

This module offers placeholders for those times. The placeholder values are
intentionally called "leaks", because they should be used carefully: a wrongful
implementation might cause unexpected behaviour, vulnerabilities or even
security risks!

You should not use this module unless you know what you're doing. That is:

  - By exclusively using leaking values in opaque types so a user cannot
    accidentally reach an impossible state
  - By exclusively using leaking values in cases where the compiler is the only
    reason that the leaking value needs to be used
  - By exclusively using leaking values if there is no way to circumvent the
    compiler with a reasonable method.

One such example would be to turn an `Maybe Int` into an `Int` if you already
know 100% sure that the value isn't `Nothing`.

    Just 5 |> Maybe.withDefault Leaks.number

@docs accessToken

-}


{-| Placeholder access token.
-}
accessToken : String
accessToken =
    "elm-sdk-placeholder-access-token-leaks"
