module Internal.Config.Leaks exposing
    ( accessToken, baseUrl, field, transaction, versions
    , allLeaks
    )

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

@docs accessToken, baseUrl, field, transaction, versions

For safety purposes, all leaking values are stored in the following value:

@docs allLeaks

-}

import Set exposing (Set)


{-| Placeholder access token.
-}
accessToken : String
accessToken =
    "elm-sdk-placeholder-access-token-leaks"


{-| Complete set of all leaking values. Commonly using for testing purposes.
-}
allLeaks : Set String
allLeaks =
    Set.fromList
        [ accessToken
        , baseUrl
        , field
        , transaction
        , "elm-sdk-placeholder-versions-leaks" -- Old leaking value
        ]
        |> Set.union (Set.fromList versions.versions)
        |> Set.union versions.unstableFeatures


{-| Placeholder base URL.
-}
baseUrl : String
baseUrl =
    "elm-sdk-placeholder-baseurl-leaks.example.org"


{-| Placeholder JSON field.
-}
field : String
field =
    "elm-sdk-placeholder-json-field"


{-| Placeholder transaction id.
-}
transaction : String
transaction =
    "elm-sdk-placeholder-transaction-leaks"


{-| Placeholder versions list.
-}
versions : { versions : List String, unstableFeatures : Set String }
versions =
    { versions = [ "elm-sdk-placeholder-versions-versions-leaks" ]
    , unstableFeatures = Set.singleton "elm-sdk-placeholder-versions-unstableFeatures-leaks"
    }
