module Internal.Tools.Decode exposing (Decoder)
{-| # Advanced security Json.Decode

This module extends the standard JSON encode / decode library for security
measures. Most Elm libraries do not access an API this often without insight
for the user, and hence this module aims to offer the user more insight into
what is going on.

Additionally, the decoder will warn for suspicious values, and provide helpful
errors when the JSON fails to decode.

## Primitives

@docs Decoder
-}

import Json.Decode as D

type Decoder a = D.Decoder { value : a, messages : List String }