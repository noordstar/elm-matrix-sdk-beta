module Internal.Tools.EncodeExtra exposing (maybeObject)

{-|


# Encode module

This module contains helper functions that help decode JSON.


# Optional body object

@docs maybeObject

-}

import Json.Encode as E


{-| Create a body object based on optionally provided values.

In other words, the following two variables create the same JSON value:

    value1 : Json.Encode.Value
    value1 =
        maybeObject
            [ ( "name", Just (Json.Encode.string "Alice") )
            , ( "age", Nothing )
            , ( "height", Just (Json.Encode.float 1.61) )
            , ( "weight", Nothing )
            ]

    value2 : Json.Encode.Value
    value2 =
        Json.Encode.object
            [ ( "name", Json.Encode.string "Alice" )
            , ( "height", Json.Encode.float 1.61 )
            ]

-}
maybeObject : List ( String, Maybe E.Value ) -> E.Value
maybeObject =
    List.filterMap
        (\( name, value ) ->
            case value of
                Just v ->
                    Just ( name, v )

                _ ->
                    Nothing
        )
        >> E.object
