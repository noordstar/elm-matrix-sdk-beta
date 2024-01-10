module Internal.Config.Phantom exposing (PString(..), PInt(..), PBool(..), PList(..))

{-|


# Phantom types

This module contains a lot of phantom types that do not necessarily do anything,
but they force the compiler to create an error whenever something illegal is
done.

Compiler errors may seem annoying, they can help you write good code. In a
functional programming language like Elm, the trick is to design your code in
such a way that if it compiles, it works. Phantom types can help you do so.

The phantom types in this module help you in the following way:

1.  They can help force an compile to fault when you forget to run a function.

2.  They can help track values for security.


## Standard data types

@docs PString, PInt, PBool, PList

-}


{-| Opaque type that encapsulates a bool.
-}
type PBool ph
    = PBool Bool


{-| Opaque type that encapsulates an int.
-}
type PInt ph
    = PInt Int


{-| Opaque type that encapsulates a list.
-}
type PList ph a
    = PList (List a)


{-| Opaque type that encapsulates a string.
-}
type PString ph
    = PString String
