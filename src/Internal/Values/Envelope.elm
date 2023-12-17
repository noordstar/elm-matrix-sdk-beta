module Internal.Values.Envelope exposing
    ( Envelope, init
    , map, mapMaybe
    , Settings, mapSettings
    , getContent, extract
    )

{-| The Envelope module wraps existing data types with lots of values and
settings that can be adjusted manually.


## Create

@docs Envelope, init


## Manipulate

@docs map, mapMaybe


## Settings

@docs Settings, mapSettings


## Extract

@docs getContent, extract

-}

import Internal.Config.Default as Default


{-| There are lots of different data types in the Elm SDK, and many of them
need the same values. The Envelope type wraps settings, tokens and values around
each data type so they can all enjoy those values without needing to explicitly
define them in their type.
-}
type Envelope a
    = Envelope
        { content : a
        , settings : Settings
        }


{-| Custom settings that can be manipulated by the user. These serve as a
configuration for how the Elm SDK should behave.

Custom settings are always part of the Envelope, allowing all functions to
behave under the user's preferred settings.

-}
type alias Settings =
    { currentVersion : String
    , deviceName : String
    , syncTime : Int
    }


{-| Map a function, then get its content. This is useful for getting information
from a data type inside an Envelope.

    type alias User =
        { name : String, age : Int }

    getName : Envelope User -> String
    getName =
        Envelope.extract .name

-}
extract : (a -> b) -> Envelope a -> b
extract f (Envelope data) =
    f data.content


{-| Get the original item that is stored inside an Envelope.

Make sure that you're only using this if you're interested in the actual value!
If you'd like to get the content, run a function on it, and put it back in an
Envelope, consider using [map](#map) instead.

-}
getContent : Envelope a -> a
getContent =
    extract identity


{-| Create a new enveloped data type. All settings are set to default values
from the [Internal.Config.Default](Internal-Config-Default) module.
-}
init : a -> Envelope a
init x =
    Envelope
        { content = x
        , settings =
            { currentVersion = Default.currentVersion
            , deviceName = Default.deviceName
            , syncTime = Default.syncTime
            }
        }


{-| Map a function on the content of the Envelope.

    type alias User =
        { name : String, age : Int }

    getName : Envelope User -> Envelope String
    getName =
        Envelope.map .name

-}
map : (a -> b) -> Envelope a -> Envelope b
map f (Envelope data) =
    Envelope
        { content = f data.content
        , settings = data.settings
        }


{-| Update the settings in the Envelope.

    setDeviceName : String -> Envelope a -> Envelope a
    setDeviceName name envelope =
        mapSettings
            (\settings ->
                { settings | deviceName = name }
            )
            envelope

-}
mapSettings : (Settings -> Settings) -> Envelope a -> Envelope a
mapSettings f (Envelope data) =
    Envelope
        { content = data.content
        , settings = f data.settings
        }


{-| Map the contents of a function, where the result is wrapped in a `Maybe`
type. This can be useful when you are not guaranteed to find the value you're
looking for.

    type alias User =
        { name : String, age : Int }

    type alias UserDatabase =
        List User

    getFirstUser : Envelope UserDatabase -> Maybe (Envelope User)
    getFirstUser envelope =
        mapMaybe List.head envelope

-}
mapMaybe : (a -> Maybe b) -> Envelope a -> Maybe (Envelope b)
mapMaybe f =
    map f >> toMaybe


toMaybe : Envelope (Maybe a) -> Maybe (Envelope a)
toMaybe (Envelope data) =
    Maybe.map
        (\content -> map (always content) (Envelope data))
        data.content
