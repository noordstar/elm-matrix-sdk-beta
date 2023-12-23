module Internal.Values.Envelope exposing
    ( Envelope(..), init
    , map, mapMaybe, mapList
    , Settings, mapSettings, extractSettings
    , mapContext
    , getContent, extract
    , encode, decoder
    )

{-| The Envelope module wraps existing data types with lots of values and
settings that can be adjusted manually.


## Create

@docs Envelope, init


## Manipulate

@docs map, mapMaybe, mapList


## Settings

@docs Settings, mapSettings, extractSettings


## Context

@docs mapContext


## Extract

@docs getContent, extract


## JSON coders

@docs encode, decoder

-}

import Internal.Config.Default as Default
import Internal.Tools.Decode as D
import Internal.Tools.Encode as E
import Internal.Values.Context as Context exposing (Context)
import Json.Decode as D
import Json.Encode as E


{-| There are lots of different data types in the Elm SDK, and many of them
need the same values. The Envelope type wraps settings, tokens and values around
each data type so they can all enjoy those values without needing to explicitly
define them in their type.
-}
type Envelope a
    = Envelope
        { content : a
        , context : Context
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


{-| Decode an enveloped type from a JSON value. The decoder also imports any
potential tokens, values and settings included in the JSON.
-}
decoder : D.Decoder a -> D.Decoder (Envelope a)
decoder xDecoder =
    D.map3 (\a b c -> Envelope { content = a, context = b, settings = c })
        (D.field "content" xDecoder)
        (D.field "context" Context.decoder)
        (D.field "settings" decoderSettings)


{-| Decode settings from a JSON value.
-}
decoderSettings : D.Decoder Settings
decoderSettings =
    D.map3 Settings
        (D.opFieldWithDefault "currentVersion" Default.currentVersion D.string)
        (D.opFieldWithDefault "deviceName" Default.deviceName D.string)
        (D.opFieldWithDefault "syncTime" Default.syncTime D.int)


{-| Encode an enveloped type into a JSON value. The function encodes all
non-standard settings, tokens and values.
-}
encode : (a -> E.Value) -> Envelope a -> E.Value
encode encodeX (Envelope data) =
    E.object
        [ ( "content", encodeX data.content )
        , ( "context", Context.encode data.context )
        , ( "settings", encodeSettings data.settings )
        , ( "version", E.string Default.currentVersion )
        ]


{-| Encode the settings into a JSON value.
-}
encodeSettings : Settings -> E.Value
encodeSettings settings =
    let
        differentFrom : b -> b -> Maybe b
        differentFrom defaultValue currentValue =
            if currentValue == defaultValue then
                Nothing

            else
                Just currentValue
    in
    E.maybeObject
        [ ( "currentVersion"
          , settings.currentVersion
                |> differentFrom Default.currentVersion
                |> Maybe.map E.string
          )
        , ( "deviceName"
          , settings.deviceName
                |> differentFrom Default.deviceName
                |> Maybe.map E.string
          )
        , ( "syncTime"
          , settings.syncTime
                |> differentFrom Default.syncTime
                |> Maybe.map E.int
          )
        ]


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


{-| Map a function on the settings, effectively getting data that way.

This can be helpful if you have a UI that displays custom settings to a user.

-}
extractSettings : (Settings -> b) -> Envelope a -> b
extractSettings f (Envelope data) =
    f data.settings


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
        , context = Context.init
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
        , context = data.context
        , settings = data.settings
        }


{-| Update the Context in the Envelope.
-}
mapContext : (Context -> Context) -> Envelope a -> Envelope a
mapContext f (Envelope data) =
    Envelope
        { content = data.content
        , context = f data.context
        , settings = data.settings
        }


{-| Map the contents of a function, where the result is wrapped in a `List`
type. This can be useful when you are mapping to a list of individual values
that you would all like to see enveloped.

    type alias User =
        { name : String, age : Int }

    type alias Company =
        { name : String, employees : List User }

    getEmployees : Envelope Company -> List (Envelope User)
    getEmployees envelope =
        mapList .employees envelope

-}
mapList : (a -> List b) -> Envelope a -> List (Envelope b)
mapList f =
    map f >> toList


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
        , context = data.context
        , settings = f data.settings
        }


toList : Envelope (List a) -> List (Envelope a)
toList (Envelope data) =
    List.map
        (\content -> map (always content) (Envelope data))
        data.content


toMaybe : Envelope (Maybe a) -> Maybe (Envelope a)
toMaybe (Envelope data) =
    Maybe.map
        (\content -> map (always content) (Envelope data))
        data.content
