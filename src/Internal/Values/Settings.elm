module Internal.Values.Settings exposing
    ( Settings, init
    , encode, decoder
    )

{-|


# Settings

The Settings module exposes a data type to configure settings in the enveloped
data types.

@docs Settings, init


## JSON coders

@docs encode, decoder

-}

import Internal.Config.Default as Default
import Internal.Tools.Decode as D
import Internal.Tools.Encode as E
import Json.Decode as D
import Json.Encode as E


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


{-| Decode settings from a JSON value.
-}
decoder : D.Decoder Settings
decoder =
    D.map3 Settings
        (D.opFieldWithDefault "currentVersion" Default.currentVersion D.string)
        (D.opFieldWithDefault "deviceName" Default.deviceName D.string)
        (D.opFieldWithDefault "syncTime" Default.syncTime D.int)


{-| Encode the settings into a JSON value.
-}
encode : Settings -> E.Value
encode settings =
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


{-| Create a new Settings module based on default values
-}
init : Settings
init =
    { currentVersion = Default.currentVersion
    , deviceName = Default.deviceName
    , syncTime = Default.syncTime
    }
