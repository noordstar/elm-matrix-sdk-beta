module Internal.Values.Settings exposing
    ( Settings, init
    , coder, encode, decoder
    )

{-|


# Settings

The Settings module exposes a data type to configure settings in the enveloped
data types.

@docs Settings, init


## JSON coders

@docs coder, encode, decoder

-}

import Internal.Config.Default as Default
import Internal.Config.Text as Text
import Internal.Tools.Json as Json


{-| Custom settings that can be manipulated by the user. These serve as a
configuration for how the Elm SDK should behave.

Custom settings are always part of the Envelope, allowing all functions to
behave under the user's preferred settings.

-}
type alias Settings =
    { currentVersion : String
    , deviceName : String
    , removePasswordOnLogin : Bool
    , syncTime : Int
    }


{-| Define how a Settings type can be encoded to and decoded from a JSON object.
-}
coder : Json.Coder Settings
coder =
    Json.object4
        { name = Text.docs.settings.name
        , description = Text.docs.settings.description
        , init = Settings
        }
        (Json.field.optional.withDefault
            { fieldName = "currentVersion"
            , toField = .currentVersion
            , description = Text.fields.settings.currentVersion
            , coder = Json.string
            , default = Tuple.pair Default.currentVersion []
            }
        )
        (Json.field.optional.withDefault
            { fieldName = "deviceName"
            , toField = .deviceName
            , description = Text.fields.settings.deviceName
            , coder = Json.string
            , default = Tuple.pair Default.deviceName []
            }
        )
        (Json.field.optional.withDefault
            { fieldName = "removePasswordOnLogin"
            , toField = .removePasswordOnLogin
            , description = Text.fields.settings.removePasswordOnLogin
            , coder = Json.bool
            , default = Tuple.pair Default.removePasswordOnLogin []
            }
        )
        (Json.field.optional.withDefault
            { fieldName = "syncTime"
            , toField = .syncTime
            , description = Text.fields.settings.syncTime
            , coder = Json.int
            , default = Tuple.pair Default.syncTime []
            }
        )


{-| Decode settings from a JSON value.
-}
decoder : Json.Decoder Settings
decoder =
    Json.decode coder


{-| Encode the settings into a JSON value.
-}
encode : Json.Encoder Settings
encode =
    Json.encode coder


{-| Create a new Settings module based on default values
-}
init : Settings
init =
    { currentVersion = Default.currentVersion
    , deviceName = Default.deviceName
    , removePasswordOnLogin = Default.removePasswordOnLogin
    , syncTime = Default.syncTime
    }
