module Internal.Values.Envelope exposing
    ( Envelope, init
    , map, mapMaybe, mapList
    , Settings, mapSettings, extractSettings
    , mapContext
    , getContent, extract
    , EnvelopeUpdate(..), update
    , coder, encode, decoder
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


## Update

@docs EnvelopeUpdate, update


## JSON coders

@docs coder, encode, decoder

-}

import Internal.Api.Request as Request
import Internal.Config.Log exposing (Log)
import Internal.Config.Text as Text
import Internal.Tools.Hashdict as Hashdict
import Internal.Tools.Json as Json
import Internal.Tools.Timestamp exposing (Timestamp)
import Internal.Values.Context as Context exposing (AccessToken, Context, Versions)
import Internal.Values.Settings as Settings
import Recursion
import Recursion.Fold


{-| There are lots of different data types in the Elm SDK, and many of them
need the same values. The Envelope type wraps settings, tokens and values around
each data type so they can all enjoy those values without needing to explicitly
define them in their type.
-}
type alias Envelope a =
    { content : a
    , context : Context
    , settings : Settings
    }


{-| The Envelope update type helps update either the envelope or a content type.
-}
type EnvelopeUpdate a
    = ContentUpdate a
    | HttpRequest (Request.Request ( Request.Error, List Log ) ( EnvelopeUpdate a, List Log ))
    | More (List (EnvelopeUpdate a))
    | Optional (Maybe (EnvelopeUpdate a))
    | RemoveAccessToken String
    | RemovePasswordIfNecessary
    | SetAccessToken AccessToken
    | SetBaseUrl String
    | SetDeviceId String
    | SetNextBatch String
    | SetNow Timestamp
    | SetRefreshToken String
    | SetVersions Versions


{-| Settings value from
[Internal.Values.Settings](Internal-Values-Settings#Settings). Can be used to
manipulate the Matrix Vault.
-}
type alias Settings =
    Settings.Settings


{-| Define how an Envelope can be encoded to and decoded from a JSON object.
-}
coder : Json.Coder a -> Json.Coder (Envelope a)
coder c1 =
    Json.object3
        { name = Text.docs.envelope.name
        , description = Text.docs.envelope.description
        , init = Envelope
        }
        (Json.field.required
            { fieldName = "content"
            , toField = .content
            , description = Text.fields.envelope.content
            , coder = c1
            }
        )
        (Json.field.required
            { fieldName = "context"
            , toField = .context
            , description = Text.fields.envelope.context
            , coder = Context.coder
            }
        )
        (Json.field.optional.withDefault
            { fieldName = "settings"
            , toField = .settings
            , description = Text.fields.envelope.settings
            , coder = Settings.coder
            , default = Tuple.pair Settings.init []
            }
        )


{-| Decode an enveloped type from a JSON value. The decoder also imports any
potential tokens, values and settings included in the JSON.
-}
decoder : Json.Coder a -> Json.Decoder (Envelope a)
decoder c1 =
    Json.decode (coder c1)


{-| Encode an enveloped type into a JSON value. The function encodes all
non-standard settings, tokens and values.
-}
encode : Json.Coder a -> Json.Encoder (Envelope a)
encode c1 =
    Json.encode (coder c1)


{-| Map a function, then get its content. This is useful for getting information
from a data type inside an Envelope.

    type alias User =
        { name : String, age : Int }

    getName : Envelope User -> String
    getName =
        Envelope.extract .name

-}
extract : (a -> b) -> Envelope a -> b
extract f data =
    f data.content


{-| Map a function on the settings, effectively getting data that way.

This can be helpful if you have a UI that displays custom settings to a user.

-}
extractSettings : (Settings -> b) -> Envelope a -> b
extractSettings f data =
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
init : { serverName : String, content : a } -> Envelope a
init data =
    { content = data.content
    , context = Context.init data.serverName
    , settings = Settings.init
    }


{-| Map a function on the content of the Envelope.

    type alias User =
        { name : String, age : Int }

    getName : Envelope User -> Envelope String
    getName =
        Envelope.map .name

-}
map : (a -> b) -> Envelope a -> Envelope b
map f data =
    { content = f data.content
    , context = data.context
    , settings = data.settings
    }


{-| Update the Context in the Envelope.
-}
mapContext : (Context -> Context) -> Envelope a -> Envelope a
mapContext f data =
    { data | context = f data.context }


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
mapSettings f data =
    { data | settings = f data.settings }


toList : Envelope (List a) -> List (Envelope a)
toList data =
    List.map
        (\content -> map (always content) data)
        data.content


toMaybe : Envelope (Maybe a) -> Maybe (Envelope a)
toMaybe data =
    Maybe.map
        (\content -> map (always content) data)
        data.content


{-| Updates the Envelope with a given EnvelopeUpdate value.
-}
update : (au -> a -> a) -> EnvelopeUpdate au -> Envelope a -> Envelope a
update updateContent eu startData =
    Recursion.runRecursion
        (\updt ->
            case updt of
                ContentUpdate v ->
                    Recursion.base
                        (\data ->
                            { data | content = updateContent v data.content }
                        )

                HttpRequest _ ->
                    Recursion.base identity

                More items ->
                    Recursion.Fold.foldList (<<) identity items

                Optional (Just u) ->
                    Recursion.recurse u

                Optional Nothing ->
                    Recursion.base identity

                RemoveAccessToken token ->
                    Recursion.base
                        (\({ context } as data) ->
                            { data
                                | context =
                                    { context
                                        | accessTokens =
                                            Hashdict.removeKey token context.accessTokens
                                    }
                            }
                        )

                RemovePasswordIfNecessary ->
                    Recursion.base
                        (\({ context } as data) ->
                            if data.settings.removePasswordOnLogin then
                                { data | context = { context | password = Nothing } }

                            else
                                data
                        )

                SetAccessToken a ->
                    Recursion.base
                        (\({ context } as data) ->
                            { data | context = { context | accessTokens = Hashdict.insert a context.accessTokens } }
                        )

                SetBaseUrl b ->
                    Recursion.base
                        (\({ context } as data) ->
                            { data | context = { context | baseUrl = Just b } }
                        )

                SetDeviceId d ->
                    Recursion.base
                        (\({ context } as data) ->
                            { data | context = { context | deviceId = Just d } }
                        )

                SetNextBatch nextBatch ->
                    Recursion.base
                        (\{ context } as data ->
                            { data | context = { context | nextBatch = Just nextBatch } }
                        )
                
                SetNow n ->
                    Recursion.base
                        (\({ context } as data) ->
                            { data | context = { context | now = Just n } }
                        )

                SetRefreshToken r ->
                    Recursion.base
                        (\({ context } as data) ->
                            { data | context = { context | refreshToken = Just r } }
                        )

                SetVersions vs ->
                    Recursion.base
                        (\({ context } as data) ->
                            { data | context = { context | versions = Just vs } }
                        )
        )
        eu
        startData
