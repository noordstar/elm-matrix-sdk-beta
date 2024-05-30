module Matrix.Settings exposing
    ( setAccessToken, removeAccessToken
    , getDeviceName, setDeviceName
    , getSyncTime, setSyncTime
    )

{-| The Matrix Vault has lots of configurable variables that you rarely want to
interact with. Usually, you configure these variables only when creating a new
Vault, or when a user explicitly changes one of their preferred settings.


## Access token

The Vault is able to log in on its own, but sometimes you would rather have the
Vault use an access token than log in to get one on its own. For this case, you
can use this option to insert an access token into the Vault.

As long as the access token remains valid, the Vault will use this provided
access token.

@docs setAccessToken, removeAccessToken


## Device name

The default device name that is being communicated with the Matrix API.

This is mostly useful for users who are logged in with multiple sessions. They
will see device names like "Element for Android" or "Element on iOS". For the
Elm SDK, they will by default see the Elm SDK with its version included. If you
are writing a custom client, however, you are free to change this to something
more meaningful to the user.

@docs getDeviceName, setDeviceName


## Sync time

Whenever the Matrix API has nothing new to report, the Elm SDK is kept on
hold until something new happens. The `syncTime` indicates a timeout to how long
the Elm SDK tolerates being held on hold.

  - ↗️ A high value is good because it significantly reduces traffic between the
    user and the homeserver.
  - ↘️ A low value is good because it reduces the risk of
    the connection ending abruptly or unexpectedly.

Nowadays, most libraries use 30 seconds as the standard, as does the Elm SDK.
The value is in miliseconds, so it is set at 30,000.

@docs getSyncTime, setSyncTime

-}

import Internal.Values.Envelope as Envelope
import Types exposing (Vault(..))


{-| Insert a suggested access token.
-}
setAccessToken : String -> Vault -> Vault
setAccessToken token (Vault vault) =
    vault
        |> Envelope.mapContext
            (\c -> { c | suggestedAccessToken = Just token })
        |> Vault


{-| Remove an access token that has been inserted using the
[setAccessToken](Matrix-Settings#setAccessToken) function.

This should generally not be necessary, but it can be nice security-wise.

-}
removeAccessToken : Vault -> Vault
removeAccessToken (Vault vault) =
    vault
        |> Envelope.mapContext
            (\c -> { c | suggestedAccessToken = Nothing })
        |> Vault


{-| Determine the device name.
-}
getDeviceName : Vault -> String
getDeviceName (Vault vault) =
    Envelope.extractSettings .deviceName vault


{-| Override the device name.
-}
setDeviceName : String -> Vault -> Vault
setDeviceName name (Vault vault) =
    Vault <| Envelope.mapSettings (\s -> { s | deviceName = name }) vault


{-| Determine the sync timeout value.
-}
getSyncTime : Vault -> Int
getSyncTime (Vault vault) =
    Envelope.extractSettings .syncTime vault


{-| Override the sync timeout value.
-}
setSyncTime : Int -> Vault -> Vault
setSyncTime time (Vault vault) =
    Vault <| Envelope.mapSettings (\s -> { s | syncTime = max 1 time }) vault
