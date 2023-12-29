module Internal.Config.Default exposing
    ( currentVersion, deviceName
    , syncTime
    )

{-| This module hosts all default settings and configurations that the Vault
will assume until overriden by the user.


## Version management

@docs currentVersion, deviceName


## Communication config

@docs syncTime

-}


{-| The version that is being communicated to the user
-}
currentVersion : String
currentVersion =
    "beta 2.1.0"


{-| The default device name that is being communicated with the Matrix API.

This is mostly useful for users who are logged in with multiple sessions.

-}
deviceName : String
deviceName =
    "Elm SDK (" ++ currentVersion ++ ")"


{-| Whenever the Matrix API has nothing new to report, the Elm SDK is kept on
hold until something new happens. The `syncTime` indicates a timeout to how long
the Elm SDK tolerates being held on hold.

  - ↗️ A high value is good because it significantly reduces traffic between the
    user and the homeserver.
  - ↘️ A low value is good because it reduces the risk of
    the connection ending abruptly or unexpectedly.

Nowadays, most libraries use 30 seconds as the standard, as does the Elm SDK.
The value is in miliseconds, so it is set at 30,000.

-}
syncTime : Int
syncTime =
    30 * 1000
