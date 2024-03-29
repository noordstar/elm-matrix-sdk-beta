module Internal.Config.Log exposing (Log, log)

{-|


# Logs

The logs module exposes various log types that can be used to indicate logs.
This helps users filter for the logs that they care about.

@docs Log, log

The logs are encoded as strings as to allow the addition of new log types
without triggering a major update.

-}

-- @docs caughtError, debug, error, info, securityWarn, warn


{-| Common pattern for a log message. The log message consists of a log channel
like `debug`, `warn`, `error`, etc. and the content of the message.

These logs are completely optional: they can be ignored, they can be sent to the
console, or a dialog may be created that presents the log messages.

-}
type alias Log =
    { channel : String, content : String }


{-| Create a log message of various log types.
-}
log :
    { caughtError : String -> Log
    , debug : String -> Log
    , error : String -> Log
    , info : String -> Log
    , securityWarn : String -> Log
    , warn : String -> Log
    }
log =
    { caughtError = Log caughtError
    , debug = Log debug
    , error = Log error
    , info = Log info
    , securityWarn = Log securityWarn
    , warn = Log warn
    }


{-| A caught error is an error that has been caught elsewhere in the code, hence
functioning as a secondary debug channel.
-}
caughtError : String
caughtError =
    "caught-error"


{-| Debug logs are logs that can be used to debug API interactions.
-}
debug : String
debug =
    "debug"


{-| Error strings indicate that something unexpected has happened. As a result,
something has stopped working.
-}
error : String
error =
    "error"


{-| Info contains relevant info for the user
-}
info : String
info =
    "info"


{-| Security warnings are warnings that contain red flags.

Of course, the Elm SDK is not aware of any security vulnerabilities that it
contains, but it can raise a user's attention to suspicious situations.

For example, if the homeserver returns room ids that do not look like usernames
at all, the homeserver can raise a security warning, which indicates that:

1.  The homeserver might be bugged
2.  The Elm SDK might be severaly outdated
3.  The homeserver might be compromised and/or trying to attack the Elm SDK

-}
securityWarn : String
securityWarn =
    "security-warn"


{-| Warning logs are logs that are unusual, but that can be dealt with. Warnings
are debug logs that are out of the ordinary.
-}
warn : String
warn =
    "warn"
