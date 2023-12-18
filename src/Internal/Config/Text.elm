module Internal.Config.Text exposing
    ( versionsFoundLocally, versionsReceived, versionsFailedToDecode
    , accessTokenFoundLocally, accessTokenExpired, accessTokenInvalid
    , unsupportedVersionForEndpoint
    )

{-| Throughout the Elm SDK, there are lots of pieces of text being used for
various purposes. Some of these are:

  - To log what is happening during an API call.
  - To fail with custom decoder errors.
  - To describe custom values in a human readable format.

All magic values of text are gathered in this module, to form a monolithic
source of text. This allows people to learn more about the Elm SDK, and it
offers room for future translations.

Optionally, developers can even consider taking the values of some of these
variables to interpret them automatically when they appear as logs on the other
side. This could be used to automatically detect when the Vault is failing to
authenticate, for example, so that a new login screen can be shown. **WARNING:**
This is a risky feature, keep in mind that even a patch update might break this!
You should only do this if you know what you're doing.


## API Versions

Messages sent as API logs while the Elm SDK is figuring out how modern the
homeserver is and how it can best communicate.

@docs versionsFoundLocally, versionsReceived, versionsFailedToDecode


## API Authentication

Messages sent as API logs during the authentication phase of the API
interaction.

@docs accessTokenFoundLocally, accessTokenExpired, accessTokenInvalid

offers room for translation, re-wording and refactors.


## API miscellaneous messages

Messages sent as API logs during communication with the API.

@docs unsupportedVersionForEndpoint

-}


{-| Logs when the Matrix API returns that an access token is no longer valid.
-}
accessTokenExpired : String
accessTokenExpired =
    "Matrix API reports access token as no longer valid"


{-| Logs when the Vault has an access token that is still (locally) considered
valid.
-}
accessTokenFoundLocally : String
accessTokenFoundLocally =
    "Found locally cached access token"


{-| Logs when the Matrix API rejects an access token without explicitly
mentioning a reason.
-}
accessTokenInvalid : String
accessTokenInvalid =
    "Matrix API rejected access token as invalid"


{-| The Matrix homeserver can specify how it wishes to communicate, and the Elm
SDK aims to communicate accordingly. This may fail in some scenarios, however,
in which case it will throw this error.

Most of the time, the error is caused by one of two options:

1.  The homeserver is very archaic and does not (yet) support API endpoints that
    are nowadays considered mature.

2.  The homeserver is much more modern than the Elm SDK and either uses
    exclusively API endpoints that the Elm SDK doesn't (yet) support, or it uses
    spec versions that aren't considered "official" Matrix spec versions and
    were designed by a third party.

-}
unsupportedVersionForEndpoint : String
unsupportedVersionForEndpoint =
    "This Matrix homeserver and the Elm SDK do not share a common spec version for this endpoint"


{-| Occasionally, the Matrix homeserver fails to communicate how it is best
communicated with. Most of the time, this means that the homeserver is somehow
unreachable or some gateway error has occured.
-}
versionsFailedToDecode : String
versionsFailedToDecode =
    "Matrix API returned an invalid version list"


{-| Logs when the Vault remembers how to communicate with the Matrix homeserver.
-}
versionsFoundLocally : String
versionsFoundLocally =
    "Found locally cached version list"


{-| Logs when the Matrix API has returned how to best communicate with them.
-}
versionsReceived : String
versionsReceived =
    "Matrix API returned a version list"
