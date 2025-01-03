module Internal.Config.Text exposing
    ( docs, failures, fields, mappings, logs, parses
    , accessTokenFoundLocally, accessTokenExpired, accessTokenInvalid
    , versionsFoundLocally, versionsReceived, versionsFailedToDecode
    , unsupportedVersionForEndpoint
    , decodedDictSize, invalidHashInHashdict, invalidHashInMashdict, leakingValueFound
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


## Type documentation

@docs docs, failures, fields, mappings, logs, parses


## API Authentication

Messages sent as API logs during the authentication phase of the API
interaction.

@docs accessTokenFoundLocally, accessTokenExpired, accessTokenInvalid

offers room for translation, re-wording and refactors.


## API Versions

Messages sent as API logs while the Elm SDK is figuring out how modern the
homeserver is and how it can best communicate.

@docs versionsFoundLocally, versionsReceived, versionsFailedToDecode


## API miscellaneous messages

Messages sent as API logs during communication with the API.

@docs unsupportedVersionForEndpoint


## JSON decoder

Messages sent as API logs when a JSON value is being decoded.

@docs decodedDictSize, invalidHashInHashdict, invalidHashInMashdict, leakingValueFound

-}


type alias Desc =
    List String


type alias TypeDocs =
    { name : String, description : Desc }


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


{-| Logs when the JSON decoder detects that an imported dictionary contained
duplicate keys.
-}
decodedDictSize : Int -> Int -> String
decodedDictSize from to =
    String.concat
        [ "JSON dict contained duplicate keys (JSON had "
        , String.fromInt from
        , " keys, Elm dict has "
        , String.fromInt to
        , " keys)"
        ]


{-| Documentation used for all functions and data types in JSON coders
-}
docs :
    { accessToken : TypeDocs
    , context : TypeDocs
    , envelope : TypeDocs
    , event : TypeDocs
    , hashdict : TypeDocs
    , ibatch : TypeDocs
    , invite : TypeDocs
    , inviteEvent : TypeDocs
    , itoken : TypeDocs
    , mashdict : TypeDocs
    , room : TypeDocs
    , settings : TypeDocs
    , stateManager : TypeDocs
    , strippedEvent : TypeDocs
    , timeline : TypeDocs
    , timelineFilter : TypeDocs
    , unsigned : TypeDocs
    , vault : TypeDocs
    , versions : TypeDocs
    }
docs =
    { accessToken =
        { name = "Access Token"
        , description =
            [ "The Access Token type stores information about an access token - its value, when it expires, and how one may get a new access token when the current value expires."
            ]
        }
    , context =
        { name = "Context"
        , description =
            [ "The Context is the set of variables that the user (mostly) cannot control."
            , "The Context contains tokens, values and other bits that the Vault receives from the Matrix API."
            ]
        }
    , envelope =
        { name = "Envelope"
        , description =
            [ "The Envelope module wraps existing data types with lots of values and settings that can be adjusted manually."
            ]
        }
    , event =
        { name = "Event"
        , description =
            [ "The Event type represents a single value that contains all the information for a single event in the room."
            ]
        }
    , hashdict =
        { name = "Hashdict"
        , description =
            [ "This allows you to store values based on an externally defined identifier."
            , "For example, the hashdict can store events and use their event id as their key."
            ]
        }
    , ibatch =
        { name = "IBatch"
        , description =
            [ "The internal batch tracks a patch of events on the Matrix timeline."
            ]
        }
    , invite =
        { name = "Invite"
        , description =
            [ "The Invite type represents an invitation to join a Matrix room."
            ]
        }
    , inviteEvent =
        { name = "InviteEvent"
        , description =
            [ "The InviteEvent type represents a state event that can be previewed of a room that the user has been invited to."
            ]
        }
    , itoken =
        { name = "IToken"
        , description =
            [ "The IToken connects batches in the timeline and maintains relative order."
            ]
        }
    , mashdict =
        { name = "Mashdict"
        , description =
            [ "The mashdict exclusively stores values for which the hashing algorithm returns a value, and it ignores the outcome for all other scenarios."
            ]
        }
    , room =
        { name = "Room"
        , description =
            [ "The Room type represents a conversation in Matrix."
            ]
        }
    , settings =
        { name = "Settings"
        , description =
            [ "The settings type is a data type to configure settings in the enveloped data type."
            ]
        }
    , stateManager =
        { name = "StateManager"
        , description =
            [ "The StateManager tracks the room state based on events, their event types and the optional state keys they provide."
            , "Instead of making the user loop through the room's timeline of events, the StateManager offers the user a dictionary-like experience to navigate through the Matrix room state."
            ]
        }
    , strippedEvent =
        { name = "StrippedEvent"
        , description =
            [ "The StrippedEvent is a simplified Matrix event that contains no metadata."
            ]
        }
    , timeline =
        { name = "Timeline"
        , description =
            [ "The Timeline tracks events and orders them in a simple way for the user to view them."
            ]
        }
    , timelineFilter =
        { name = "Timeline Filter"
        , description =
            [ "The Timeline Filter allows the user to be very specific about which events they're interested in."
            ]
        }
    , unsigned =
        { name = "Unsigned Data"
        , description =
            [ "Unsigned data is optional data that might come along with the event."
            , "This information is often supportive but not necessary to the context."
            ]
        }
    , vault =
        { name = "Vault"
        , description =
            [ "Main type storing all relevant information from the Matrix API."
            ]
        }
    , versions =
        { name = "Versions"
        , description =
            [ "Versions type describing the supported spec versions and MSC properties."
            ]
        }
    }


{-| Description of all edge cases where a JSON decoder can fail.
-}
failures : { hashdict : Desc, listWithOne : String, mashdict : Desc }
failures =
    { hashdict =
        [ "Not all values map to their respected hash with the given hash function."
        ]
    , listWithOne = "Expected at least one value in the list - zero found."
    , mashdict =
        [ "Not all values map to their respected hash with the given hash function."
        ]
    }


{-| Objects contain multiple fields. These fields are here described, explaining
what they do and what they are for.
-}
fields :
    { accessToken :
        { created : Desc
        , expiryMs : Desc
        , lastUsed : Desc
        , refresh : Desc
        , value : Desc
        }
    , context :
        { accessToken : Desc
        , baseUrl : Desc
        , deviceId : Desc
        , experimental : Desc
        , nextBatch : Desc
        , now : Desc
        , password : Desc
        , refreshToken : Desc
        , username : Desc
        , serverName : Desc
        , suggestedAccessToken : Desc
        , transaction : Desc
        , user : Desc
        , versions : Desc
        }
    , envelope :
        { content : Desc
        , context : Desc
        , settings : Desc
        }
    , event :
        { content : Desc
        , eventId : Desc
        , originServerTs : Desc
        , roomId : Desc
        , sender : Desc
        , stateKey : Desc
        , eventType : Desc
        , unsigned : Desc
        }
    , ibatch :
        { end : Desc
        , events : Desc
        , filter : Desc
        , start : Desc
        }
    , iddict :
        { cursor : Desc
        , dict : Desc
        }
    , invite :
        { events : Desc
        , roomId : Desc
        }
    , itoken :
        { behind : Desc
        , ends : Desc
        , inFrontOf : Desc
        , name : Desc
        , starts : Desc
        }
    , room :
        { accountData : Desc
        , ephemeral : Desc
        , events : Desc
        , roomId : Desc
        , state : Desc
        , timeline : Desc
        }
    , settings :
        { currentVersion : Desc
        , deviceName : Desc
        , presence : Desc
        , removePasswordOnLogin : Desc
        , syncTime : Desc
        }
    , timeline :
        { batches : Desc
        , events : Desc
        , filledBatches : Desc
        , mostRecentBatch : Desc
        , tokens : Desc
        }
    , timelineFilter :
        { senders : Desc
        , sendersAllowOthers : Desc
        , types : Desc
        , typesAllowOthers : Desc
        }
    , unsigned :
        { age : Desc
        , membership : Desc
        , prevContent : Desc
        , redactedBecause : Desc
        , transactionId : Desc
        }
    , vault :
        { accountData : Desc
        , invites : Desc
        , nextBatch : Desc
        , rooms : Desc
        , user : Desc
        }
    , versions :
        { unstableFeatures : Desc
        , versions : Desc
        }
    }
fields =
    { accessToken =
        { created =
            [ "Timestamp of when the access token was received." ]
        , expiryMs =
            [ "Given time in milliseconds of when the access token might expire." ]
        , lastUsed =
            [ "Timestamp of when the access token was last used." ]
        , refresh =
            [ "Refresh token used to gain a new access token." ]
        , value =
            [ "Secret access token value." ]
        }
    , context =
        { accessToken =
            [ "The access token used for authentication with the Matrix server."
            ]
        , baseUrl =
            [ "The base URL of the Matrix server."
            ]
        , deviceId =
            [ "The reported device ID according to the API."
            ]
        , experimental =
            [ "Experimental features supported by the homeserver."
            ]
        , nextBatch =
            [ "The batch token to supply in the since param of the next /sync request."
            ]
        , now =
            [ "The most recently found timestamp."
            ]
        , password =
            [ "The user's password for authentication purposes."
            ]
        , refreshToken =
            [ "The token used to obtain a new access token upon expiration of the current access token."
            ]
        , suggestedAccessToken =
            [ "An access token provided with no context by the user."
            ]
        , username =
            [ "The username of the Matrix account."
            ]
        , serverName =
            [ "The homeserver that the user is trying to communicate with."
            , "This name doesn't need to be the address. For example, the name might be `matrix.org` even though the homeserver is at a different location."
            ]
        , transaction =
            [ "A unique identifier for a transaction initiated by the user."
            ]
        , user =
            [ "The Matrix user the Vault is representing."
            ]
        , versions =
            [ "The versions of the Matrix protocol that are supported by the server."
            ]
        }
    , envelope =
        { content =
            [ "The actual data or payload that is wrapped within the envelope."
            ]
        , context =
            [ "The context information associated with the envelope, such as environment or session details."
            , "In general, this data cannot be directly configured by the user."
            ]
        , settings =
            [ "The configurable settings that affect how the enveloped data is handled or processed."
            ]
        }
    , event =
        { content =
            [ "The body of this event, as created by the client which sent it."
            ]
        , eventId =
            [ "The globally unique identifier for this event."
            ]
        , originServerTs =
            [ "Timestamp (in milliseconds since the unix epoch) on originating homeserver when this event was sent."
            ]
        , roomId =
            [ "The ID of the room associated with this event."
            ]
        , sender =
            [ "Contains the fully-qualified ID of the user who sent this event."
            ]
        , stateKey =
            [ "Present if, and only if, this event is a state event. The key making this piece of state unique in the room. Note that it is often an empty string."
            , "State keys starting with an @ are reserved for referencing user IDs, such as room members. With the exception of a few events, state events set with a given user’s ID as the state key MUST only be set by that user."
            ]
        , eventType =
            [ "The type of the event."
            ]
        , unsigned =
            [ "Contains optional extra information about the event."
            ]
        }
    , ibatch =
        { end =
            [ "Pointer to the token that ends the internal batch."
            ]
        , events =
            [ "List of event IDs contained within the internal batch."
            ]
        , filter =
            [ "Filter that indicates how strictly the homeserver has selected when resulting into the given list of events."
            ]
        , start =
            [ "Pointer to the token that starts the internal batch."
            ]
        }
    , iddict =
        { cursor =
            [ "To ensure uniqueness of all keys and to prevent the usage of keys that were previously assigned to older values, the iddict tracks which is the smallest non-negative integer that hasn't been used yet."
            ]
        , dict =
            [ "Dictionary that contains all values stored in the iddict."
            ]
        }
    , invite =
        { events =
            [ "State events that render a preview of the room that the user is invited to."
            ]
        , roomId =
            [ "Room ID of the room that the invite is to."
            ]
        }
    , itoken =
        { behind =
            [ "This token is behind all tokens in this field."
            ]
        , ends =
            [ "This token is in front of the batches in this field."
            ]
        , inFrontOf =
            [ "This token is ahead of all tokens in this field."
            ]
        , name =
            [ "Opaque value provided by the homeserver."
            ]
        , starts =
            [ "This token is at the start of the batches in this field."
            ]
        }
    , room =
        { accountData =
            [ "Room account data tracking the user's private storage about this room." ]
        , ephemeral =
            [ "Ephemeral events that were sent recently in this room."
            ]
        , events =
            [ "Database containing events that were sent in this room." ]
        , roomId =
            [ "Unique room identifier" ]
        , state =
            [ "Current state of the room based on state events" ]
        , timeline =
            [ "Current timeline of the room" ]
        }
    , settings =
        { currentVersion =
            [ "Indicates the current version of the Elm SDK."
            ]
        , deviceName =
            [ "Indicates the device name that is communicated to the Matrix API."
            ]
        , presence =
            [ "Controls whether the client is automatically marked as online. The value is passed on to the Matrix API."
            ]
        , removePasswordOnLogin =
            [ "Remove the password as soon as a valid access token has been received."
            ]
        , syncTime =
            [ "Indicates the frequency in miliseconds with which the Elm SDK should long-poll the /sync endpoint."
            ]
        }
    , timeline =
        { batches =
            [ "Dictionary storing all event batches in the timeline."
            ]
        , events =
            [ "Mapping that allows us to quickly zoom in on an event."
            ]
        , filledBatches =
            [ "Counter that tracks how many batches are kept by the timeline."
            , "Batches are only counted if they are filled by at least one event."
            ]
        , mostRecentBatch =
            [ "Tracks the most recent batch that was sent by the homeserver - usually through `/sync`"
            ]
        , tokens =
            [ "Index of all the tokens used to connect event batches on the timeline."
            ]
        }
    , timelineFilter =
        { senders =
            [ "A list of senders that is considered an exception to the infinite pool of \"other\" users"
            ]
        , sendersAllowOthers =
            [ "Value that determines whether the infinite pool of others is included."
            , "If False, only the users mentioned in `senders` are included. If True, then all users who aren't mentioned in `senders` are included."
            ]
        , types =
            [ "A list of event types that is considered an exception to the infinite pool of \"other\" event types."
            ]
        , typesAllowOthers =
            [ "Value that determines whether the infinite pool of others is included."
            , "If False, only the event types mentioned in `types` are included. If True, then all users who aren't mentioned in `types` are included."
            ]
        }
    , unsigned =
        { age =
            [ "The time in milliseconds that has elapsed since the event was sent. This field is generated by the local homeserver, and may be incorrect if the local time on at least one of the two servers is out of sync, which can cause the age to either be negative or greater than it actually is."
            ]
        , membership =
            [ "The room membership of the user making the request, at the time of the event."
            ]
        , prevContent =
            [ "The previous content for this event. This field is generated by the local homeserver, and is only returned if the event is a state event, and the client has permission to see the previous content."
            ]
        , redactedBecause =
            [ "The event that redacted this event, if any."
            ]
        , transactionId =
            [ "The client-supplied transaction ID, for example, provided via PUT /_matrix/client/v3/rooms/{roomId}/send/{eventType}/{txnId}, if the client being given the event is the same one which sent it."
            ]
        }
    , vault =
        { accountData =
            [ "The account's global private data."
            ]
        , invites =
            [ "Collection of invites that the user has received."
            ]
        , nextBatch =
            [ "The next batch that can be used to sync with the Matrix API."
            ]
        , rooms =
            [ "Directory of joined rooms that the user is a member of."
            ]
        , user =
            [ "User that the Vault is logging in as."
            ]
        }
    , versions =
        { unstableFeatures =
            [ "Unstable features such as experimental MSCs that are supported by a homeserver."
            ]
        , versions =
            [ "Spec versions supported by a homeserver." ]
        }
    }


{-| This message will be shown when a [Hashdict](Internal-Tools-Hashdict)
encounters a hash-value pair where the value does not hash to the provided hash.
-}
invalidHashInHashdict : String
invalidHashInHashdict =
    "Invalid hash function: not all elements hash to their JSON-stored hashes"


{-| This message will be shown when a [Mashdict](Internal-Tools-Mashdict)
encounters a hash-value pair where the value does not hash to the provided hash.
-}
invalidHashInMashdict : String
invalidHashInMashdict =
    "Invalid hash function: not all elements hash to their JSON-stored hashes"


{-| The Elm SDK occassionally uses [leaking values](Internal-Config-Leaks),
which might indicate exceptional behaviour. As such, this log is sent when one
of those leaking values is found: to alert the user that something fishy might
be going on.
-}
leakingValueFound : String -> String
leakingValueFound leaking_value =
    "Found leaking value : " ++ leaking_value


{-| These logs might appear during a process where something unexpected has
happened. Most of these unexpected results, are taken account of by the Elm SDK,
but logged so that the programmer can do something about it.
-}
logs :
    { baseUrlFailed : String -> String
    , baseUrlFound : String -> String -> String
    , getEventId : String -> String
    , getNow : Int -> String
    , httpRequest : String -> String -> String
    , invitedUser : String -> String -> String
    , keyIsNotAnInt : String -> String
    , loggedInAs : String -> String
    , sendEvent : Maybe String -> String
    , serverReturnedInvalidJSON : String -> String
    , serverReturnedUnknownJSON : String -> String
    , syncAccountDataFound : Int -> String
    }
logs =
    { baseUrlFailed =
        (++) "Failed to find .well-known, using default server address: "
    , baseUrlFound =
        \url baseUrl ->
            String.concat [ "Found baseURL of ", url, " at address ", baseUrl ]
    , getEventId = (++) "Received event with id = "
    , getNow =
        \now ->
            String.concat
                [ "Identified current time at Unix time "
                , String.fromInt now
                ]
    , httpRequest =
        \method url -> String.concat [ "Matrix HTTP: ", method, " ", url ]
    , invitedUser =
        \userId roomId ->
            String.concat [ "Invited user ", userId, " to room ", roomId ]
    , keyIsNotAnInt =
        \key ->
            String.concat
                [ "Encountered a key `"
                , key
                , "` that cannot be converted to an Int"
                ]
    , loggedInAs =
        \username ->
            String.concat [ "Successfully logged in as user ", username ]
    , sendEvent =
        \eventId ->
            case eventId of
                Just e ->
                    "Sent event, received event id " ++ e

                Nothing ->
                    "Sent event, event id not known - make sure to check transaction id"
    , serverReturnedInvalidJSON = (++) "The server returned invalid JSON: "
    , serverReturnedUnknownJSON = (++) "The server returned JSON that doesn't seem to live up to spec rules: "
    , syncAccountDataFound =
        \n -> String.concat [ "Found ", String.fromInt n, " account data updates" ]
    }


{-| Function descriptions
-}
mappings : { itokenPTR : TypeDocs }
mappings =
    { itokenPTR =
        { name = "ITokenPTR init"
        , description =
            [ "Converts an optional string to an Itoken pointer."
            ]
        }
    }


{-| Logs for issues that might be found while parsing strings into meaningful data.
-}
parses :
    { historicalUserId : String -> String
    , reservedIPs :
        { ipv6Toipv4 : String
        , multicast : String
        , futureUse : String
        , unspecified : String
        }
    }
parses =
    { historicalUserId = \name -> "Found a historical username `" ++ name ++ "`."
    , reservedIPs =
        { ipv6Toipv4 = "Detected a reserved ip address that is formerly used as an IPv6 to IPv4 relay. It is unlikely that this IP Address is real."
        , multicast = "Detected a reserved ip address that is used for multicasting. It is unlikely that this IP Address is real."
        , futureUse = "Detected a reserves ip address that is reserved for future use. It is unlikely that this IP Address is real if you're running a recent version of the Elm SDK."
        , unspecified = "This is an unspecified ip address. It is unlikely that this IP Address is real and someone might try to break something."
        }
    }


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
