module Internal.Api.Sync.V2 exposing (..)

{-|


# Sync V1

Given the complexity of the /sync endpoint, the JSON coders have been placed
in separate modules. Version 2 provides a valid JSON coder for the following
spec versions:

  - r0.4.0 : <https://spec.matrix.org/legacy/client_server/r0.3.0.html#get-matrix-client-r0-sync>

-- @docs coder

-}
import Internal.Tools.StrippedEvent as StrippedEvent exposing (StrippedEvent)
import Internal.Api.Sync.V1 as V1
import Internal.Tools.Json as Json
import Internal.Tools.Timestamp as Timestamp exposing (Timestamp)
import Internal.Values.User as User exposing (User)
import Internal.Values.Event as Event

type alias RoomEvent =
    { content : Json.Value
    , eventId : String
    , originServerTs : Timestamp
    , sender : User
    , stateKey : Maybe String
    , eventType : String
    , unsigned : Maybe Unsigned
    }


roomEventCoder : Json.Coder RoomEvent
roomEventCoder =
    Json.object7
        { name = "Event"
        , description =
            [ "Event describing a JSON object sent in a room."
            ]
        , init = RoomEvent
        }
        (Json.field.required
            { fieldName = "content"
            , toField = .content
            , description =
                [ "The content of this event. The fields in this object will vary depending on the type of event."
                ]
            , coder = Json.value
            }
        )
        (Json.field.required
            { fieldName = "event_id"
            , toField = .eventId
            , description =
                [ "The ID of this event, if applicable."
                ]
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "origin_server_ts"
            , toField = .originServerTs
            , description =
                [ "Timestamp in milliseconds on originating homeserver when this event was sent."
                ]
            , coder = Timestamp.coder
            }
        )
        (Json.field.required
            { fieldName = "sender"
            , toField = .sender
            , description =
                [ "The MXID of the user who sent this event."
                ]
            , coder = User.coder
            }
        )
        (Json.field.optional.value
            { fieldName = "state_key"
            , toField = .stateKey
            , description =
                [ "This key will only be present for state events. A unique key which defines the overwriting semantics for this piece of room state."
                ]
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "type"
            , toField = .eventType
            , description =
                [ "The type of event."
                ]
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "unsigned"
            , toField = .unsigned
            , description =
                [ "Information about this event which was not sent by the originating homeserver"
                ]
            , coder = unsignedCoder
            }
        )


roomEventToUpdate : String -> RoomEvent -> Event.Event
roomEventToUpdate roomId e =
    -- TODO: Use Recursion module for call stack safety
    { content = e.content
    , eventId = e.eventId
    , originServerTs = e.originServerTs
    , roomId = roomId
    , sender = e.sender
    , stateKey = e.stateKey
    , eventType = e.eventType
    , unsigned =
        Maybe.map
            (\(Unsigned u) ->
                Event.UnsignedData
                    { age = Just u.age
                    , transactionId = u.transactionId
                    , redactedBecause = Maybe.map (roomEventToUpdate roomId) u.redactedBecause
                    , prevContent = u.prevContent
                    }
            )
            e.unsigned
    }

type Unsigned
    = Unsigned { age : Int, prevContent : Maybe Json.Value, redactedBecause : Maybe RoomEvent, transactionId : Maybe String }


unsignedCoder : Json.Coder Unsigned
unsignedCoder =
    Json.object4
        { name = "Unsigned"
        , description =
            [ "Information about the event that doesn't originate from the original homeserver."
            ]
        , init = \a b c d -> Unsigned { age = a, prevContent = b, redactedBecause = c, transactionId = d }
        }
        (Json.field.required
            { fieldName = "age"
            , toField = \(Unsigned u) -> u.age
            , description =
                [ "Time in milliseconds since the event was sent." ]
            , coder = Json.int
            }
        )
        (Json.field.optional.value -- Officially not supported in this spec version, but hey, what gives to try and look for it?
            { fieldName = "prev_content"
            , toField = \(Unsigned u) -> u.prevContent
            , description =
                [ "The previous content for this state. This will be present only for state events appearing in the timeline. If this is not a state event, or there is no previous content, this key will be missing."
                ]
            , coder = Json.value
            }
        )
        (Json.field.optional.value
            { fieldName = "redacted_because"
            , toField = \(Unsigned u) -> u.redactedBecause
            , description =
                [ "The event that redacted this event, if any."
                ]
            , coder = Json.lazy (\() -> roomEventCoder)
            }
        )
        (Json.field.optional.value
            { fieldName = "transaction_id"
            , toField = \(Unsigned u) -> u.transactionId
            , description =
                [ "The transaction ID set when this message was sent. This key will only be present for message events sent by the device calling this API."
                ]
            , coder = Json.string
            }
        )

type alias Presence = { events : List Event }

presenceCoder : Json.Coder Presence
presenceCoder =
    Json.object1
        { name = "Presence"
        , description =
            [ "Events indicating users' presence"
            ]
        , init = Presence
        }
        (Json.field.required
            { fieldName = "events"
            , toField = .events
            , description =
                [ "List of events"
                ]
            , coder = Json.list eventCoder
            }
        )

type alias AccountData = { events : List Event }

accountDataCoder : Json.Coder AccountData
accountDataCoder =
        Json.object1
        { name = "AccountData"
        , description =
            [ "Account data events sent by the user in a given room using a different client."
            ]
        , init = AccountData
        }
        (Json.field.required
            { fieldName = "events"
            , toField = .events
            , description =
                [ "List of events"
                ]
            , coder = Json.list eventCoder
            }
        )

type alias Event = StrippedEvent

eventCoder : Json.Coder Event
eventCoder = StrippedEvent.coder
