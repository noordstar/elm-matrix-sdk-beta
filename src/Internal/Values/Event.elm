module Internal.Values.Event exposing
    ( Event
    , UnsignedData(..), age, prevContent, redactedBecause, transactionId
    , coder, encode, decoder
    )

{-|


# Event

The `Event` module hosts all the information for a single event in the timeline
of a room.

@docs Event


## Unsigned data

@docs UnsignedData, age, prevContent, redactedBecause, transactionId


## JSON Coder

@docs coder, encode, decoder

-}

import Internal.Config.Text as Text
import Internal.Tools.Json as Json
import Internal.Tools.Timestamp as Timestamp exposing (Timestamp)


{-| The Event type occurs everywhere on a user's timeline.
-}
type alias Event =
    { content : Json.Value
    , eventId : String
    , originServerTs : Timestamp
    , roomId : String
    , sender : String
    , stateKey : Maybe String
    , eventType : String
    , unsigned : Maybe UnsignedData
    }


{-| Unsigned Data contains a lot of extra information. You can access it through
helper functions.
-}
type UnsignedData
    = UnsignedData
        { age : Maybe Int
        , prevContent : Maybe Json.Value
        , redactedBecause : Maybe Event
        , transactionId : Maybe String
        }


{-| Get the event's age, if at all provided by the homeserver.
-}
age : Event -> Maybe Int
age event =
    Maybe.andThen (\(UnsignedData data) -> data.age) event.unsigned


{-| Define how an Event can be encoded to and decoded from a JSON object.
-}
coder : Json.Coder Event
coder =
    Json.object8
        { name = Text.docs.event.name
        , description = Text.docs.event.description
        , init = Event
        }
        (Json.field.required
            { fieldName = "content"
            , toField = .content
            , description = Text.fields.event.content
            , coder = Json.value
            }
        )
        (Json.field.required
            { fieldName = "eventId"
            , toField = .eventId
            , description = Text.fields.event.eventId
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "originServerTs"
            , toField = .originServerTs
            , description = Text.fields.event.originServerTs
            , coder = Timestamp.coder
            }
        )
        (Json.field.required
            { fieldName = "roomId"
            , toField = .roomId
            , description = Text.fields.event.roomId
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "sender"
            , toField = .sender
            , description = Text.fields.event.sender
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "stateKey"
            , toField = .stateKey
            , description = Text.fields.event.stateKey
            , coder = Json.string
            }
        )
        (Json.field.required
            -- NOTE! | In JSON we call it `type`, not `eventType`,
            -- NOTE! | so that the data is easier to read for other non-Elm
            -- NOTE! | JSON parsers
            { fieldName = "type"
            , toField = .eventType
            , description = Text.fields.event.eventType
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "unsigned"
            , toField = .unsigned
            , description = Text.fields.event.unsigned
            , coder = unsignedCoder
            }
        )


{-| Decode an Event from a JSON value.
-}
decoder : Json.Decoder Event
decoder =
    Json.decode coder


{-| Encode an Event into a JSON value.
-}
encode : Json.Encoder Event
encode =
    Json.encode coder


{-| Determine the previous `content` value for this event. This field is only a
`Just value` if the event is a state event, and the Matrix Vault has permission
to see the previous content.
-}
prevContent : Event -> Maybe Json.Value
prevContent event =
    Maybe.andThen (\(UnsignedData data) -> data.prevContent) event.unsigned


{-| If the event has been redacted, the homeserver can display the event that
redacted it here.
-}
redactedBecause : Event -> Maybe Event
redactedBecause event =
    Maybe.andThen (\(UnsignedData data) -> data.redactedBecause) event.unsigned


{-| If the user has sent this event to the homeserver, then the homeserver might
display the original transaction id used for the event.
-}
transactionId : Event -> Maybe String
transactionId event =
    Maybe.andThen (\(UnsignedData data) -> data.transactionId) event.unsigned


unsignedCoder : Json.Coder UnsignedData
unsignedCoder =
    Json.object4
        { name = Text.docs.unsigned.name
        , description = Text.docs.unsigned.description
        , init = \a b c d -> UnsignedData { age = a, prevContent = b, redactedBecause = c, transactionId = d }
        }
        (Json.field.optional.value
            { fieldName = "age"
            , toField = \(UnsignedData data) -> data.age
            , description = Text.fields.unsigned.age
            , coder = Json.int
            }
        )
        (Json.field.optional.value
            { fieldName = "prevContent"
            , toField = \(UnsignedData data) -> data.prevContent
            , description = Text.fields.unsigned.prevContent
            , coder = Json.value
            }
        )
        (Json.field.optional.value
            { fieldName = "redactedBecause"
            , toField = \(UnsignedData data) -> data.redactedBecause
            , description = Text.fields.unsigned.redactedBecause
            , coder = Json.lazy (\_ -> coder)
            }
        )
        (Json.field.optional.value
            { fieldName = "transactionId"
            , toField = \(UnsignedData data) -> data.transactionId
            , description = Text.fields.unsigned.transactionId
            , coder = Json.string
            }
        )
