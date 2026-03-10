module Matrix.Types.EventId exposing
    ( EventId, fromString, toString
    , encode, decoder
    )

{-| Event identifiers.

Every event stored in Matrix has an `EventId`.


## EventId

@docs EventId, fromString, toString


## JSON

@docs encode, decoder

-}

import Json.Decode as D
import Json.Encode as E


{-| The ID of a Matrix event.
-}
type EventId
    = EventId String


{-| Encode an event ID as a JSON string.

    EventId.encode eventId

-}
encode : EventId -> E.Value
encode =
    toString >> E.string


{-| Decode an event ID from a JSON string.

    import Json.Decode as D
    import Matrix.Types.EventId as EventId

    D.decodeString EventId.decoder "\"$event:example.org\""
    -- Ok <eventId>

-}
decoder : D.Decoder EventId
decoder =
    D.map fromString D.string


{-| Create an event ID from its raw string value.

    EventId.fromString "$event:example.org"

-}
fromString : String -> EventId
fromString =
    EventId


{-| Extract the raw string value from an event ID.

    EventId.toString eventId
    -- "$event:example.org"

-}
toString : EventId -> String
toString (EventId s) =
    s
