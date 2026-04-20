module Matrix.Types.RawEvent exposing
    ( RawEvent, content, eventType
    , decoder
    )

{-|


# Raw events

Raw events are the most basic form of information from the Matrix API.

They contain no metadata, only an event type string and its content in JSON
format.

    {
        "content": { "text": "hi!" },
        "type": "org.example.message"
    }

These events appear occasionally throughout the Matrix API.

@docs RawEvent, content, eventType


## JSON

@docs decoder

-}

import Json.Decode as D


{-| The RawEvent type contains two
-}
type RawEvent
    = RawEvent { content : D.Value, type_ : String }


{-| Returns the content JSON of a raw event. This content can be anything,
and might have been created by unknown actors on the internet.

Make sure to use Elm's standard JSON decoders to keep yourself clean from
malicious JSON attacks.

-}
content : RawEvent -> D.Value
content (RawEvent e) =
    e.content


{-| Decodes a RawEvent from JSON.
-}
decoder : D.Decoder RawEvent
decoder =
    D.map2 (\c t -> RawEvent { content = c, type_ = t })
        (D.field "content" D.value)
        (D.field "type" D.string)


{-| Returns the event type. This string gives a suggestion on how you can
decode the JSON content of the raw event.
-}
eventType : RawEvent -> String
eventType (RawEvent e) =
    e.type_
