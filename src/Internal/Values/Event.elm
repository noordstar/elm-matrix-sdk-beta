module Internal.Values.Event exposing
    ( Event
    , content, eventId, eventType, originServerTs, roomId, sender, stateKey
    , UnsignedData(..), age, prevContent, redactedBecause, transactionId
    , encode, decoder
    )

{-|


# Event

The `Event` module hosts all the information for a single event in the timeline
of a room.

@docs Event


## Get information

@docs content, eventId, eventType, originServerTs, roomId, sender, stateKey


## Unsigned data

@docs UnsignedData, age, prevContent, redactedBecause, transactionId


## JSON Coder

@docs encode, decoder

-}

import Internal.Config.Default as Default
import Internal.Tools.Decode as D
import Internal.Tools.Encode as E
import Internal.Tools.Timestamp as Timestamp exposing (Timestamp)
import Internal.Values.Envelope as Envelope
import Json.Decode as D
import Json.Encode as E


{-| The Event type occurs everywhere on a user's timeline.
-}
type alias Event =
    Envelope.Envelope IEvent


type alias IEvent =
    { content : E.Value
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
        , prevContent : Maybe E.Value
        , redactedBecause : Maybe Event
        , transactionId : Maybe String
        }


{-| Get the event's age, if at all provided by the homeserver.
-}
age : Event -> Maybe Int
age envelope =
    Envelope.extract
        (\event ->
            Maybe.andThen
                (\(UnsignedData data) -> data.age)
                event.unsigned
        )
        envelope


{-| Determine the body of this event, as created by the user that sent it.
-}
content : Event -> E.Value
content =
    Envelope.extract .content


{-| Decode an Event from a JSON value.
-}
decoder : D.Decoder Event
decoder =
    D.map8 IEvent
        (D.field "content" D.value)
        (D.field "eventId" D.string)
        (D.field "originServerTs" Timestamp.decoder)
        (D.field "roomId" D.string)
        (D.field "sender" D.string)
        (D.opField "stateKey" D.string)
        (D.field "eventType" D.string)
        (D.opField "unsigned" decoderUnsignedData)
        |> Envelope.decoder


{-| Decode Unsigned Data from a JSON value.
-}
decoderUnsignedData : D.Decoder UnsignedData
decoderUnsignedData =
    D.map4 (\a b c d -> UnsignedData { age = a, prevContent = b, redactedBecause = c, transactionId = d })
        (D.opField "age" D.int)
        (D.opField "prevContent" D.value)
        (D.opField "redactedBecause" (D.lazy (\_ -> decoder)))
        (D.opField "transactionId" D.string)


{-| Encode an Event into a JSON value.
-}
encode : Event -> E.Value
encode envelope =
    Envelope.encode
        (\event ->
            E.maybeObject
                [ ( "content", Just event.content )
                , ( "eventId", Just <| E.string event.eventId )
                , ( "originServerTs", Just <| Timestamp.encode event.originServerTs )
                , ( "roomId", Just <| E.string event.roomId )
                , ( "sender", Just <| E.string event.sender )
                , ( "stateKey", Maybe.map E.string event.stateKey )
                , ( "eventType", Just <| E.string event.eventType )
                , ( "unsigned", Maybe.map encodeUnsignedData event.unsigned )
                , ( "version", Just <| E.string Default.currentVersion )
                ]
        )
        envelope


{-| Encode Unsigned Data into a JSON value.
-}
encodeUnsignedData : UnsignedData -> E.Value
encodeUnsignedData (UnsignedData data) =
    E.maybeObject
        [ ( "age", Maybe.map E.int data.age )
        , ( "prevContent", data.prevContent )
        , ( "redactedBecause", Maybe.map encode data.redactedBecause )
        , ( "transactionId", Maybe.map E.string data.transactionId )
        ]


{-| Determine the globally unique identifier for an event.
-}
eventId : Event -> String
eventId =
    Envelope.extract .eventId


{-| To give a hint what the event's [content](#content) might look like, users
can use this eventType value to hint at how the JSON might be decoded.

Standard examples of event types are `m.room.message`, `m.room.member` and
`me.noordstar.game.chess.move`.

-}
eventType : Event -> String
eventType =
    Envelope.extract .eventType


{-| Determine the timestamp of at what time the event was originally received by
the original homeserver.

Generally, this timestamp offers a relatively accurate indicator of when a
message was sent. However, this number isn't completely reliable! The timestamp
can be far in the past due to long network lag, and a (malicious) homeserver can
spoof this number to make it seem like something was sent ridiculously far in
the past - or even in the future.

-}
originServerTs : Event -> Timestamp
originServerTs =
    Envelope.extract .originServerTs


{-| Determine the previous `content` value for this event. This field is only a
`Just value` if the event is a state event, and the Matrix Vault has permission
to see the previous content.
-}
prevContent : Event -> Maybe E.Value
prevContent envelope =
    Envelope.extract
        (\event ->
            Maybe.andThen
                (\(UnsignedData data) -> data.prevContent)
                event.unsigned
        )
        envelope


{-| If the event has been redacted, the homeserver can display the event that
redacted it here.
-}
redactedBecause : Event -> Maybe Event
redactedBecause envelope =
    Envelope.extract
        (\event ->
            Maybe.andThen
                (\(UnsignedData data) -> data.redactedBecause)
                event.unsigned
        )
        envelope


{-| Unique id assigned to the Matrix room. You can use this room id to reference
or look up rooms.
-}
roomId : Event -> String
roomId =
    Envelope.extract .roomId


{-| Determine the fully-qualified ID of the user who sent an event.
-}
sender : Event -> String
sender =
    Envelope.extract .sender


{-| Determine an event's state key.

It is present if, and only if, the event is a _state_ event. The key makes the
piece of state unique in the room. Note that it is often `Just ""`. If it is not
present, its value is `Nothing`.

State keys starting with an `@` are reserved for referencing user IDs, such as
room members. With the exception of a few events, state events set with a given
user'd ID as the state key can only be set by that user.

-}
stateKey : Event -> Maybe String
stateKey =
    Envelope.extract .stateKey


{-| If the user has sent this event to the homeserver, then the homeserver might
display the original transaction id used for the event.
-}
transactionId : Event -> Maybe String
transactionId envelope =
    Envelope.extract
        (\event ->
            Maybe.andThen
                (\(UnsignedData data) -> data.transactionId)
                event.unsigned
        )
        envelope
