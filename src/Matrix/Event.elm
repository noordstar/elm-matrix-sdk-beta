module Matrix.Event exposing
    ( Event, content, eventType, stateKey
    , eventId, originServerTs, roomId, sender
    , previousContent, redactedBecause
    )

{-|


# Matrix Events

This module contains all the functions necessary to view and manipulate Matrix
events.


## Event

@docs Event, content, eventType, stateKey


## Metadata

@docs eventId, originServerTs, roomId, sender


## Optional data

Occasionally, the Event might bring some extra information. Given how this
information isn't always applicable, it doesn't always exist.

@docs previousContent, redactedBecause

-}

import Internal.Values.Event as Internal
import Json.Encode
import Time
import Types exposing (Event(..))


{-| In Matrix, the primary form of communication is to send JSON values to one
another. These JSON values, together with their metadata, are bundled into Event
types. They contain information like:

  - Who sent the JSON value
  - How they intend you to decode it
  - When they sent it
  - In what room they sent it

-}
type alias Event =
    Types.Event


{-| Receive the body of an Event, as created by the user that sent it.
-}
content : Event -> Json.Encode.Value
content (Event event) =
    Internal.content event


{-| Determine the globally unique identifier for an event.
-}
eventId : Event -> String
eventId (Event event) =
    Internal.eventId event


{-| To give a hint what the event's [content](#content) might look like, users
can use this eventType value to hint at how the JSON might be decoded.

Standard examples of event types are `m.room.message`, `m.room.member` and
`me.noordstar.game.chess.move`.

-}
eventType : Event -> String
eventType (Event event) =
    Internal.eventType event


{-| Determine the timestamp of at what time the event was originally received by
the original homeserver.

Generally, this timestamp offers a relatively accurate indicator of when a
message was sent. However, this number isn't completely reliable! The timestamp
can be far in the past due to long network lag, and a (malicious) homeserver can
spoof this number to make it seem like something was sent ridiculously far in
the past - or even in the future.

-}
originServerTs : Event -> Time.Posix
originServerTs (Event event) =
    Internal.originServerTs event


{-| Determine the previous `content` value for this event. This field is only a
`Just value` if the event is a state event, and the Matrix Vault has permission
to see the previous content.
-}
previousContent : Event -> Maybe Json.Encode.Value
previousContent (Event event) =
    Internal.prevContent event


{-| If the event has been redacted, the homeserver can display the event that
redacted it here.
-}
redactedBecause : Event -> Maybe Event
redactedBecause (Event event) =
    Internal.redactedBecause event
        |> Maybe.map Event


{-| Unique id assigned to the Matrix room. You can use this room id to reference
or look up rooms.
-}
roomId : Event -> String
roomId (Event event) =
    Internal.roomId event


{-| Determine the fully-qualified ID of the user who sent an event.
-}
sender : Event -> String
sender (Event event) =
    Internal.sender event


{-| Determine an event's state key.

It is present if, and only if, the event is a _state_ event. The key makes the
piece of state unique in the room. Note that it is often `Just ""`. If it is not
present, its value is `Nothing`.

State keys starting with an `@` are reserved for referencing user IDs, such as
room members. With the exception of a few events, state events set with a given
user'd ID as the state key can only be set by that user.

-}
stateKey : Event -> Maybe String
stateKey (Event event) =
    Internal.stateKey event
