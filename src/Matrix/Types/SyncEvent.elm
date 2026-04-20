module Matrix.Types.SyncEvent exposing
    ( SyncEvent, content, eventType, stateKey
    , eventId, sender, originServerTs
    , previousContent, redactedBecause
    , decoder, decoderWithMembership
    )

{-| Matrix Events

This module contains all the functions necessary to view and manipulate Matrix
events that your client receives. The major difference with the
[ClientEvent](Matrix.Types.ClientEvent) type is that this type does not contain
a room id.


## Event

@docs SyncEvent, content, eventType, stateKey


## Metadata

@docs eventId, sender, originServerTs


## Optional data

Occasionally, the event might bring some extra information. Given how this
information isn't always applicable, it doesn't always exist.

@docs previousContent, redactedBecause


## JSON

@docs decoder, decoderWithMembership

-}

import Duration exposing (Duration)
import Internal.Tools.DecodeExtra as DE
import Json.Decode as D
import Json.Encode
import Matrix.Types.EventId as EventId exposing (EventId)
import Matrix.Types.UserId as UserId exposing (UserId)
import Time


{-| In Matrix, the primary form of communication is to send JSON values to one
another. These JSON values, together with their metadata, are bundled into
event types. They contain information like:

  - Who sent the JSON value
  - How they intend you to decode it
  - When they sent it
  - In what room they sent it

-}
type SyncEvent
    = SyncEvent
        { content : D.Value
        , eventId : EventId
        , originServerTs : Time.Posix
        , sender : UserId
        , stateKey : Maybe String
        , type_ : String
        , age : Maybe Duration
        , membership : Maybe String
        , prevContent : Maybe D.Value
        , redactedBecause : Maybe SyncEvent
        , transactionId : Maybe String
        }


{-| Receive the body of an event, as created by the user that sent it.
-}
content : SyncEvent -> Json.Encode.Value
content (SyncEvent event) =
    event.content


{-| Determine the globally unique identifier for an event.
-}
eventId : SyncEvent -> EventId
eventId (SyncEvent event) =
    event.eventId


{-| To give a hint what the event's [content](#content) might look like, users
can use this eventType value to hint at how the JSON might be decoded.

Standard examples of event types are `m.room.message`, `m.room.member` and
`me.noordstar.game.chess.move`.

-}
eventType : SyncEvent -> String
eventType (SyncEvent event) =
    event.type_


{-| Determine the timestamp of at what time the event was originally received by
the original homeserver.

Generally, this timestamp offers a relatively accurate indicator of when a
message was sent. However, this number isn't completely reliable! The timestamp
can be far in the past due to long network lag, and a (malicious) homeserver can
spoof this number to make it seem like something was sent ridiculously far in
the past - or even in the future.

-}
originServerTs : SyncEvent -> Time.Posix
originServerTs (SyncEvent event) =
    event.originServerTs


{-| Determine the previous `content` value for this event. This field is only a
`Just value` if the event is a state event, and the Matrix API included the
previous content in its response.
-}
previousContent : SyncEvent -> Maybe Json.Encode.Value
previousContent (SyncEvent event) =
    event.prevContent


{-| If the event has been redacted, the homeserver can display the event that
redacted it here.
-}
redactedBecause : SyncEvent -> Maybe SyncEvent
redactedBecause (SyncEvent event) =
    event.redactedBecause


{-| Determine the fully-qualified ID of the user who sent an event.
-}
sender : SyncEvent -> UserId
sender (SyncEvent event) =
    event.sender


{-| Determine an event's state key.

It is present if, and only if, the event is a _state_ event. The key makes the
piece of state unique in the room. Note that it is often `Just ""`. If it is not
present, its value is `Nothing`.

State keys starting with an `@` are reserved for referencing user IDs, such as
room members. With the exception of a few events, state events set with a given
user'd ID as the state key can only be set by that user.

-}
stateKey : SyncEvent -> Maybe String
stateKey (SyncEvent event) =
    event.stateKey


{-| Decode a Matrix event from JSON.

This decoder handles common top-level fields and optional `unsigned` metadata.

    import Json.Decode as D
    import Matrix.Types.SyncEvent as SyncEvent

    D.decodeString SyncEvent.decoder "{ \"type\": \"m.room.message\", ... }"
    -- Ok <event>

Note: This decoder does not interpret `content`. You are expected to decode it
based on the event `type_`.

-}
decoder : D.Decoder SyncEvent
decoder =
    decoderWithMembership True


{-| Decode a Matrix event using a specific response profile.
-}
decoderWithMembership : Bool -> D.Decoder SyncEvent
decoderWithMembership includeMembership =
    DE.map11
        (\ct ei ts sd sk t ag ms pc rb ti ->
            SyncEvent
                { content = ct
                , eventId = ei
                , originServerTs = ts
                , sender = sd
                , stateKey = sk
                , type_ = t
                , age = ag
                , membership = ms
                , prevContent = pc
                , redactedBecause = rb
                , transactionId = ti
                }
        )
        (D.field "content" D.value)
        (D.field "event_id" EventId.decoder)
        (D.field "origin_server_ts" (D.map Time.millisToPosix D.int))
        (D.field "sender" UserId.decoder)
        (DE.opField "state_key" D.string)
        (D.field "type" D.string)
        (unsignedField "age" (D.map (toFloat >> Duration.milliseconds) D.int))
        (membershipDecoder includeMembership)
        (unsignedField "prev_content" D.value)
        (unsignedField "redacted_because" (D.lazy (\() -> decoderWithMembership includeMembership)))
        (unsignedField "transaction_id" D.string)


membershipDecoder : Bool -> D.Decoder (Maybe String)
membershipDecoder includeMembership =
    if includeMembership then
        unsignedField "membership" D.string

    else
        D.succeed Nothing


unsignedField : String -> D.Decoder a -> D.Decoder (Maybe a)
unsignedField fieldName fieldDecoder =
    DE.opField fieldName fieldDecoder
        |> DE.opField "unsigned"
        |> D.map (Maybe.andThen identity)
