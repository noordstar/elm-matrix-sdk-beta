module Matrix.Types.ClientEvent exposing
    ( ClientEvent, content, eventType, stateKey
    , eventId, roomId, sender, originServerTs
    , previousContent, redactedBecause
    , decoder, decoderWithMembership
    )

{-| Matrix Events

This module contains all the functions necessary to view and manipulate Matrix
events that your client receives. The major difference with the
[SyncEvent](Matrix.Types.SyncEvent) type is that this type also stores an
attached room id.


## Event

@docs ClientEvent, content, eventType, stateKey


## Metadata

@docs eventId, roomId, sender, originServerTs


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
import Matrix.Types.RoomId as RoomId exposing (RoomId)
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
type ClientEvent
    = ClientEvent
        { content : D.Value
        , eventId : EventId
        , originServerTs : Time.Posix
        , roomId : RoomId
        , sender : UserId
        , stateKey : Maybe String
        , type_ : String
        , age : Maybe Duration
        , membership : Maybe String
        , prevContent : Maybe D.Value
        , redactedBecause : Maybe ClientEvent
        , transactionId : Maybe String
        }


{-| Receive the body of an event, as created by the user that sent it.
-}
content : ClientEvent -> Json.Encode.Value
content (ClientEvent event) =
    event.content


{-| Determine the globally unique identifier for an event.
-}
eventId : ClientEvent -> EventId
eventId (ClientEvent event) =
    event.eventId


{-| To give a hint what the event's [content](#content) might look like, users
can use this eventType value to hint at how the JSON might be decoded.

Standard examples of event types are `m.room.message`, `m.room.member` and
`me.noordstar.game.chess.move`.

-}
eventType : ClientEvent -> String
eventType (ClientEvent event) =
    event.type_


{-| Determine the timestamp of at what time the event was originally received by
the original homeserver.

Generally, this timestamp offers a relatively accurate indicator of when a
message was sent. However, this number isn't completely reliable! The timestamp
can be far in the past due to long network lag, and a (malicious) homeserver can
spoof this number to make it seem like something was sent ridiculously far in
the past - or even in the future.

-}
originServerTs : ClientEvent -> Time.Posix
originServerTs (ClientEvent event) =
    event.originServerTs


{-| Determine the previous `content` value for this event. This field is only a
`Just value` if the event is a state event, and the Matrix API included the
previous content in its response.
-}
previousContent : ClientEvent -> Maybe Json.Encode.Value
previousContent (ClientEvent event) =
    event.prevContent


{-| If the event has been redacted, the homeserver can display the event that
redacted it here.
-}
redactedBecause : ClientEvent -> Maybe ClientEvent
redactedBecause (ClientEvent event) =
    event.redactedBecause


{-| Unique id assigned to the Matrix room. You can use this room id to reference
or look up rooms.
-}
roomId : ClientEvent -> RoomId
roomId (ClientEvent event) =
    event.roomId


{-| Determine the fully-qualified ID of the user who sent an event.
-}
sender : ClientEvent -> UserId
sender (ClientEvent event) =
    event.sender


{-| Determine an event's state key.

It is present if, and only if, the event is a _state_ event. The key makes the
piece of state unique in the room. Note that it is often `Just ""`. If it is not
present, its value is `Nothing`.

State keys starting with an `@` are reserved for referencing user IDs, such as
room members. With the exception of a few events, state events set with a given
user'd ID as the state key can only be set by that user.

-}
stateKey : ClientEvent -> Maybe String
stateKey (ClientEvent event) =
    event.stateKey


{-| Decode a Matrix event from JSON.

This decoder handles common top-level fields and optional `unsigned` metadata.

    import Json.Decode as D
    import Matrix.Types.ClientEvent as ClientEvent

    D.decodeString ClientEvent.decoder "{ \"type\": \"m.room.message\", ... }"
    -- Ok <event>

Note: This decoder does not interpret `content`. You are expected to decode it
based on the event `type_`.

-}
decoder : D.Decoder ClientEvent
decoder =
    decoderWithMembership True


{-| Decode a Matrix event using a specific response profile.
-}
decoderWithMembership : Bool -> D.Decoder ClientEvent
decoderWithMembership includeMembership =
    DE.map12
        (\ct ei ts ri sd sk t ag ms pc rb ti ->
            ClientEvent
                { content = ct
                , eventId = ei
                , originServerTs = ts
                , roomId = ri
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
        (D.field "room_id" RoomId.decoder)
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
