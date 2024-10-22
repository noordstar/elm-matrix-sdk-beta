module Matrix.Room exposing
    ( Room, mostRecentEvents, roomId
    , name, topic, pinnedEvents, getState
    , getAccountData, setAccountData
    , sendMessageEvent, sendStateEvent
    , invite, kick, ban
    )

{-|


# Room

What is usually called a chat, a channel, a conversation or a group chat on
other platforms, the term used in Matrix is a "room". A room is a conversation
where a group of users talk to each other.

@docs Room, mostRecentEvents, roomId

This module exposes various functions that help you inspect various aspects of
a room.


## Room state

State events describe the room's current state. Using state events, you can
determine various pieces of information, such as the room's name, its members
and how many people have rejected your invitation to join.

@docs name, topic, pinnedEvents, getState


## Account data

Account data is personal information that the user stores about this Matrix
room. This may include information like:

  - What type of room this is
  - A list of members in the room to ignore
  - A list of currently ongoing chess matches in the room
  - Personal notes the user may be taking

You may consider the account data as a `Dict String Json.Value` type. Account
data is linked to the user account: other logged in devices can see the account
data too, as the server synchronizes it, but the server shouldn´t show it to
other users.

@docs getAccountData, setAccountData


## Sending events

Besides reading the latest events, one can also send new events to the Matrix
room. These events are JSON objects that can be shaped in any way or form that
you like. To help other users with decoding your JSON objects, you pass an
`eventType` string which helps them figure out the nature of your JSON object.

@docs sendMessageEvent, sendStateEvent


## Moderating users

@docs invite, kick, ban

-}

import Internal.Api.Main as Api
import Internal.Values.Envelope as Envelope
import Internal.Values.Room as Internal
import Json.Decode as D
import Json.Encode as E
import Types exposing (Room(..))


{-| The Matrix Room type representing a room that the Matrix user has joined.
-}
type alias Room =
    Types.Room


{-| Ban a user from a room.
-}
ban :
    { reason : Maybe String
    , room : Room
    , toMsg : Types.VaultUpdate -> msg
    , user : Types.User
    }
    -> Cmd msg
ban data =
    case ( data.room, data.user ) of
        ( Room room, Types.User user ) ->
            Api.banUser room
                { reason = data.reason
                , roomId = roomId data.room
                , toMsg = Types.VaultUpdate >> data.toMsg
                , user = Envelope.getContent user
                }


{-| Get a piece of account data linked to a certain string key.
-}
getAccountData : String -> Room -> Maybe E.Value
getAccountData key (Room room) =
    Envelope.extract (Internal.getAccountData key) room


{-| Get a room's current state.
-}
getState : { eventType : String, stateKey : String } -> Room -> Maybe Types.Event
getState data (Room room) =
    Envelope.mapMaybe (Internal.getState data) room
        |> Maybe.map Types.Event


{-| Invite a user to a room.
-}
invite :
    { reason : Maybe String
    , room : Room
    , toMsg : Types.VaultUpdate -> msg
    , user : Types.User
    }
    -> Cmd msg
invite data =
    case ( data.room, data.user ) of
        ( Room room, Types.User user ) ->
            Api.inviteUser room
                { reason = data.reason
                , roomId = roomId data.room
                , toMsg = Types.VaultUpdate >> data.toMsg
                , user = Envelope.getContent user
                }


{-| Kick a user from a room.
-}
kick :
    { reason : Maybe String
    , room : Room
    , toMsg : Types.VaultUpdate -> msg
    , user : Types.User
    }
    -> Cmd msg
kick data =
    case ( data.room, data.user ) of
        ( Room room, Types.User user ) ->
            Api.kickUser room
                { reason = data.reason
                , roomId = roomId data.room
                , toMsg = Types.VaultUpdate >> data.toMsg
                , user = Envelope.getContent user
                }


{-| Get a list of the most recent events sent in the room.
-}
mostRecentEvents : Room -> List Types.Event
mostRecentEvents (Room room) =
    Envelope.mapList Internal.mostRecentEvents room
        |> List.map Types.Event


{-| Get a room's name. The room name is a human-friendly string designed to be
displayed to the end-user.

Keep in mind that the room name is not unique: multiple rooms can have the same
name.

-}
name : Room -> Maybe String
name (Room room) =
    room
        |> Envelope.extract (Internal.getState { eventType = "m.room.name", stateKey = "" })
        |> Maybe.map .content
        |> Maybe.andThen (D.decodeValue (D.field "name" D.string) >> Result.toMaybe)


{-| Get a room's pinned events. Pinned events are event IDs that the user is
expected to review later.
-}
pinnedEvents : Room -> List String
pinnedEvents (Room room) =
    room
        |> Envelope.extract (Internal.getState { eventType = "m.room.pinned_events", stateKey = "" })
        |> Maybe.map .content
        |> Maybe.andThen (D.decodeValue (D.field "pinned" (D.list D.string)) >> Result.toMaybe)
        |> Maybe.withDefault []


{-| Get a room's room id. This is an opaque string that distinguishes rooms from
each other.
-}
roomId : Room -> String
roomId (Room room) =
    Envelope.extract .roomId room


{-| Send a message event to a given room.
-}
sendMessageEvent :
    { content : E.Value
    , eventType : String
    , room : Room
    , toMsg : Types.VaultUpdate -> msg
    , transactionId : String
    }
    -> Cmd msg
sendMessageEvent data =
    case data.room of
        Room room ->
            Api.sendMessageEvent room
                { content = data.content
                , eventType = data.eventType
                , roomId = roomId data.room
                , toMsg = Types.VaultUpdate >> data.toMsg
                , transactionId = data.transactionId
                }


{-| Send a state event to a given room.
-}
sendStateEvent :
    { content : E.Value
    , eventType : String
    , room : Room
    , stateKey : String
    , toMsg : Types.VaultUpdate -> msg
    }
    -> Cmd msg
sendStateEvent data =
    case data.room of
        Room room ->
            Api.sendStateEvent room
                { content = data.content
                , eventType = data.eventType
                , roomId = roomId data.room
                , stateKey = data.stateKey
                , toMsg = Types.VaultUpdate >> data.toMsg
                }


{-| Set account data to a Matrix room.
-}
setAccountData :
    { content : E.Value
    , eventType : String
    , room : Room
    , toMsg : Types.VaultUpdate -> msg
    }
    -> Cmd msg
setAccountData data =
    case data.room of
        Room room ->
            Api.setRoomAccountData room
                { content = data.content
                , eventType = data.eventType
                , roomId = roomId data.room
                , toMsg = Types.VaultUpdate >> data.toMsg
                }


{-| Get a room's topic. A topic is a short message detailing what is currently
being discussed in the room. It can also be used as a way to display extra
information about the room, which may not be suitable for the room name.
-}
topic : Room -> Maybe String
topic (Room room) =
    room
        |> Envelope.extract (Internal.getState { eventType = "m.room.topic", stateKey = "" })
        |> Maybe.map .content
        |> Maybe.andThen (D.decodeValue (D.field "topic" D.string) >> Result.toMaybe)
