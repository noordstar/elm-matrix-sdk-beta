module Matrix.Room exposing
    ( Room, mostRecentEvents, roomId
    , getAccountData
    )

{-|


# Room

What is usually called a chat, a channel, a conversation or a group chat on
other platforms, the term used in Matrix is a "room". A room is a conversation
where a group of users talk to each other.

@docs Room, mostRecentEvents, roomId

This module exposes various functions that help you inspect various aspects of
a room.


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

@docs getAccountData

-}

import Internal.Values.Envelope as Envelope
import Internal.Values.Room as Internal
import Json.Encode as E
import Types exposing (Room(..))


{-| The Matrix Room type representing a room that the Matrix user has joined.
-}
type alias Room =
    Types.Room


{-| Get a piece of account data linked to a certain string key.
-}
getAccountData : String -> Room -> Maybe E.Value
getAccountData key (Room room) =
    Envelope.extract (Internal.getAccountData key) room


{-| Get a room's room id. This is an opaque string that distinguishes rooms from
each other.
-}
roomId : Room -> String
roomId (Room room) =
    Envelope.extract .roomId room


{-| Get a list of the most recent events sent in the room.
-}
mostRecentEvents : Room -> List Types.Event
mostRecentEvents (Room room) =
    Envelope.mapList Internal.mostRecentEvents room
        |> List.map Types.Event
