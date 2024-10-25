module Matrix.Invite exposing
    ( Invite, accept, reject
    , roomId, InviteEvent, get, events
    , sender, stateKey, eventType, content
    )

{-|


# Invites

Sometimes, your user will be invited to a new room!
This module offers you a few simple handles to deal with such invites -
you can accept them, reject them or inspect them for further information.


# Invitations

@docs Invite, accept, reject


# Exploring invitations

Sometimes, you may want to display information about the room.

Be careful though, anyone can invite you to any room! This means that room invites
may contain offensive, shocking or other unwanted content that the user may not
want to see.

@docs roomId, InviteEvent, get, events

Once you have the event you want, you can explore it with the following functions.

@docs sender, stateKey, eventType, content

-}

import Internal.Api.Main as Api
import Internal.Values.Envelope as Envelope
import Internal.Values.Invite as Invite
import Json.Encode as E
import Types exposing (Invite(..), InviteEvent(..))


{-| Invitation to a given room. This type represents contains all available
information about the room.
-}
type alias Invite =
    Types.Invite


{-| An InviteEvent is a state event that indicates the room's state.

This InviteEvent is more limited than the standard [Event](Matrix-Event#Event).
It exclusively provides the information that is considered relevant for the
invitation.

-}
type alias InviteEvent =
    Types.InviteEvent


{-| Accept the invite. As a result, the user joins the room.
-}
accept :
    { invite : Invite
    , reason : Maybe String
    , toMsg : Types.VaultUpdate -> msg
    }
    -> Cmd msg
accept data =
    case data.invite of
        Invite invite ->
            Api.join
                invite
                { reason = data.reason
                , roomId = roomId data.invite
                , toMsg = Types.VaultUpdate >> data.toMsg
                }


{-| Determines the content of an event.
-}
content : InviteEvent -> E.Value
content (InviteEvent inviteEvent) =
    Envelope.extract .content inviteEvent


{-| Determines the content of a state event.
-}
eventType : InviteEvent -> String
eventType (InviteEvent inviteEvent) =
    Envelope.extract .eventType inviteEvent


{-| Get a list of all shared state events in the invite.
-}
events : Invite -> List InviteEvent
events (Invite invite) =
    Envelope.mapList Invite.toList invite
        |> List.map Types.InviteEvent


{-| Get a specific state event. This allows you to generate a previes of the
room. Do keep in mind that most servers do not give ALL state events, so you
might not have access to some state events.
-}
get : { eventType : String, invite : Invite, stateKey : String } -> Maybe InviteEvent
get data =
    case data.invite of
        Invite invite ->
            invite
                |> Envelope.mapMaybe
                    (Invite.getInviteEvent
                        { eventType = data.eventType
                        , stateKey = data.stateKey
                        }
                    )
                |> Maybe.map Types.InviteEvent


{-| Reject the invite. As a result, the room is informed that the user has
decided not to join. If the room is private, the user can no longer join until
they receive a new invite.
-}
reject :
    { invite : Invite
    , reason : Maybe String
    , toMsg : Types.VaultUpdate -> msg
    }
    -> Cmd msg
reject data =
    case data.invite of
        Invite invite ->
            Api.leave
                invite
                { reason = data.reason
                , roomId = roomId data.invite
                , toMsg = Types.VaultUpdate >> data.toMsg
                }


{-| Get the room id of the room that the user is invited to.
-}
roomId : Invite -> String
roomId (Invite invite) =
    Envelope.extract .roomId invite


{-| Determines the user that has originally sent this event.
-}
sender : InviteEvent -> Types.User
sender (InviteEvent inviteEvent) =
    Envelope.map .sender inviteEvent
        |> Types.User


{-| Determines the state key of a state event.
-}
stateKey : InviteEvent -> String
stateKey (InviteEvent inviteEvent) =
    Envelope.extract .stateKey inviteEvent
