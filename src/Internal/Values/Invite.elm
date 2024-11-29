module Internal.Values.Invite exposing (..)

{-|


# Invite

An invite is an invitation to join a room. This offers the user an opportunity
to participate in a private room, or helps other users discover a room that
might be relevant to them.

-}

import FastDict as Dict exposing (Dict)
import Internal.Config.Text as Text
import Internal.Grammar.UserId as UserId
import Internal.Tools.Json as Json
import Internal.Values.User as User exposing (User)


type alias Invite =
    { roomId : String
    , events : Dict String (Dict String InviteEvent)
    }


type alias InviteEvent =
    { content : Json.Value
    , sender : User
    , stateKey : String
    , eventType : String
    }


addInviteEvent : InviteEvent -> Invite -> Invite
addInviteEvent event invite =
    if UserId.isIllegal event.sender then
        invite

    else
        { invite
            | events =
                Dict.update event.eventType
                    (\mdict ->
                        case mdict of
                            Just dict ->
                                Just (Dict.insert event.stateKey event dict)

                            Nothing ->
                                Just (Dict.singleton event.stateKey event)
                    )
                    invite.events
        }


{-| Define how an invite is encoded in JSON.
-}
coder : Json.Coder Invite
coder =
    Json.object2
        { name = Text.docs.invite.name
        , description = Text.docs.invite.description
        , init = Invite
        }
        (Json.field.required
            { fieldName = "roomId"
            , toField = .roomId
            , description = Text.fields.invite.roomId
            , coder = Json.string
            }
        )
        (Json.field.optional.withDefault
            { fieldName = "events"
            , toField = .events
            , description = Text.fields.invite.events
            , coder = Json.fastDict <| Json.fastDict coderEvent
            , default = ( Dict.empty, [] )
            }
        )


{-| Define how an InviteEvent is encoded in JSON.
-}
coderEvent : Json.Coder InviteEvent
coderEvent =
    Json.object4
        { name = Text.docs.inviteEvent.name
        , description = Text.docs.inviteEvent.description
        , init = InviteEvent
        }
        (Json.field.required
            { fieldName = "content"
            , toField = .content
            , description = Text.fields.event.content
            , coder = Json.value
            }
        )
        (Json.field.required
            { fieldName = "sender"
            , toField = .sender
            , description = Text.fields.event.sender
            , coder = User.strictCoder
            }
        )
        (Json.field.required
            { fieldName = "stateKey"
            , toField = .stateKey
            , description = Text.fields.event.stateKey
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "eventType"
            , toField = .eventType
            , description = Text.fields.event.eventType
            , coder = Json.string
            }
        )


getInviteEvent : { eventType : String, stateKey : String } -> Invite -> Maybe InviteEvent
getInviteEvent { eventType, stateKey } invite =
    invite.events
        |> Dict.get eventType
        |> Maybe.andThen (Dict.get stateKey)


toList : Invite -> List InviteEvent
toList invite =
    invite.events
        |> Dict.values
        |> List.concatMap Dict.values
