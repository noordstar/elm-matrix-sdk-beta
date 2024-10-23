module Internal.Api.Main exposing
    ( Msg
    , banUser, inviteUser, kickUser, leave, redact, sendMessageEvent, sendStateEvent, setAccountData, setRoomAccountData, sync, whoAmI
    )

{-|


# Main API module

This module is used as reference for getting


## VaultUpdate

@docs Msg


## Actions

@docs banUser, inviteUser, kickUser, leave, redact, sendMessageEvent, sendStateEvent, setAccountData, setRoomAccountData, sync, whoAmI

-}

import Internal.Api.Task as ITask exposing (Backpack)
import Internal.Tools.Json as Json
import Internal.Values.Context as Context
import Internal.Values.Envelope as E
import Internal.Values.User as User exposing (User)


{-| Update message type that is being returned.
-}
type alias Msg =
    Backpack


{-| Ban a user from a room.
-}
banUser :
    E.Envelope a
    ->
        { reason : Maybe String
        , roomId : String
        , toMsg : Msg -> msg
        , user : User
        }
    -> Cmd msg
banUser env data =
    ITask.run
        data.toMsg
        (ITask.banUser
            { reason = data.reason
            , roomId = data.roomId
            , user = data.user
            }
        )
        (Context.apiFormat env.context)


{-| Invite a user to a room.
-}
inviteUser :
    E.Envelope a
    ->
        { reason : Maybe String
        , roomId : String
        , toMsg : Msg -> msg
        , user : User
        }
    -> Cmd msg
inviteUser env data =
    ITask.run
        data.toMsg
        (ITask.inviteUser
            { reason = data.reason
            , roomId = data.roomId
            , user = data.user
            }
        )
        (Context.apiFormat env.context)


{-| Kick a user from a room.
-}
kickUser :
    E.Envelope a
    ->
        { reason : Maybe String
        , roomId : String
        , toMsg : Msg -> msg
        , user : User
        }
    -> Cmd msg
kickUser env data =
    ITask.run
        data.toMsg
        (ITask.kickUser
            { avatarUrl = Nothing
            , displayname = Nothing
            , reason = data.reason
            , roomId = data.roomId
            , user = data.user
            }
        )
        (Context.apiFormat env.context)


{-| Leave a room.
-}
leave :
    E.Envelope a
    ->
        { reason : Maybe String
        , roomId : String
        , toMsg : Msg -> msg
        }
    -> Cmd msg
leave env data =
    ITask.run
        data.toMsg
        (ITask.leave { reason = data.reason, roomId = data.roomId })
        (Context.apiFormat env.context)


{-| Redact an event.
-}
redact :
    E.Envelope a
    ->
        { eventId : String
        , reason : Maybe String
        , roomId : String
        , toMsg : Msg -> msg
        , transactionId : String
        }
    -> Cmd msg
redact env data =
    ITask.run
        data.toMsg
        (ITask.redact
            { eventId = data.eventId
            , reason = data.reason
            , roomId = data.roomId
            , transactionId = data.transactionId
            }
        )
        (Context.apiFormat env.context)


{-| Send a message event.
-}
sendMessageEvent :
    E.Envelope a
    ->
        { content : Json.Value
        , eventType : String
        , roomId : String
        , toMsg : Msg -> msg
        , transactionId : String
        }
    -> Cmd msg
sendMessageEvent env data =
    ITask.run
        data.toMsg
        (ITask.sendMessageEvent
            { content = data.content
            , eventType = data.eventType
            , roomId = data.roomId
            , transactionId = data.transactionId
            }
        )
        (Context.apiFormat env.context)


{-| Send a state event to a room.
-}
sendStateEvent :
    E.Envelope a
    ->
        { content : Json.Value
        , eventType : String
        , roomId : String
        , stateKey : String
        , toMsg : Msg -> msg
        }
    -> Cmd msg
sendStateEvent env data =
    ITask.run
        data.toMsg
        (ITask.sendStateEvent
            { content = data.content
            , eventType = data.eventType
            , roomId = data.roomId
            , stateKey = data.stateKey
            }
        )
        (Context.apiFormat env.context)


{-| Set global account data.
-}
setAccountData :
    E.Envelope a
    ->
        { content : Json.Value
        , eventType : String
        , toMsg : Msg -> msg
        }
    -> Cmd msg
setAccountData env data =
    case env.context.user of
        Just u ->
            ITask.run
                data.toMsg
                (ITask.setAccountData
                    { content = data.content
                    , eventType = data.eventType
                    , userId = User.toString u
                    }
                )
                (Context.apiFormat env.context)

        Nothing ->
            Cmd.none


{-| Set the account data for a Matrix room.
-}
setRoomAccountData :
    E.Envelope a
    ->
        { content : Json.Value
        , eventType : String
        , roomId : String
        , toMsg : Msg -> msg
        }
    -> Cmd msg
setRoomAccountData env data =
    case env.context.user of
        Just u ->
            ITask.run
                data.toMsg
                (ITask.setRoomAccountData
                    { content = data.content
                    , eventType = data.eventType
                    , roomId = data.roomId
                    , userId = User.toString u
                    }
                )
                (Context.apiFormat env.context)

        Nothing ->
            Cmd.none



-- TODO: Return error about lacking user capabilities


{-| Sync with the Matrix API to stay up-to-date.
-}
sync :
    E.Envelope a
    -> { toMsg : Msg -> msg }
    -> Cmd msg
sync env data =
    ITask.run
        data.toMsg
        (ITask.sync
            { fullState = Nothing
            , presence = env.settings.presence
            , since = env.context.nextBatch
            , timeout = Just env.settings.syncTime
            }
        )
        (Context.apiFormat env.context)


{-| Reveal personal information about the account to the user.
-}
whoAmI :
    E.Envelope a
    -> { toMsg : Msg -> msg }
    -> Cmd msg
whoAmI env data =
    ITask.run data.toMsg ITask.whoAmI (Context.apiFormat env.context)
