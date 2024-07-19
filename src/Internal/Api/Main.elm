module Internal.Api.Main exposing
    ( Msg
    , sendMessageEvent, sendStateEvent, setRoomAccountData, sync
    )

{-|


# Main API module

This module is used as reference for getting


## VaultUpdate

@docs Msg


## Actions

@docs sendMessageEvent, sendStateEvent, setRoomAccountData, sync

-}

import Internal.Api.Task as ITask exposing (Backpack)
import Internal.Tools.Json as Json
import Internal.Values.Context as Context
import Internal.Values.Envelope as E
import Internal.Values.User as User
import Internal.Values.Vault as V


{-| Update message type that is being returned.
-}
type alias Msg =
    Backpack


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
