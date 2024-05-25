module Internal.Api.Main exposing
    ( Msg
    , sendMessageEvent
    )

{-|


# Main API module

This module is used as reference for getting


## VaultUpdate

@docs Msg


## Actions

@docs sendMessageEvent

-}

import Internal.Api.Task as ITask exposing (Backpack)
import Internal.Tools.Json as Json
import Internal.Values.Context as Context
import Internal.Values.Envelope as E


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
