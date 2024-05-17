module Internal.Api.Task exposing (..)

import Internal.Api.Chain as C
import Internal.Api.Request as Request
import Internal.Config.Log exposing (Log)
import Internal.Values.Context exposing (APIContext)
import Internal.Values.Envelope exposing (EnvelopeUpdate(..))
import Internal.Values.Room exposing (RoomUpdate(..))
import Internal.Values.Vault exposing (VaultUpdate(..))
import Task


{-| A Backpack is the ultimate message type that gets sent back by the Elm
runtime, which can be accessed, viewed and inspected.
-}
type alias Backpack =
    { messages : List (EnvelopeUpdate VaultUpdate), logs : List Log }


{-| A Task is a task that is ready to be sent to the outside world.
-}
type alias Task =
    C.TaskChain Never (EnvelopeUpdate VaultUpdate) {} {}


{-| An UnFinished Task that is used somewhere else in this module to write a
complete Task type.
-}
type alias UFTask a b =
    C.TaskChain Request.Error (EnvelopeUpdate VaultUpdate) a b


{-| Transform a completed task into a Cmd.
-}
run : (Backpack -> msg) -> Task -> APIContext {} -> Cmd msg
run toMsg task context =
    context
        |> C.toTask task
        |> Task.perform toMsg
