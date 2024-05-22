module Internal.Api.Task exposing (Task, run)

{-|


# Task module

This module is used to define how API calls are made. These completed API tasks
can be directly converted to Cmd types that the end user of the SDK can access.

These tasks do not affect the `Vault` directly, but instead, return a
`VaultUpdate` type that the user can apply to keep their `Vault` type
up-to-date.


## Use

@docs Task, run

-}

import Internal.Api.BaseUrl.Api
import Internal.Api.Chain as C
import Internal.Api.Request as Request
import Internal.Api.Versions.Api
import Internal.Config.Log exposing (Log, log)
import Internal.Values.Context as Context exposing (APIContext)
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


{-| Get the base URL where the Matrix API can be accessed
-}
getBaseUrl : UFTask a { a | baseUrl : () }
getBaseUrl c =
    case Context.fromApiFormat c |> .baseUrl of
        Just b ->
            C.succeed
                { messages = []
                , logs = [ log.debug "Using cached baseURL from Vault" ]
                , contextChange = Context.setBaseUrl b
                }
                c

        Nothing ->
            Internal.Api.BaseUrl.Api.baseUrl
                { url = Context.fromApiFormat c |> .serverName }
                c


{-| Get the versions that are potentially supported by the Matrix API
-}
getVersions : UFTask { a | baseUrl : () } { a | baseUrl : (), versions : () }
getVersions c =
    case Context.fromApiFormat c |> .versions of
        Just v ->
            C.succeed
                { messages = []
                , logs = [ log.debug "Using cached versions from Vault" ]
                , contextChange = Context.setVersions v
                }
                c

        Nothing ->
            Internal.Api.Versions.Api.versions c


{-| Establish a Task Chain context where the base URL and supported list of
versions are known.
-}
makeVB : UFTask {} { a | baseUrl : (), versions : () }
makeVB =
    C.andThen getVersions getBaseUrl


{-| Transform a completed task into a Cmd.
-}
run : (Backpack -> msg) -> Task -> APIContext {} -> Cmd msg
run toMsg task context =
    context
        |> C.toTask task
        |> Task.perform toMsg
