module Internal.Api.Task exposing
    ( Task, run
    , sendMessageEvent
    )

{-|


# Task module

This module is used to define how API calls are made. These completed API tasks
can be directly converted to Cmd types that the end user of the SDK can access.

These tasks do not affect the `Vault` directly, but instead, return a
`VaultUpdate` type that the user can apply to keep their `Vault` type
up-to-date.


## Use

@docs Task, run


## Tasks

@docs sendMessageEvent

-}

import Internal.Api.BaseUrl.Api
import Internal.Api.Chain as C
import Internal.Api.LoginWithUsernameAndPassword.Api
import Internal.Api.Now.Api
import Internal.Api.Request as Request
import Internal.Api.SendMessageEvent.Api
import Internal.Api.Versions.Api
import Internal.Config.Log exposing (Log, log)
import Internal.Tools.Json as Json
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


{-| Get an access token to talk to the Matrix API
-}
getAccessToken : UFTask { a | now : () } { a | accessToken : (), now : () }
getAccessToken c =
    case Context.fromApiFormat c of
        context ->
            case ( Context.mostPopularToken context, context.username, context.password ) of
                ( Just a, _, _ ) ->
                    C.succeed
                        { messages = []
                        , logs = [ log.debug "Using cached access token from Vault" ]
                        , contextChange = Context.setAccessToken a
                        }
                        c

                ( Nothing, Just u, Just p ) ->
                    Internal.Api.LoginWithUsernameAndPassword.Api.loginWithUsernameAndPassword
                        { deviceId = Context.fromApiFormat c |> .deviceId
                        , enableRefreshToken = Just True -- TODO: Turn this into a setting
                        , initialDeviceDisplayName = Nothing -- TODO: Turn this into a setting
                        , password = p
                        , username = u
                        }
                        c

                ( Nothing, Nothing, _ ) ->
                    C.fail Request.MissingUsername c

                ( Nothing, Just _, Nothing ) ->
                    C.fail Request.MissingPassword c


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


{-| Get the current timestamp
-}
getNow : UFTask { a | baseUrl : () } { a | baseUrl : (), now : () }
getNow =
    Internal.Api.Now.Api.getNow


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


finishTask : UFTask {} b -> Task
finishTask uftask =
    uftask
        |> C.andThen
            (C.succeed
                { messages = []
                , logs = []
                , contextChange = Context.reset
                }
            )
        |> C.catchWith
            (\_ ->
                { messages = [] -- TODO: Maybe categorize errors?
                , logs = [ log.warn "Encountered unhandled error" ]
                , contextChange = Context.reset
                }
            )


{-| Establish a Task Chain context where the base URL and supported list of
versions are known.
-}
makeVB : UFTask a { a | baseUrl : (), versions : () }
makeVB =
    C.andThen getVersions getBaseUrl


{-| Establish a Task Chain context where the base URL and supported list of
versions are known, and where an access token is available to make an
authenticated API call.
-}
makeVBA : UFTask a { a | accessToken : (), baseUrl : (), now : (), versions : () }
makeVBA =
    makeVB
        |> C.andThen getNow
        |> C.andThen getAccessToken


{-| Send a message event to a room.
-}
sendMessageEvent : { content : Json.Value, eventType : String, roomId : String, transactionId : String } -> Task
sendMessageEvent input =
    makeVBA
        |> C.andThen (Internal.Api.SendMessageEvent.Api.sendMessageEvent input)
        |> finishTask


{-| Transform a completed task into a Cmd.
-}
run : (Backpack -> msg) -> Task -> APIContext {} -> Cmd msg
run toMsg task context =
    context
        |> C.toTask task
        |> Task.perform toMsg
