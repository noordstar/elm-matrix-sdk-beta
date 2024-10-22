module Internal.Api.Task exposing
    ( Task, run, Backpack
    , banUser, inviteUser, kickUser, sendMessageEvent, sendStateEvent, setAccountData, setRoomAccountData, sync, whoAmI
    )

{-|


# Task module

This module is used to define how API calls are made. These completed API tasks
can be directly converted to Cmd types that the end user of the SDK can access.

These tasks do not affect the `Vault` directly, but instead, return a
`VaultUpdate` type that the user can apply to keep their `Vault` type
up-to-date.


## Use

@docs Task, run, Backpack


## Tasks

@docs banUser, inviteUser, kickUser, sendMessageEvent, sendStateEvent, setAccountData, setRoomAccountData, sync, whoAmI

-}

import Internal.Api.BanUser.Api
import Internal.Api.BaseUrl.Api
import Internal.Api.Chain as C
import Internal.Api.InviteUser.Api
import Internal.Api.KickUser.Api
import Internal.Api.LoginWithUsernameAndPassword.Api
import Internal.Api.Now.Api
import Internal.Api.Request as Request
import Internal.Api.SendMessageEvent.Api
import Internal.Api.SendStateEvent.Api
import Internal.Api.SetAccountData.Api
import Internal.Api.SetRoomAccountData.Api
import Internal.Api.Sync.Api
import Internal.Api.Versions.Api
import Internal.Api.WhoAmI.Api
import Internal.Config.Log exposing (Log, log)
import Internal.Config.Text as Text
import Internal.Tools.Json as Json
import Internal.Values.Context as Context exposing (APIContext)
import Internal.Values.Envelope as E exposing (EnvelopeUpdate(..))
import Internal.Values.Room exposing (RoomUpdate(..))
import Internal.Values.User exposing (User)
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


{-| Ban a user from a room.
-}
banUser : { reason : Maybe String, roomId : String, user : User } -> Task
banUser input =
    makeVBA
        |> C.andThen (Internal.Api.BanUser.Api.banUser input)
        |> finishTask


{-| Get an access token to talk to the Matrix API
-}
getAccessToken : UFTask { a | baseUrl : (), now : (), versions : () } { a | accessToken : (), baseUrl : (), now : (), versions : () }
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
                |> C.catchWith
                    (\_ ->
                        let
                            url : String
                            url =
                                Context.fromApiFormat c
                                    |> .serverName
                        in
                        { contextChange = Context.setBaseUrl url
                        , logs = [ log.warn (Text.logs.baseUrlFailed url) ]
                        , messages = [ E.SetBaseUrl url ]
                        }
                    )
                |> (|>) c


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
            (\e ->
                case e of
                    Request.MissingPassword ->
                        { messages = []
                        , logs = [ log.error "Cannot log in - password is missing" ]
                        , contextChange = Context.reset
                        }

                    Request.MissingUsername ->
                        { messages = []
                        , logs = [ log.error "Cannot log in - username is missing" ]
                        , contextChange = Context.reset
                        }

                    Request.NoSupportedVersion ->
                        { messages = []
                        , logs = [ log.error "No supported version is available to complete the API interaction." ]
                        , contextChange = Context.reset
                        }

                    Request.ServerReturnsBadJSON t ->
                        { messages = []
                        , logs = [ log.error ("The server returned invalid JSON: " ++ t) ]
                        , contextChange = Context.reset
                        }

                    Request.ServerReturnsError name _ ->
                        { messages = []
                        , logs = [ log.error ("The server returns an error: " ++ name) ]
                        , contextChange = Context.reset
                        }

                    _ ->
                        { messages = [] -- TODO: Maybe categorize errors?
                        , logs = [ log.warn "Encountered unhandled error" ]
                        , contextChange = Context.reset
                        }
            )


{-| Invite a user to a room.
-}
inviteUser : { reason : Maybe String, roomId : String, user : User } -> Task
inviteUser input =
    makeVBA
        |> C.andThen (Internal.Api.InviteUser.Api.inviteUser input)
        |> finishTask


{-| Kick a user from a room.
-}
kickUser :
    { avatarUrl : Maybe String
    , displayname : Maybe String
    , reason : Maybe String
    , roomId : String
    , user : User
    }
    -> Task
kickUser input =
    makeVBA
        |> C.andThen (Internal.Api.KickUser.Api.kickUser input)
        |> finishTask


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


{-| Send a state event to a room.
-}
sendStateEvent : { content : Json.Value, eventType : String, roomId : String, stateKey : String } -> Task
sendStateEvent input =
    makeVBA
        |> C.andThen (Internal.Api.SendStateEvent.Api.sendStateEvent input)
        |> finishTask


{-| Set global account data.
-}
setAccountData : { content : Json.Value, eventType : String, userId : String } -> Task
setAccountData input =
    makeVBA
        |> C.andThen (Internal.Api.SetAccountData.Api.setAccountData input)
        |> finishTask


{-| Set account data for a Matrix room.
-}
setRoomAccountData : { content : Json.Value, eventType : String, roomId : String, userId : String } -> Task
setRoomAccountData input =
    makeVBA
        |> C.andThen (Internal.Api.SetRoomAccountData.Api.setRoomAccountData input)
        |> finishTask


{-| Sync with the Matrix API to stay up-to-date.
-}
sync : { fullState : Maybe Bool, presence : Maybe String, since : Maybe String, timeout : Maybe Int } -> Task
sync input =
    makeVBA
        |> C.andThen (Internal.Api.Sync.Api.sync input)
        |> finishTask


{-| Reveal personal information about the account to the user.
-}
whoAmI : Task
whoAmI =
    makeVBA
        |> C.andThen (Internal.Api.WhoAmI.Api.whoAmI {})
        |> finishTask


{-| Transform a completed task into a Cmd.
-}
run : (Backpack -> msg) -> Task -> APIContext {} -> Cmd msg
run toMsg task context =
    context
        |> C.toTask task
        |> Task.perform toMsg
