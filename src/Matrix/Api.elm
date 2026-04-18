module Matrix.Api exposing
    ( discoverHomeserver, getSupportInfo, SupportInfo, SupportContact
    , fetchServerVersion
    , checkUsernameAvailable, checkRegistrationToken
    , listLoginMethods, loginWithPassword, loginWithToken, login
    , LoginFlow, LoginResponse, LoginIdentifier, encodeLoginIdentifier
    , refreshTokens, RefreshTokensResponse
    , logoutDevice, logoutEverywhere
    , fetchAuthorizationMetadata, AuthorizationServerMetadata
    , banUser, banUserWithServerVersion
    , inviteUser, inviteUserWithServerVersion
    , kickUser, kickUserWithServerVersion
    , leaveRoom, leaveRoomWithServerVersion
    , fetchEvent, fetchEventWithServerVersion
    , supportContactDecoder, supportInfoDecoder
    , loginFlowDecoder, loginResponseDecoder
    , refreshTokensResponseDecoder
    , authorizationServerMetadataDecoder
    )

{-| Client-Server API

This module is your high-level entry point for talking to Matrix homeservers.
It re-exports friendly, curated functions from specialized submodules so you can
get going quickly and still drop down into focused modules when you want more
clarity.


## Quick Start

A minimal flow looks like:

    import Matrix.Api as Api

    discover : Cmd Msg
    discover =
        Api.discoverHomeserver
            { url = Url.fromString "https://matrix.org" |> Maybe.withDefault dummy
            , toMsg = GotDiscovery
            }

From there you typically:

1.  list login methods with `listLoginMethods`
2.  log in with `loginWithPassword` or `loginWithToken`
3.  refresh tokens with `refreshTokens` if needed


## Table Of Contents

  - Discovery: `Matrix.Api.Discovery`
  - Versions: `Matrix.Api.Versions`
  - Registration: `Matrix.Api.Registration`
  - Login: `Matrix.Api.Login`
  - Tokens: `Matrix.Api.Tokens`
  - Sessions: `Matrix.Api.Sessions`
  - Authorization: `Matrix.Api.Authorization`
  - Room Membership: `Matrix.Api.RoomMembership`
  - Events: `Matrix.Api.Events`


## Discovery

@docs discoverHomeserver, getSupportInfo, SupportInfo, SupportContact


## Versions

@docs fetchServerVersion


## Registration

@docs checkUsernameAvailable, checkRegistrationToken


## Login

@docs listLoginMethods, loginWithPassword, loginWithToken, login
@docs LoginFlow, LoginResponse, LoginIdentifier, encodeLoginIdentifier


## Tokens

@docs refreshTokens, RefreshTokensResponse


## Sessions

@docs logoutDevice, logoutEverywhere


## Authorization

@docs fetchAuthorizationMetadata, AuthorizationServerMetadata


## Room Membership

@docs banUser, banUserWithServerVersion
@docs inviteUser, inviteUserWithServerVersion
@docs kickUser, kickUserWithServerVersion
@docs leaveRoom, leaveRoomWithServerVersion


## Events

@docs fetchEvent, fetchEventWithServerVersion


## JSON

These decoders are exposed for advanced customization.

@docs supportContactDecoder, supportInfoDecoder
@docs loginFlowDecoder, loginResponseDecoder
@docs refreshTokensResponseDecoder
@docs authorizationServerMetadataDecoder

-}

import Json.Decode as D
import Json.Encode as E
import Matrix.Api.Authorization as Authorization
import Matrix.Api.Discovery as Discovery
import Matrix.Api.Events as Events
import Matrix.Api.Login as Login
import Matrix.Api.Registration as Registration
import Matrix.Api.RoomMembership as RoomMembership
import Matrix.Api.Sessions as Sessions
import Matrix.Api.Tokens as Tokens
import Matrix.Api.Versions as Versions
import Matrix.Http exposing (Error)
import Matrix.Types.AccessToken exposing (AccessToken)
import Matrix.Types.ClientEvent exposing (ClientEvent)
import Matrix.Types.DeviceId exposing (DeviceId)
import Matrix.Types.DiscoveryInformation exposing (DiscoveryInformation)
import Matrix.Types.EventId exposing (EventId)
import Matrix.Types.HomeserverUrl exposing (HomeserverUrl)
import Matrix.Types.RefreshToken exposing (RefreshToken)
import Matrix.Types.RoomId exposing (RoomId)
import Matrix.Types.ServerVersion exposing (ServerVersion)
import Matrix.Types.UserId exposing (UserId)
import Url exposing (Url)


{-| See `Matrix.Api.Discovery.discoverHomeserver`.
-}
discoverHomeserver :
    { url : Url
    , toMsg : Result Error DiscoveryInformation -> msg
    }
    -> Cmd msg
discoverHomeserver =
    Discovery.discoverHomeserver


{-| See `Matrix.Api.Discovery.getSupportInfo`.
-}
getSupportInfo :
    { url : Url
    , toMsg : Result Error SupportInfo -> msg
    }
    -> Cmd msg
getSupportInfo =
    Discovery.getSupportInfo


{-| See `Matrix.Api.Versions.fetchServerVersion`.
-}
fetchServerVersion :
    { accessToken : Maybe AccessToken
    , baseUrl : HomeserverUrl
    , toMsg : Result Error ServerVersion -> msg
    }
    -> Cmd msg
fetchServerVersion =
    Versions.fetchServerVersion


{-| See `Matrix.Api.Registration.checkUsernameAvailable`.
-}
checkUsernameAvailable :
    { baseUrl : HomeserverUrl
    , username : String
    , toMsg : Result Error Bool -> msg
    }
    -> Cmd msg
checkUsernameAvailable =
    Registration.checkUsernameAvailable


{-| See `Matrix.Api.Registration.checkRegistrationToken`.
-}
checkRegistrationToken :
    { baseUrl : HomeserverUrl
    , registrationToken : String
    , toMsg : Result Error Bool -> msg
    }
    -> Cmd msg
checkRegistrationToken =
    Registration.checkRegistrationToken


{-| See `Matrix.Api.Login.listLoginMethods`.
-}
listLoginMethods :
    { baseUrl : HomeserverUrl
    , toMsg : Result Error (List LoginFlow) -> msg
    }
    -> Cmd msg
listLoginMethods =
    Login.listLoginMethods


{-| See `Matrix.Api.Login.loginWithPassword`.
-}
loginWithPassword :
    { baseUrl : HomeserverUrl
    , deviceId : Maybe DeviceId
    , identifier : LoginIdentifier
    , initialDeviceDisplayName : Maybe String
    , password : String
    , supportsRefreshTokens : Maybe Bool
    , toMsg : Result Error LoginResponse -> msg
    }
    -> Cmd msg
loginWithPassword =
    Login.loginWithPassword


{-| See `Matrix.Api.Login.loginWithToken`.
-}
loginWithToken :
    { baseUrl : HomeserverUrl
    , deviceId : Maybe DeviceId
    , identifier : LoginIdentifier
    , initialDeviceDisplayName : Maybe String
    , supportsRefreshTokens : Maybe Bool
    , token : String
    , toMsg : Result Error LoginResponse -> msg
    }
    -> Cmd msg
loginWithToken =
    Login.loginWithToken


{-| See `Matrix.Api.Login.login`.
-}
login :
    { baseUrl : HomeserverUrl
    , deviceId : Maybe DeviceId
    , identifier : LoginIdentifier
    , initialDeviceDisplayName : Maybe String
    , password : Maybe String
    , supportsRefreshTokens : Maybe Bool
    , token : Maybe String
    , type_ : String
    , toMsg : Result Error LoginResponse -> msg
    }
    -> Cmd msg
login =
    Login.login


{-| See `Matrix.Api.Login.encodeLoginIdentifier`.
-}
encodeLoginIdentifier : LoginIdentifier -> E.Value
encodeLoginIdentifier =
    Login.encodeLoginIdentifier


{-| See `Matrix.Api.Tokens.refreshTokens`.
-}
refreshTokens :
    { baseUrl : HomeserverUrl
    , refreshToken : RefreshToken
    , toMsg : Result Error RefreshTokensResponse -> msg
    }
    -> Cmd msg
refreshTokens =
    Tokens.refreshTokens


{-| See `Matrix.Api.Sessions.logoutDevice`.
-}
logoutDevice :
    { accessToken : AccessToken
    , baseUrl : HomeserverUrl
    , toMsg : Result Error () -> msg
    }
    -> Cmd msg
logoutDevice =
    Sessions.logoutDevice


{-| See `Matrix.Api.Sessions.logoutEverywhere`.
-}
logoutEverywhere :
    { accessToken : AccessToken
    , baseUrl : HomeserverUrl
    , toMsg : Result Error () -> msg
    }
    -> Cmd msg
logoutEverywhere =
    Sessions.logoutEverywhere


{-| See `Matrix.Api.Authorization.fetchAuthorizationMetadata`.
-}
fetchAuthorizationMetadata :
    { baseUrl : HomeserverUrl
    , toMsg : Result Error AuthorizationServerMetadata -> msg
    }
    -> Cmd msg
fetchAuthorizationMetadata =
    Authorization.fetchAuthorizationMetadata


{-| See `Matrix.Api.RoomMembership.banUser`.
-}
banUser :
    { accessToken : AccessToken
    , baseUrl : HomeserverUrl
    , roomId : RoomId
    , userId : UserId
    , reason : Maybe String
    , toMsg : Result Error () -> msg
    }
    -> Cmd msg
banUser =
    RoomMembership.banUser


{-| See `Matrix.Api.RoomMembership.banUserWithServerVersion`.
-}
banUserWithServerVersion :
    { accessToken : AccessToken
    , baseUrl : HomeserverUrl
    , roomId : RoomId
    , userId : UserId
    , reason : Maybe String
    , serverVersion : ServerVersion
    , toMsg : Result Error () -> msg
    }
    -> Cmd msg
banUserWithServerVersion =
    RoomMembership.banUserWithServerVersion


{-| See `Matrix.Api.RoomMembership.inviteUser`.
-}
inviteUser :
    { accessToken : AccessToken
    , baseUrl : HomeserverUrl
    , roomId : RoomId
    , userId : UserId
    , reason : Maybe String
    , toMsg : Result Error () -> msg
    }
    -> Cmd msg
inviteUser =
    RoomMembership.inviteUser


{-| See `Matrix.Api.RoomMembership.inviteUserWithServerVersion`.
-}
inviteUserWithServerVersion :
    { accessToken : AccessToken
    , baseUrl : HomeserverUrl
    , roomId : RoomId
    , userId : UserId
    , reason : Maybe String
    , serverVersion : ServerVersion
    , toMsg : Result Error () -> msg
    }
    -> Cmd msg
inviteUserWithServerVersion =
    RoomMembership.inviteUserWithServerVersion


{-| See `Matrix.Api.RoomMembership.kickUser`.
-}
kickUser :
    { accessToken : AccessToken
    , baseUrl : HomeserverUrl
    , roomId : RoomId
    , userId : UserId
    , reason : Maybe String
    , avatarUrl : Maybe String
    , displayName : Maybe String
    , toMsg : Result Error () -> msg
    }
    -> Cmd msg
kickUser =
    RoomMembership.kickUser


{-| See `Matrix.Api.RoomMembership.kickUserWithServerVersion`.
-}
kickUserWithServerVersion :
    { accessToken : AccessToken
    , baseUrl : HomeserverUrl
    , roomId : RoomId
    , userId : UserId
    , reason : Maybe String
    , avatarUrl : Maybe String
    , displayName : Maybe String
    , serverVersion : ServerVersion
    , toMsg : Result Error () -> msg
    }
    -> Cmd msg
kickUserWithServerVersion =
    RoomMembership.kickUserWithServerVersion


{-| See `Matrix.Api.RoomMembership.leaveRoom`.
-}
leaveRoom :
    { accessToken : AccessToken
    , baseUrl : HomeserverUrl
    , roomId : RoomId
    , reason : Maybe String
    , toMsg : Result Error () -> msg
    }
    -> Cmd msg
leaveRoom =
    RoomMembership.leaveRoom


{-| See `Matrix.Api.RoomMembership.leaveRoomWithServerVersion`.
-}
leaveRoomWithServerVersion :
    { accessToken : AccessToken
    , baseUrl : HomeserverUrl
    , roomId : RoomId
    , reason : Maybe String
    , serverVersion : ServerVersion
    , toMsg : Result Error () -> msg
    }
    -> Cmd msg
leaveRoomWithServerVersion =
    RoomMembership.leaveRoomWithServerVersion


{-| See `Matrix.Api.Events.fetchEvent`.
-}
fetchEvent :
    { accessToken : AccessToken
    , baseUrl : HomeserverUrl
    , eventId : EventId
    , roomId : RoomId
    , toMsg : Result Error ClientEvent -> msg
    }
    -> Cmd msg
fetchEvent =
    Events.fetchEvent


{-| See `Matrix.Api.Events.fetchEventWithServerVersion`.
-}
fetchEventWithServerVersion :
    { accessToken : AccessToken
    , baseUrl : HomeserverUrl
    , eventId : EventId
    , roomId : RoomId
    , serverVersion : ServerVersion
    , toMsg : Result Error ClientEvent -> msg
    }
    -> Cmd msg
fetchEventWithServerVersion =
    Events.fetchEventWithServerVersion


{-| Decode support contact information.
-}
supportContactDecoder : D.Decoder SupportContact
supportContactDecoder =
    Discovery.supportContactDecoder


{-| Decode support information.
-}
supportInfoDecoder : D.Decoder SupportInfo
supportInfoDecoder =
    Discovery.supportInfoDecoder


{-| Decode login flows.
-}
loginFlowDecoder : D.Decoder LoginFlow
loginFlowDecoder =
    Login.loginFlowDecoder


{-| Decode login responses.
-}
loginResponseDecoder : D.Decoder LoginResponse
loginResponseDecoder =
    Login.loginResponseDecoder


{-| Decode refresh-token responses.
-}
refreshTokensResponseDecoder : D.Decoder RefreshTokensResponse
refreshTokensResponseDecoder =
    Tokens.refreshTokensResponseDecoder


{-| Decode authorization metadata.
-}
authorizationServerMetadataDecoder : D.Decoder AuthorizationServerMetadata
authorizationServerMetadataDecoder =
    Authorization.authorizationServerMetadataDecoder


{-| Support information for a homeserver domain.
-}
type alias SupportInfo =
    Discovery.SupportInfo


{-| A support contact record for a homeserver.
-}
type alias SupportContact =
    Discovery.SupportContact


{-| A login flow advertised by the server.
-}
type alias LoginFlow =
    Login.LoginFlow


{-| Response returned by a successful login.
-}
type alias LoginResponse =
    Login.LoginResponse


{-| Identify which account should log in.
-}
type alias LoginIdentifier =
    Login.LoginIdentifier


{-| Response returned when refreshing an access token.
-}
type alias RefreshTokensResponse =
    Tokens.RefreshTokensResponse


{-| OpenID authorization server metadata.
-}
type alias AuthorizationServerMetadata =
    Authorization.AuthorizationServerMetadata
