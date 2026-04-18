module Matrix.Api.RoomMembership exposing
    ( banUser, banUserWithServerVersion
    , inviteUser, inviteUserWithServerVersion
    , kickUser, kickUserWithServerVersion
    , leaveRoom, leaveRoomWithServerVersion
    )

{-| Room membership and moderation actions.

These helpers let you invite, kick, ban, or leave rooms while adapting to the
server's advertised Matrix spec version.


## Room Membership

@docs banUser, banUserWithServerVersion
@docs inviteUser, inviteUserWithServerVersion
@docs kickUser, kickUserWithServerVersion
@docs leaveRoom, leaveRoomWithServerVersion

-}

import Json.Encode as E
import Matrix.Api.Internal.RoomMembership as RoomMembership
import Matrix.Http exposing (Error)
import Matrix.Types.AccessToken exposing (AccessToken)
import Matrix.Types.HomeserverUrl exposing (HomeserverUrl)
import Matrix.Types.RoomId as RoomId exposing (RoomId)
import Matrix.Types.ServerVersion as ServerVersion exposing (ServerVersion)
import Matrix.Types.UserId as UserId exposing (UserId)


{-| Ban a user from a room using the latest-compatible endpoint profile.
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
banUser data =
    banUserWithServerVersion
        { accessToken = data.accessToken
        , baseUrl = data.baseUrl
        , roomId = data.roomId
        , userId = data.userId
        , reason = data.reason
        , serverVersion = ServerVersion.naive
        , toMsg = data.toMsg
        }


{-| Ban a user from a room using an explicit server version profile.
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
banUserWithServerVersion data =
    let
        profile =
            RoomMembership.selectBanProfile data.serverVersion
    in
    Matrix.Http.post
        { accessToken = Just data.accessToken
        , baseUrl = data.baseUrl
        , path =
            [ "_matrix"
            , "client"
            , profile.clientPathVersion
            , "rooms"
            , RoomId.toString data.roomId
            , "ban"
            ]
        , body =
            E.object
                ([ Just ( "user_id", E.string (UserId.toString data.userId) )
                 , maybeField "reason" E.string data.reason
                 ]
                    |> List.filterMap identity
                )
        , expect = Matrix.Http.expectWhatever data.toMsg
        }


{-| Invite a user to a room using the latest-compatible endpoint profile.
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
inviteUser data =
    inviteUserWithServerVersion
        { accessToken = data.accessToken
        , baseUrl = data.baseUrl
        , roomId = data.roomId
        , userId = data.userId
        , reason = data.reason
        , serverVersion = ServerVersion.naive
        , toMsg = data.toMsg
        }


{-| Invite a user to a room using an explicit server version profile.
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
inviteUserWithServerVersion data =
    let
        profile =
            RoomMembership.selectInviteProfile data.serverVersion
    in
    Matrix.Http.post
        { accessToken = Just data.accessToken
        , baseUrl = data.baseUrl
        , path =
            [ "_matrix"
            , "client"
            , profile.clientPathVersion
            , "rooms"
            , RoomId.toString data.roomId
            , "invite"
            ]
        , body =
            E.object
                ([ Just ( "user_id", E.string (UserId.toString data.userId) )
                 , optionalField profile.includeReason "reason" E.string data.reason
                 ]
                    |> List.filterMap identity
                )
        , expect = Matrix.Http.expectWhatever data.toMsg
        }


{-| Kick a user from a room using the latest-compatible endpoint profile.
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
kickUser data =
    kickUserWithServerVersion
        { accessToken = data.accessToken
        , baseUrl = data.baseUrl
        , roomId = data.roomId
        , userId = data.userId
        , reason = data.reason
        , avatarUrl = data.avatarUrl
        , displayName = data.displayName
        , serverVersion = ServerVersion.naive
        , toMsg = data.toMsg
        }


{-| Kick a user from a room using an explicit server version profile.
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
kickUserWithServerVersion data =
    let
        profile =
            RoomMembership.selectKickProfile data.serverVersion
    in
    Matrix.Http.request
        { method =
            case profile.action of
                RoomMembership.LegacyMembershipFallback ->
                    "PUT"

                RoomMembership.KickEndpoint ->
                    "POST"
        , accessToken = Just data.accessToken
        , baseUrl = data.baseUrl
        , path =
            kickPath profile data.roomId data.userId
        , query = []
        , body =
            Just
                (E.object
                    (kickBodyFields profile data
                        |> List.filterMap identity
                    )
                )
        , expect = Matrix.Http.expectWhatever data.toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Leave a room using the latest-compatible endpoint profile.
-}
leaveRoom :
    { accessToken : AccessToken
    , baseUrl : HomeserverUrl
    , roomId : RoomId
    , reason : Maybe String
    , toMsg : Result Error () -> msg
    }
    -> Cmd msg
leaveRoom data =
    leaveRoomWithServerVersion
        { accessToken = data.accessToken
        , baseUrl = data.baseUrl
        , roomId = data.roomId
        , reason = data.reason
        , serverVersion = ServerVersion.naive
        , toMsg = data.toMsg
        }


{-| Leave a room using an explicit server version profile.
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
leaveRoomWithServerVersion data =
    let
        profile =
            RoomMembership.selectLeaveProfile data.serverVersion
    in
    Matrix.Http.post
        { accessToken = Just data.accessToken
        , baseUrl = data.baseUrl
        , path =
            [ "_matrix"
            , "client"
            , profile.clientPathVersion
            , "rooms"
            , RoomId.toString data.roomId
            , "leave"
            ]
        , body =
            E.object
                ([ optionalField profile.includeReason "reason" E.string data.reason ]
                    |> List.filterMap identity
                )
        , expect = Matrix.Http.expectWhatever data.toMsg
        }


kickPath : RoomMembership.KickProfile -> RoomId -> UserId -> List String
kickPath profile roomId userId =
    case profile.action of
        RoomMembership.LegacyMembershipFallback ->
            [ "_matrix"
            , "client"
            , profile.clientPathVersion
            , "rooms"
            , RoomId.toString roomId
            , "state"
            , "m.room.member"
            , UserId.toString userId
            ]

        RoomMembership.KickEndpoint ->
            [ "_matrix"
            , "client"
            , profile.clientPathVersion
            , "rooms"
            , RoomId.toString roomId
            , "kick"
            ]


kickBodyFields :
    RoomMembership.KickProfile
    ->
        { a
            | avatarUrl : Maybe String
            , displayName : Maybe String
            , reason : Maybe String
            , userId : UserId
        }
    -> List (Maybe ( String, E.Value ))
kickBodyFields profile data =
    case profile.action of
        RoomMembership.LegacyMembershipFallback ->
            [ Just ( "membership", E.string "kick" )
            , maybeField "avatar_url" E.string data.avatarUrl
            , maybeField "displayname" E.string data.displayName
            , maybeField "reason" E.string data.reason
            ]

        RoomMembership.KickEndpoint ->
            [ Just ( "user_id", E.string (UserId.toString data.userId) )
            , optionalField profile.includeReason "reason" E.string data.reason
            ]


optionalField : Bool -> String -> (a -> E.Value) -> Maybe a -> Maybe ( String, E.Value )
optionalField includeField fieldName toValue value =
    if includeField then
        maybeField fieldName toValue value

    else
        Nothing


maybeField : String -> (a -> E.Value) -> Maybe a -> Maybe ( String, E.Value )
maybeField fieldName toValue value =
    value |> Maybe.map (\v -> ( fieldName, toValue v ))
