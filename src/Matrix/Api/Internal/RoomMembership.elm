module Matrix.Api.Internal.RoomMembership exposing
    ( BanProfile
    , InviteProfile
    , KickAction(..)
    , KickProfile
    , LeaveProfile
    , selectBanProfile
    , selectInviteProfile
    , selectKickProfile
    , selectLeaveProfile
    )

{-| Version-specific behavior for room membership endpoints.
-}

import Matrix.Api.Internal.VersionedEndpoint as VersionedEndpoint exposing (Match(..), Variant)
import Matrix.Types.ServerVersion exposing (ServerVersion)


type alias BanProfile =
    { clientPathVersion : String
    }


type alias InviteProfile =
    { clientPathVersion : String
    , includeReason : Bool
    }


type KickAction
    = LegacyMembershipFallback
    | KickEndpoint


type alias KickProfile =
    { action : KickAction
    , clientPathVersion : String
    , includeReason : Bool
    }


type alias LeaveProfile =
    { clientPathVersion : String
    , includeReason : Bool
    }


selectBanProfile : ServerVersion -> BanProfile
selectBanProfile serverVersion =
    VersionedEndpoint.selectVariant serverVersion banVariants
        |> Maybe.withDefault latestBanProfile


selectInviteProfile : ServerVersion -> InviteProfile
selectInviteProfile serverVersion =
    VersionedEndpoint.selectVariant serverVersion inviteVariants
        |> Maybe.withDefault latestInviteProfile


selectKickProfile : ServerVersion -> KickProfile
selectKickProfile serverVersion =
    VersionedEndpoint.selectVariant serverVersion kickVariants
        |> Maybe.withDefault latestKickProfile


selectLeaveProfile : ServerVersion -> LeaveProfile
selectLeaveProfile serverVersion =
    VersionedEndpoint.selectVariant serverVersion leaveVariants
        |> Maybe.withDefault latestLeaveProfile


latestBanProfile : BanProfile
latestBanProfile =
    { clientPathVersion = "v3"
    }


latestInviteProfile : InviteProfile
latestInviteProfile =
    { clientPathVersion = "v3"
    , includeReason = True
    }


latestKickProfile : KickProfile
latestKickProfile =
    { action = KickEndpoint
    , clientPathVersion = "v3"
    , includeReason = True
    }


latestLeaveProfile : LeaveProfile
latestLeaveProfile =
    { clientPathVersion = "v3"
    , includeReason = True
    }


banVariants : List (Variant BanProfile)
banVariants =
    [ { matches = stableVersionsV1x
      , value = latestBanProfile
      }
    , { matches = stableVersionsLegacyR0Ban
      , value = { clientPathVersion = "r0" }
      }
    ]


inviteVariants : List (Variant InviteProfile)
inviteVariants =
    [ { matches = stableVersionsV1x
      , value = latestInviteProfile
      }
    , { matches = stableVersionsLegacyR0Invite
      , value =
            { clientPathVersion = "r0"
            , includeReason = False
            }
      }
    ]


kickVariants : List (Variant KickProfile)
kickVariants =
    [ { matches = stableVersionsV1x
      , value = latestKickProfile
      }
    , { matches = stableVersionsLegacyR0KickEndpoint
      , value =
            { action = KickEndpoint
            , clientPathVersion = "r0"
            , includeReason = True
            }
      }
    , { matches = stableVersionsLegacyR0KickFallback
      , value =
            { action = LegacyMembershipFallback
            , clientPathVersion = "r0"
            , includeReason = True
            }
      }
    ]


leaveVariants : List (Variant LeaveProfile)
leaveVariants =
    [ { matches = stableVersionsV1x
      , value = latestLeaveProfile
      }
    , { matches = stableVersionsLegacyR0LeaveNoReason
      , value =
            { clientPathVersion = "r0"
            , includeReason = False
            }
      }
    ]


stableVersionsV1x : List Match
stableVersionsV1x =
    [ "v1.1"
    , "v1.2"
    , "v1.3"
    , "v1.4"
    , "v1.5"
    , "v1.6"
    , "v1.7"
    , "v1.8"
    , "v1.9"
    , "v1.10"
    , "v1.11"
    , "v1.12"
    , "v1.13"
    , "v1.14"
    , "v1.15"
    , "v1.16"
    , "v1.17"
    , "v1.18"
    ]
        |> List.map StableVersion


stableVersionsLegacyR0Ban : List Match
stableVersionsLegacyR0Ban =
    [ "r0.0.0"
    , "r0.0.1"
    , "r0.1.0"
    , "r0.2.0"
    , "r0.3.0"
    , "r0.4.0"
    , "r0.5.0"
    , "r0.6.0"
    , "r0.6.1"
    ]
        |> List.map StableVersion


stableVersionsLegacyR0Invite : List Match
stableVersionsLegacyR0Invite =
    [ "r0.0.0"
    , "r0.0.1"
    , "r0.1.0"
    , "r0.2.0"
    , "r0.3.0"
    , "r0.4.0"
    , "r0.5.0"
    , "r0.6.0"
    , "r0.6.1"
    ]
        |> List.map StableVersion


stableVersionsLegacyR0KickEndpoint : List Match
stableVersionsLegacyR0KickEndpoint =
    [ "r0.1.0"
    , "r0.2.0"
    , "r0.3.0"
    , "r0.4.0"
    , "r0.5.0"
    , "r0.6.0"
    , "r0.6.1"
    ]
        |> List.map StableVersion


stableVersionsLegacyR0KickFallback : List Match
stableVersionsLegacyR0KickFallback =
    [ "r0.0.0"
    , "r0.0.1"
    ]
        |> List.map StableVersion


stableVersionsLegacyR0LeaveNoReason : List Match
stableVersionsLegacyR0LeaveNoReason =
    [ "r0.0.0"
    , "r0.0.1"
    , "r0.1.0"
    , "r0.2.0"
    , "r0.3.0"
    , "r0.4.0"
    , "r0.5.0"
    , "r0.6.0"
    , "r0.6.1"
    ]
        |> List.map StableVersion
