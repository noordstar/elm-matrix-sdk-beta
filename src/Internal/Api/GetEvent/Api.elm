module Internal.Api.GetEvent.Api exposing (GetEventInput, getEvent)

{-|


# Get event

Get a single event based on `roomId/eventId`. You must have permission to
retrieve this event e.g. by being a member in the room for this event.

@docs GetEventInput, getEvent

-}

import Internal.Api.Api as A
import Internal.Api.Request as R
import Internal.Config.Log exposing (log)
import Internal.Config.Text as Text
import Internal.Tools.Json as Json
import Internal.Tools.Timestamp as Timestamp
import Internal.Values.Envelope as E
import Internal.Values.Event as Event exposing (Event)
import Internal.Values.Room as Room
import Internal.Values.User as User
import Internal.Values.Vault as V


{-| Input for getting an event.
-}
type alias GetEventInput =
    { eventId : String, roomId : String }


{-| Standard input for version 1 of the GetEvent API endpoint.
-}
type alias GetEventInputV1 a =
    { a | eventId : String, roomId : String }


{-| Universal phantom type encompassing all versions of this API endpoint.
-}
type alias Phantom a =
    PhantomV1 { a | versions : () }


{-| Phantom values necessary for version 1 of the GetEvent API endpoint.
-}
type alias PhantomV1 a =
    { a | accessToken : (), baseUrl : () }


{-| Get an event based on a room id and event id.
-}
getEvent : GetEventInput -> A.TaskChain (Phantom a) (Phantom a)
getEvent =
    A.startWithVersion "r0.5.0" getEventV1
        |> A.sameForVersion "r0.6.0"
        |> A.sameForVersion "r0.6.1"
        |> A.forVersion "v1.1" getEventV2
        |> A.sameForVersion "v1.2"
        |> A.sameForVersion "v1.3"
        |> A.sameForVersion "v1.4"
        |> A.sameForVersion "v1.5"
        |> A.sameForVersion "v1.6"
        |> A.sameForVersion "v1.7"
        |> A.sameForVersion "v1.8"
        |> A.sameForVersion "v1.9"
        |> A.sameForVersion "v1.10"
        |> A.versionChain


{-| Version 1 of the GetEvent API endpoint
-}
getEventV1 : GetEventInputV1 input -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
getEventV1 { eventId, roomId } =
    A.request
        { attributes =
            [ R.accessToken
            , R.onStatusCode 404 "M_NOT_FOUND"
            ]
        , coder = getEventCoderV1
        , contextChange = always identity
        , method = "GET"
        , path = [ "_matrix", "client", "r0", "rooms", roomId, "event", eventId ]
        , toUpdate =
            \event ->
                ( E.ContentUpdate <| V.MapRoom roomId (Room.AddEvent event)
                , event.eventId
                    |> Text.logs.getEventId
                    |> log.debug
                    |> List.singleton
                )
        }


{-| Version 2 of the GetEvent API endpoint
-}
getEventV2 : GetEventInputV1 input -> A.TaskChain (PhantomV1 a) (PhantomV1 a)
getEventV2 { eventId, roomId } =
    A.request
        { attributes =
            [ R.accessToken
            , R.onStatusCode 404 "M_NOT_FOUND"
            ]
        , coder = getEventCoderV1
        , contextChange = always identity
        , method = "GET"
        , path = [ "_matrix", "client", "v3", "rooms", roomId, "event", eventId ]
        , toUpdate =
            \event ->
                ( E.ContentUpdate <| V.MapRoom roomId (Room.AddEvent event)
                , event.eventId
                    |> Text.logs.getEventId
                    |> log.debug
                    |> List.singleton
                )
        }


getEventCoderV1 : Json.Coder Event
getEventCoderV1 =
    Json.object8
        { name = "ClientEvent"
        , description =
            [ "ClientEvent as described by the Matrix spec"
            , "https://spec.matrix.org/v1.10/client-server-api/#get_matrixclientv3roomsroomideventeventid"
            ]
        , init = Event
        }
        (Json.field.required
            { fieldName = "content"
            , toField = .content
            , description =
                [ "The body of this event, as created by the client which sent it."
                ]
            , coder = Json.value
            }
        )
        (Json.field.required
            { fieldName = "event_id"
            , toField = .eventId
            , description =
                [ "The globally unique identifier for this event."
                ]
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "origin_server_ts"
            , toField = .originServerTs
            , description =
                [ "Timestamp (in milliseconds since the unix epoch) on originating homeserver when this event was sent."
                ]
            , coder = Timestamp.coder
            }
        )
        (Json.field.required
            { fieldName = "room_id"
            , toField = .roomId
            , description =
                [ "The ID of the room associated with this event."
                ]
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "sender"
            , toField = .sender
            , description =
                [ "Contains the fully-qualified ID of the user who sent this event."
                ]
            , coder = User.coder
            }
        )
        (Json.field.optional.value
            { fieldName = "state_key"
            , toField = .stateKey
            , description =
                [ "Present if, and only if, this event is a state event. The key making this piece of state unique in the room. Note that it is often an empty string."
                , "State keys starting with an @ are reserved for referencing user IDs, such as room members. With the exception of a few events, state events set with a given user’s ID as the state key MUST only be set by that user."
                ]
            , coder = Json.string
            }
        )
        (Json.field.required
            { fieldName = "type"
            , toField = .eventType
            , description =
                [ "The type of the event."
                ]
            , coder = Json.string
            }
        )
        (Json.field.optional.value
            { fieldName = "unsigned"
            , toField = .unsigned
            , description =
                [ "Contains optional extra information about the event."
                ]
            , coder =
                Json.object4
                    { name = "UnsignedData"
                    , description =
                        [ "UnsignedData as described by the Matrix spec"
                        , "https://spec.matrix.org/v1.10/client-server-api/#get_matrixclientv3roomsroomideventeventid"
                        ]
                    , init = \a b c d -> Event.UnsignedData { age = a, membership = Nothing, prevContent = b, redactedBecause = c, transactionId = d }
                    }
                    (Json.field.optional.value
                        { fieldName = "age"
                        , toField = \(Event.UnsignedData data) -> data.age
                        , description =
                            [ "The time in milliseconds that has elapsed since the event was sent. This field is generated by the local homeserver, and may be incorrect if the local time on at least one of the two servers is out of sync, which can cause the age to either be negative or greater than it actually is."
                            ]
                        , coder = Json.int
                        }
                    )
                    (Json.field.optional.value
                        { fieldName = "prev_content"
                        , toField = \(Event.UnsignedData data) -> data.prevContent
                        , description =
                            [ " The previous content for this event. This field is generated by the local homeserver, and is only returned if the event is a state event, and the client has permission to see the previous content."
                            , "Changed in v1.2: Previously, this field was specified at the top level of returned events rather than in unsigned (with the exception of the GET .../notifications endpoint), though in practice no known server implementations honoured this."
                            ]
                        , coder = Json.value
                        }
                    )
                    (Json.field.optional.value
                        { fieldName = "redacted_because"
                        , toField = \(Event.UnsignedData data) -> data.redactedBecause
                        , description =
                            [ "The event that redacted this event, if any."
                            ]
                        , coder = Json.lazy (\() -> getEventCoderV1)
                        }
                    )
                    (Json.field.optional.value
                        { fieldName = "transaction_id"
                        , toField = \(Event.UnsignedData data) -> data.transactionId
                        , description =
                            [ "The client-supplied transaction ID, for example, provided via PUT /_matrix/client/v3/rooms/{roomId}/send/{eventType}/{txnId}, if the client being given the event is the same one which sent it."
                            ]
                        , coder = Json.string
                        }
                    )
            }
        )
