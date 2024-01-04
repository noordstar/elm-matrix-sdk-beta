module Internal.Values.Event exposing
    ( Event
    , UnsignedData(..), age, prevContent, redactedBecause, transactionId
    , encode, decoder
    , isEqual
    )

{-|


# Event

The `Event` module hosts all the information for a single event in the timeline
of a room.

@docs Event


## Unsigned data

@docs UnsignedData, age, prevContent, redactedBecause, transactionId


## JSON Coder

@docs encode, decoder


## Test functions

@docs isEqual

-}

import Internal.Config.Default as Default
import Internal.Tools.Decode as D
import Internal.Tools.Encode as E
import Internal.Tools.Timestamp as Timestamp exposing (Timestamp)
import Json.Decode as D
import Json.Encode as E


{-| The Event type occurs everywhere on a user's timeline.
-}
type alias Event =
    { content : E.Value
    , eventId : String
    , originServerTs : Timestamp
    , roomId : String
    , sender : String
    , stateKey : Maybe String
    , eventType : String
    , unsigned : Maybe UnsignedData
    }


{-| Unsigned Data contains a lot of extra information. You can access it through
helper functions.
-}
type UnsignedData
    = UnsignedData
        { age : Maybe Int
        , prevContent : Maybe E.Value
        , redactedBecause : Maybe Event
        , transactionId : Maybe String
        }


{-| Get the event's age, if at all provided by the homeserver.
-}
age : Event -> Maybe Int
age event =
    Maybe.andThen (\(UnsignedData data) -> data.age) event.unsigned


{-| Decode an Event from a JSON value.
-}
decoder : D.Decoder Event
decoder =
    D.map8 Event
        (D.field "content" D.value)
        (D.field "eventId" D.string)
        (D.field "originServerTs" Timestamp.decoder)
        (D.field "roomId" D.string)
        (D.field "sender" D.string)
        (D.opField "stateKey" D.string)
        (D.field "eventType" D.string)
        (D.opField "unsigned" decoderUnsignedData)


{-| Decode Unsigned Data from a JSON value.
-}
decoderUnsignedData : D.Decoder UnsignedData
decoderUnsignedData =
    D.map4 (\a b c d -> UnsignedData { age = a, prevContent = b, redactedBecause = c, transactionId = d })
        (D.opField "age" D.int)
        (D.opField "prevContent" D.value)
        (D.opField "redactedBecause" (D.lazy (\_ -> decoder)))
        (D.opField "transactionId" D.string)


{-| Encode an Event into a JSON value.
-}
encode : Event -> E.Value
encode event =
    E.maybeObject
        [ ( "content", Just event.content )
        , ( "eventId", Just <| E.string event.eventId )
        , ( "originServerTs", Just <| Timestamp.encode event.originServerTs )
        , ( "roomId", Just <| E.string event.roomId )
        , ( "sender", Just <| E.string event.sender )
        , ( "stateKey", Maybe.map E.string event.stateKey )
        , ( "eventType", Just <| E.string event.eventType )
        , ( "unsigned", Maybe.map encodeUnsignedData event.unsigned )
        , ( "version", Just <| E.string Default.currentVersion )
        ]


{-| Encode Unsigned Data into a JSON value.
-}
encodeUnsignedData : UnsignedData -> E.Value
encodeUnsignedData (UnsignedData data) =
    E.maybeObject
        [ ( "age", Maybe.map E.int data.age )
        , ( "prevContent", data.prevContent )
        , ( "redactedBecause", Maybe.map encode data.redactedBecause )
        , ( "transactionId", Maybe.map E.string data.transactionId )
        ]


{-| Compare two events and determine whether they're identical. Used mostly for
testing purposes.
-}
isEqual : Event -> Event -> Bool
isEqual e1 e2 =
    if e1.eventId /= e2.eventId then
        False

    else if e1.originServerTs /= e2.originServerTs then
        False

    else if e1.roomId /= e2.roomId then
        False

    else if e1.sender /= e2.sender then
        False

    else if e1.stateKey /= e2.stateKey then
        False

    else if e1.eventType /= e2.eventType then
        False

    else
        case ( e1.unsigned, e2.unsigned ) of
            ( Nothing, Nothing ) ->
                True

            ( Just _, Nothing ) ->
                False

            ( Nothing, Just _ ) ->
                False

            ( Just (UnsignedData d1), Just (UnsignedData d2) ) ->
                if d1.age /= d2.age then
                    False

                else if d1.transactionId /= d2.transactionId then
                    False

                else if Maybe.map (E.encode 0) d1.prevContent /= Maybe.map (E.encode 0) d2.prevContent then
                    False

                else
                    case ( d1.redactedBecause, d2.redactedBecause ) of
                        ( Nothing, Nothing ) ->
                            True

                        ( Nothing, Just _ ) ->
                            False

                        ( Just _, Nothing ) ->
                            False

                        ( Just se1, Just se2 ) ->
                            isEqual se1 se2


{-| Determine the previous `content` value for this event. This field is only a
`Just value` if the event is a state event, and the Matrix Vault has permission
to see the previous content.
-}
prevContent : Event -> Maybe E.Value
prevContent event =
    Maybe.andThen (\(UnsignedData data) -> data.prevContent) event.unsigned


{-| If the event has been redacted, the homeserver can display the event that
redacted it here.
-}
redactedBecause : Event -> Maybe Event
redactedBecause event =
    Maybe.andThen (\(UnsignedData data) -> data.redactedBecause) event.unsigned


{-| If the user has sent this event to the homeserver, then the homeserver might
display the original transaction id used for the event.
-}
transactionId : Event -> Maybe String
transactionId event =
    Maybe.andThen (\(UnsignedData data) -> data.transactionId) event.unsigned
