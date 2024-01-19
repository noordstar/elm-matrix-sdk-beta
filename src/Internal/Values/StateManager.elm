module Internal.Values.StateManager exposing
    ( StateManager
    , empty, singleton, insert, remove, append
    , isEmpty, member, memberKey, get, size, isEqual
    , keys, values, fromList, toList
    , coder, encode, decoder
    )

{-| The StateManager tracks the room state based on events, their event types
and the optional state keys they provide. Instead of making the user loop
through the room's timeline of events, the StateManager offers the user a
dictionary-like experience to navigate through the Matrix room state.


## Dictionaries

@docs StateManager


## Build

@docs empty, singleton, insert, remove, append


## Query

@docs isEmpty, member, memberKey, get, size, isEqual


## Lists

@docs keys, values, fromList, toList


## JSON coders

@docs coder, encode, decoder

-}

import FastDict as Dict exposing (Dict)
import Internal.Config.Text as Text
import Internal.Tools.Json as Json
import Internal.Tools.Mashdict as Mashdict exposing (Mashdict)
import Internal.Values.Event as Event exposing (Event)


{-| The StateManager manages the room state by gathering events and looking at
their details.
-}
type StateManager
    = StateManager (Dict String (Mashdict Event))


{-| Add a new statemanager on top of an existing StateManager. This can be
useful when trying to calculate a room state based on two already existing
types.
-}
append : StateManager -> StateManager -> StateManager
append sm2 sm1 =
    List.foldl insert sm1 (values sm2)



-- {-| Remove any floating empty Mashdicts from ALL keys in the dictionary.
-- -}
-- cleanAll : StateManager -> StateManager
-- cleanAll ((StateManager manager) as sm) =
--     List.foldl cleanKey sm (Dict.keys manager)


{-| To keep the StateManager as simple as possible, you can keep the dictionary
clean by removing any floating empty Mashdicts in the dictionary.

To save time, this function exclusively removes an empty Mashdict at a given
key. This way, you don't need to run a complete clean of a large dictionary
every time you just edit a single key in the dictionary.

-}
cleanKey : String -> StateManager -> StateManager
cleanKey key (StateManager manager) =
    manager
        |> Dict.update key
            (Maybe.andThen
                (\dict ->
                    if Mashdict.isEmpty dict then
                        Nothing

                    else
                        Just dict
                )
            )
        |> StateManager


coder : Json.Coder StateManager
coder =
    Event.coder
        |> Mashdict.coder .stateKey
        |> Json.fastDict
        |> Json.map
            { name = Text.docs.stateManager.name
            , description = Text.docs.stateManager.description
            , forth = StateManager
            , back = \(StateManager manager) -> manager
            }


{-| Decode a StateManager from a JSON value.
-}
decoder : Json.Decoder StateManager
decoder =
    Json.decode coder


{-| Create an empty StateManager.
-}
empty : StateManager
empty =
    StateManager Dict.empty


{-| Encode a StateManager into a JSON value.
-}
encode : Json.Encoder StateManager
encode =
    Json.encode coder


{-| Build a StateManager using a list of events.
-}
fromList : List Event -> StateManager
fromList events =
    List.foldl insert empty events


{-| Get an event based on its event type and state key. If there is no such
event sent in the room, the function returns `Nothing`.
-}
get : { eventType : String, stateKey : String } -> StateManager -> Maybe Event
get { eventType, stateKey } (StateManager manager) =
    manager
        |> Dict.get eventType
        |> Maybe.andThen (Mashdict.get stateKey)


{-| Insert a new event into the state manager. If the event does not have a
state key, it is overlooked.
-}
insert : Event -> StateManager -> StateManager
insert event (StateManager manager) =
    manager
        |> Dict.update
            event.eventType
            (\typeDict ->
                case typeDict of
                    Nothing ->
                        Just <| Mashdict.singleton .stateKey event

                    Just md ->
                        Just <| Mashdict.insert event md
            )
        |> StateManager
        |> cleanKey event.eventType


{-| Determine whether the StateManager contains any events.
-}
isEmpty : StateManager -> Bool
isEmpty (StateManager manager) =
    Dict.isEmpty manager


{-| Since the StateManager's internal structure prevents Elm from making (==)
comparisons, the `isEqual` function allows you to make comparisons that ignore
the incomparable function.
-}
isEqual : StateManager -> StateManager -> Bool
isEqual (StateManager sm1) (StateManager sm2) =
    if Dict.size sm1 /= Dict.size sm2 then
        False

    else if Dict.keys sm1 /= Dict.keys sm2 then
        False

    else
        List.all
            (\key ->
                case ( Dict.get key sm1, Dict.get key sm2 ) of
                    ( Just s1, Just s2 ) ->
                        Mashdict.isEqual s1 s2

                    ( _, _ ) ->
                        False
            )
            (Dict.keys sm1)


{-| Retrieve all keys from a StateManager.
-}
keys : StateManager -> List { eventType : String, stateKey : String }
keys (StateManager manager) =
    manager
        |> Dict.toList
        |> List.map
            (\( eventType, dict ) ->
                dict
                    |> Mashdict.keys
                    |> List.map
                        (\stateKey ->
                            { eventType = eventType, stateKey = stateKey }
                        )
            )
        |> List.concat


{-| Determine whether an event is part of the StateManager.
-}
member : Event -> StateManager -> Bool
member event (StateManager manager) =
    case Dict.get event.eventType manager of
        Just dict ->
            Mashdict.member event dict

        Nothing ->
            False


{-| Determine whether a given key is part of the StateManager.
-}
memberKey : { eventType : String, stateKey : String } -> StateManager -> Bool
memberKey { eventType, stateKey } (StateManager manager) =
    case Dict.get eventType manager of
        Just dict ->
            Mashdict.memberKey stateKey dict

        Nothing ->
            False


{-| Get a StateManager without a given event in it. If the StateManager already
doesn't have the event, nothing changes.
-}
remove : Event -> StateManager -> StateManager
remove event (StateManager manager) =
    manager
        |> Dict.update event.eventType (Maybe.map (Mashdict.remove event))
        |> StateManager
        |> cleanKey event.eventType


{-| Create a StateManager that contains a single event.
-}
singleton : Event -> StateManager
singleton event =
    insert event empty


{-| Determine the StateManager's size by the amount of events.
-}
size : StateManager -> Int
size (StateManager manager) =
    manager
        |> Dict.values
        |> List.map Mashdict.size
        |> List.sum


{-| Transform the StateManager to a list of events.
-}
toList : StateManager -> List Event
toList =
    values


{-| Get the values from the StateManager, ordered by their event type (and by
their state key, if multiple events are of the same event type).
-}
values : StateManager -> List Event
values (StateManager manager) =
    manager
        |> Dict.values
        |> List.map Mashdict.values
        |> List.concat
