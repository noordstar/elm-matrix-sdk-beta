module Internal.Filter.Timeline exposing
    ( Filter
    , pass, onlySenders, allSendersExcept, onlyTypes, allTypesExcept, fail
    , match, run
    , and
    , subsetOf
    )

{-|


# Timeline filter

The timeline filter creates filters for looking through a timeline, as well as
for interacting with the Matrix API.


## Timeline

@docs Filter


## Create

@docs pass, onlySenders, allSendersExcept, onlyTypes, allTypesExcept, fail


## Filter

@docs match, run


## Combine

@docs and


## Compare

@docs subsetOf

-}

import Set exposing (Set)
import Task exposing (fail)


{-| Placeholder Event type so the real Event doesn't need to be imported.
-}
type alias Event a =
    { a | eventType : String, sender : String }


{-| The Timeline Filter filters events out of a timeline, guaranteeing that only
the events that meet the given criteria, meet the requirements.
-}
type Filter
    = Filter
        { senders : Set String
        , sendersAllowOthers : Bool
        , types : Set String
        , typesAllowOthers : Bool
        }


{-| Allow events from all senders, except if they are on the provided list.

If the list is empty, all events are allowed.

-}
allSendersExcept : List String -> Filter
allSendersExcept senders =
    case senders of
        [] ->
            pass

        _ :: _ ->
            Filter
                { senders = Set.fromList senders
                , sendersAllowOthers = True
                , types = Set.empty
                , typesAllowOthers = True
                }


{-| Allow events of every event type, except if they are on the provided list.

If the list is empty, all events are allowed.

-}
allTypesExcept : List String -> Filter
allTypesExcept types =
    case types of
        [] ->
            pass

        _ :: _ ->
            Filter
                { senders = Set.empty
                , sendersAllowOthers = True
                , types = Set.fromList types
                , typesAllowOthers = True
                }


{-| Only allow an event if it meets the criteria of two Filters.
-}
and : Filter -> Filter -> Filter
and (Filter f1) (Filter f2) =
    let
        stdAnd : Filter
        stdAnd =
            Filter
                { senders =
                    case ( f1.sendersAllowOthers, f2.sendersAllowOthers ) of
                        ( True, True ) ->
                            Set.union f1.senders f2.senders

                        ( True, False ) ->
                            Set.diff f2.senders f1.senders

                        ( False, True ) ->
                            Set.diff f1.senders f2.senders

                        ( False, False ) ->
                            Set.intersect f1.senders f2.senders
                , sendersAllowOthers = f1.sendersAllowOthers && f2.sendersAllowOthers
                , types =
                    case ( f1.typesAllowOthers, f2.typesAllowOthers ) of
                        ( True, True ) ->
                            Set.union f1.types f2.types

                        ( True, False ) ->
                            Set.diff f2.types f1.types

                        ( False, True ) ->
                            Set.diff f1.types f2.types

                        ( False, False ) ->
                            Set.intersect f1.types f2.types
                , typesAllowOthers = f1.typesAllowOthers && f2.typesAllowOthers
                }
    in
    case stdAnd of
        Filter f ->
            if Set.isEmpty f.senders && not f.sendersAllowOthers then
                fail

            else if Set.isEmpty f.types && not f.typesAllowOthers then
                fail

            else
                stdAnd


{-| Allow no events. This filter is likely quite useless in practice, but it is
used in the test environment for sanity checks and comparisons.
-}
fail : Filter
fail =
    Filter
        { senders = Set.empty
        , sendersAllowOthers = False
        , types = Set.empty
        , typesAllowOthers = False
        }


{-| Determine whether an event passes a filter.
-}
match : Filter -> Event a -> Bool
match (Filter f) { eventType, sender } =
    let
        mentionedSender : Bool
        mentionedSender =
            Set.member sender f.senders

        mentionedType : Bool
        mentionedType =
            Set.member eventType f.types
    in
    xor mentionedSender f.sendersAllowOthers
        && xor mentionedType f.typesAllowOthers


{-| Only allow event sent by given senders.

If an empty list is given, no events are allowed.

-}
onlySenders : List String -> Filter
onlySenders senders =
    case senders of
        [] ->
            fail

        _ :: _ ->
            Filter
                { senders = Set.fromList senders
                , sendersAllowOthers = False
                , types = Set.empty
                , typesAllowOthers = True
                }


{-| Only allow events of a given event type.

If an empty list is given, no events are allowed.

-}
onlyTypes : List String -> Filter
onlyTypes types =
    case types of
        [] ->
            fail

        _ :: _ ->
            Filter
                { senders = Set.empty
                , sendersAllowOthers = True
                , types = Set.fromList types
                , typesAllowOthers = False
                }


{-| Create a filter that allows all events. This can be useful when trying to
combine multiple filters, or when simply all events are allowed.
-}
pass : Filter
pass =
    Filter
        { senders = Set.empty
        , sendersAllowOthers = True
        , types = Set.empty
        , typesAllowOthers = True
        }


{-| Use a filter on a list of events.
-}
run : Filter -> List (Event a) -> List (Event a)
run f events =
    List.filter (match f) events


{-| Determine whether the second argument is a subset filter of the first
argument.
-}
subsetOf : Filter -> Filter -> Bool
subsetOf (Filter big) (Filter small) =
    let
        isSSof : Set String -> Set String -> Bool
        isSSof b s =
            Set.intersect b s == s

        isSubsetFor : ( Bool, Set String ) -> ( Bool, Set String ) -> Bool
        isSubsetFor ( bb, sb ) ( bs, ss ) =
            case ( bb, bs ) of
                ( True, True ) ->
                    isSSof ss sb

                ( True, False ) ->
                    Set.isEmpty (Set.intersect sb ss)

                ( False, True ) ->
                    False

                ( False, False ) ->
                    isSSof sb ss
    in
    isSubsetFor
        ( big.sendersAllowOthers, big.senders )
        ( small.sendersAllowOthers, small.senders )
        && isSubsetFor
            ( big.typesAllowOthers, big.types )
            ( small.typesAllowOthers, small.types )
