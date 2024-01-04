module Test.Filter.Timeline exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Internal.Filter.Timeline as Filter exposing (Filter)
import Internal.Values.Event as Event
import Test exposing (..)
import Test.Values.Event as TestEvent


fuzzer : Fuzzer Filter
fuzzer =
    Fuzz.map2 Filter.and
        (Fuzz.oneOf
            [ Fuzz.map Filter.allSendersExcept (Fuzz.list Fuzz.string)
            , Fuzz.map Filter.onlySenders (Fuzz.list Fuzz.string)
            , Fuzz.constant Filter.pass
            ]
        )
        (Fuzz.oneOf
            [ Fuzz.map Filter.allTypesExcept (Fuzz.list Fuzz.string)
            , Fuzz.map Filter.onlyTypes (Fuzz.list Fuzz.string)
            , Fuzz.constant Filter.pass
            ]
        )


suite : Test
suite =
    describe "Timeline filter"
        [ describe "Tautological equivalences"
            [ test "Pass /= fail"
                (Filter.pass
                    |> Expect.notEqual Filter.fail
                    |> always
                )
            , test "All senders == pass"
                (Filter.allSendersExcept []
                    |> Expect.equal Filter.pass
                    |> always
                )
            , test "All types == pass"
                (Filter.allTypesExcept []
                    |> Expect.equal Filter.pass
                    |> always
                )
            , test "No senders == fail"
                (Filter.onlySenders []
                    |> Expect.equal Filter.fail
                    |> always
                )
            , test "No types == fail"
                (Filter.onlyTypes []
                    |> Expect.equal Filter.fail
                    |> always
                )
            , fuzz2 Fuzz.string
                (Fuzz.list Fuzz.string)
                "Some types /= some senders"
                (\head tail ->
                    Expect.notEqual
                        (Filter.onlyTypes (head :: tail))
                        (Filter.onlySenders (head :: tail))
                )
            ]
        , describe "Event filters"
            [ fuzz TestEvent.fuzzer
                "Only event type filter matches"
                (\event ->
                    event
                        |> Filter.match (Filter.onlyTypes [ event.eventType ])
                        |> Expect.equal True
                )
            , fuzz TestEvent.fuzzer
                "Only event sender filter matches"
                (\event ->
                    event
                        |> Filter.match (Filter.onlySenders [ event.sender ])
                        |> Expect.equal True
                )
            , fuzz TestEvent.fuzzer
                "Not event type filter doesn't match"
                (\event ->
                    event
                        |> Filter.match (Filter.allTypesExcept [ event.eventType ])
                        |> Expect.equal False
                )
            , fuzz TestEvent.fuzzer
                "Not event sender filter doesn't match"
                (\event ->
                    event
                        |> Filter.match (Filter.allSendersExcept [ event.sender ])
                        |> Expect.equal False
                )
            , fuzz2 TestEvent.fuzzer
                (Fuzz.list Fuzz.string)
                "Only matches when in sender list"
                (\event senders ->
                    event
                        |> Filter.match (Filter.onlySenders senders)
                        |> Expect.equal (List.member event.sender senders)
                )
            , fuzz2 TestEvent.fuzzer
                (Fuzz.list Fuzz.string)
                "Only matches when in type list"
                (\event types ->
                    event
                        |> Filter.match (Filter.onlyTypes types)
                        |> Expect.equal (List.member event.eventType types)
                )
            , fuzz2 TestEvent.fuzzer
                (Fuzz.list Fuzz.string)
                "All except doesn't match when in sender list"
                (\event senders ->
                    event
                        |> Filter.match (Filter.allSendersExcept senders)
                        |> Expect.notEqual (List.member event.sender senders)
                )
            , fuzz2 TestEvent.fuzzer
                (Fuzz.list Fuzz.string)
                "All except doesn't match when in type list"
                (\event types ->
                    event
                        |> Filter.match (Filter.allTypesExcept types)
                        |> Expect.notEqual (List.member event.eventType types)
                )
            , fuzz (Fuzz.list Fuzz.string)
                "Only list AND all except list = fail senders"
                (\senders ->
                    Filter.onlySenders senders
                        |> Filter.and (Filter.allSendersExcept senders)
                        |> Expect.equal Filter.fail
                )
            , fuzz (Fuzz.list Fuzz.string)
                "Only list AND all except list = fail types"
                (\types ->
                    Filter.onlyTypes types
                        |> Filter.and (Filter.allTypesExcept types)
                        |> Expect.equal Filter.fail
                )
            ]
        , describe "Use case testing"
            [ fuzz3 (Fuzz.list TestEvent.fuzzer)
                (Fuzz.list Fuzz.string)
                (Fuzz.list Fuzz.string)
                "Only senders + only type"
                (\events senders types ->
                    let
                        l1 : List Event.Event
                        l1 =
                            events
                                |> Filter.run
                                    ( Filter.and
                                        ( Filter.onlySenders senders )
                                        ( Filter.onlyTypes types )
                                    )
                        
                        l2 : List Event.Event
                        l2 =
                            ( List.filter
                                (\e ->
                                    (List.member e.sender senders) &&
                                    (List.member e.eventType types)
                                )
                                events
                            )
                    in
                        Expect.all
                            [ Expect.equal (List.length l1) (List.length l2)
                                |> always
                            , List.map2 Event.isEqual l1 l2
                                |> List.all identity
                                |> Expect.equal True
                                |> always
                            ]
                            ()
                )
            , fuzz3 (Fuzz.list TestEvent.fuzzer)
                (Fuzz.list Fuzz.string)
                (Fuzz.list Fuzz.string)
                "Only senders + all except type"
                (\events senders types ->
                    let
                        l1 : List Event.Event
                        l1 =
                            events
                                |> Filter.run
                                    ( Filter.and
                                        ( Filter.onlySenders senders )
                                        ( Filter.allTypesExcept types )
                                    )
                        
                        l2 : List Event.Event
                        l2 =
                            ( List.filter
                                (\e ->
                                    (List.member e.sender senders) &&
                                    (not <| List.member e.eventType types)
                                )
                                events
                            )
                    in
                        Expect.all
                            [ Expect.equal (List.length l1) (List.length l2)
                                |> always
                            , List.map2 Event.isEqual l1 l2
                                |> List.all identity
                                |> Expect.equal True
                                |> always
                            ]
                            ()
                )
            , fuzz3 (Fuzz.list TestEvent.fuzzer)
                (Fuzz.list Fuzz.string)
                (Fuzz.list Fuzz.string)
                "All except senders + only type"
                (\events senders types ->
                    let
                        l1 : List Event.Event
                        l1 =
                            events
                                |> Filter.run
                                    ( Filter.and
                                        ( Filter.allSendersExcept senders )
                                        ( Filter.onlyTypes types )
                                    )
                        
                        l2 : List Event.Event
                        l2 =
                            ( List.filter
                                (\e ->
                                    (not <| List.member e.sender senders) &&
                                    (List.member e.eventType types)
                                )
                                events
                            )
                    in
                        Expect.all
                            [ Expect.equal (List.length l1) (List.length l2)
                                |> always
                            , List.map2 Event.isEqual l1 l2
                                |> List.all identity
                                |> Expect.equal True
                                |> always
                            ]
                            ()
                )
            , fuzz3 (Fuzz.list TestEvent.fuzzer)
                (Fuzz.list Fuzz.string)
                (Fuzz.list Fuzz.string)
                "All except senders + all except type"
                (\events senders types ->
                    let
                        l1 : List Event.Event
                        l1 =
                            events
                                |> Filter.run
                                    ( Filter.and
                                        ( Filter.allSendersExcept senders )
                                        ( Filter.allTypesExcept types )
                                    )
                        
                        l2 : List Event.Event
                        l2 =
                            ( List.filter
                                (\e ->
                                    (not <| List.member e.sender senders) &&
                                    (not <| List.member e.eventType types)
                                )
                                events
                            )
                    in
                        Expect.all
                            [ Expect.equal (List.length l1) (List.length l2)
                                |> always
                            , List.map2 Event.isEqual l1 l2
                                |> List.all identity
                                |> Expect.equal True
                                |> always
                            ]
                            ()
                )
            ]
        ]
