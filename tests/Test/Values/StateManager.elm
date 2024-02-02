module Test.Values.StateManager exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Internal.Values.StateManager as StateManager exposing (StateManager)
import Json.Decode as D
import Json.Encode as E
import Test exposing (..)
import Test.Values.Event as TestEvent


fuzzer : Fuzzer StateManager
fuzzer =
    TestEvent.fuzzer
        |> Fuzz.list
        |> Fuzz.map StateManager.fromList


fuzzerKey : Fuzzer { eventType : String, stateKey : String }
fuzzerKey =
    Fuzz.map2
        (\a b -> { eventType = a, stateKey = b })
        Fuzz.string
        Fuzz.string


suite : Test
suite =
    describe "StateManager"
        [ describe "empty"
            [ test "empty isEmpty"
                (StateManager.empty
                    |> StateManager.isEmpty
                    |> Expect.equal True
                    |> always
                )
            , fuzz TestEvent.fuzzer
                "empty has no member"
                (\event ->
                    StateManager.empty
                        |> StateManager.member event
                        |> Expect.equal False
                )
            , fuzz fuzzerKey
                "empty has no memberKey"
                (\key ->
                    StateManager.empty
                        |> StateManager.memberKey key
                        |> Expect.equal False
                )
            , fuzz fuzzerKey
                "Empty gets Nothing"
                (\key ->
                    StateManager.empty
                        |> StateManager.get key
                        |> Expect.equal Nothing
                )
            , test "Empty has no keys"
                (StateManager.empty
                    |> StateManager.keys
                    |> Expect.equal []
                    |> always
                )
            , test "Empty has no values"
                (StateManager.empty
                    |> StateManager.values
                    |> Expect.equal []
                    |> always
                )
            , test "toList empty equals []"
                (StateManager.empty
                    |> StateManager.toList
                    |> Expect.equal []
                    |> always
                )
            , test "fromList [] equals empty"
                ([]
                    |> StateManager.fromList
                    |> Expect.equal StateManager.empty
                    |> always
                )
            , test "JSON encode -> JSON decode remains empty"
                (StateManager.empty
                    |> StateManager.encode
                    |> E.encode 0
                    |> D.decodeString StateManager.decoder
                    |> Expect.equal (Ok ( StateManager.empty, [] ))
                    |> always
                )
            ]
        , describe "singleton"
            [ fuzz TestEvent.fuzzerState
                "singleton = empty + event"
                (\event ->
                    StateManager.empty
                        |> StateManager.insert event
                        |> StateManager.isEqual (StateManager.singleton event)
                        |> Expect.equal True
                )
            , fuzz TestEvent.fuzzerState
                "singleton - event = empty"
                (\event ->
                    StateManager.singleton event
                        |> StateManager.remove event
                        |> StateManager.isEqual StateManager.empty
                        |> Expect.equal True
                )
            , fuzz TestEvent.fuzzerState
                "singleton has one member"
                (\event ->
                    StateManager.singleton event
                        |> StateManager.member event
                        |> Expect.equal True
                )

            -- , fuzz2 TestEvent.fuzzerState TestEvent.fuzzerState
            --     "singleton has no other members"
            --     (\e1 e2 ->
            --         if (Debug.log "To compare" e1) == e2 then
            --             Expect.pass
            --         else
            --             ()
            --                 |> Debug.log "Not equal"
            --                 |> always (StateManager.singleton e1)
            --                 |> StateManager.member e2
            --                 |> Expect.equal False
            --     )
            , fuzz TestEvent.fuzzerState
                "singleton has one value"
                (\event ->
                    StateManager.singleton event
                        |> StateManager.values
                        |> Expect.equal [ event ]
                )
            ]

        -- Write other tests here
        ]
