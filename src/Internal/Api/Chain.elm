module Internal.Api.Chain exposing
    ( TaskChain, CompleteChain
    , IdemChain, toTask
    , fail, succeed
    )

{-|


# Task chains

Elm uses a `Task` type to avoid issues that JavaScript deals with, yet the same
**callback hell** issue might appear that JavaScript developers often deal with.
For this reason, this module helps chain different `Task` types together such
that all information is stored and values are dealt with appropriately.

Elm's type checking system helps making this system sufficiently rigorous to
avoid leaking values passing through the API in unexpected ways.

@docs TaskChain, CompleteChain


## Finished chain

@docs IdemChain, toTask


## Operations

@docs fail, succeed

-}

import Internal.Config.Log exposing (Log)
import Internal.Values.Context as Context exposing (APIContext)
import Task


type alias Backpacked u a =
    { a | messages : List u, logs : List Log }


{-| The TaskChain is a piece in the long chain of tasks that need to be completed.
The type defines four variables:

  - `err` value that may arise on an error
  - `u` the update msg that should be returned
  - `a` phantom type before executing the chain's context
  - `b` phantom type after executing the chain's context

-}
type alias TaskChain err u a b =
    APIContext a -> Task.Task (FailedChainPiece err u) (TaskChainPiece u a b)


{-| An IdemChain is a TaskChain that does not influence the chain's context

  - `err` value that may arise on an error
  - `u` the update msg that should be executed
  - `a` phantom type before, during and after the chain's context

-}
type alias IdemChain err u a =
    TaskChain err u a a


{-| A CompleteChain is a complete task chain where all necessary information
has been defined. In simple terms, whenever a Matrix API call is made, all
necessary information for that endpoint:

1.  Was previously known and has been inserted, or
2.  Was acquired before actually making the API call.

-}
type alias CompleteChain u =
    TaskChain Never u {} {}


{-| A TaskChainPiece is a piece that updates the chain's context.

Once a chain is executed, the process will add the `messages` value to its list
of updates, and it will update its context according to the `contextChange`
function.

-}
type alias TaskChainPiece u a b =
    Backpacked u { contextChange : APIContext a -> APIContext b }


{-| A FailedChainPiece initiates an early breakdown of a chain. Unless caught,
this halts execution of the chain. The process will add the `messages` value to
its list of updates, and it will return the given `err` value for a direct
explanation of what went wrong.
-}
type alias FailedChainPiece err u =
    Backpacked u { error : err }


{-| Chain two tasks together. The second task will only run if the first one
succeeds.
-}
andThen : TaskChain err u b c -> TaskChain err u a b -> TaskChain err u a c
andThen f2 f1 =
    \context ->
        f1 context
            |> Task.andThen
                (\old ->
                    context
                        |> old.contextChange
                        |> f2
                        |> Task.map
                            (\new ->
                                { contextChange = old.contextChange >> new.contextChange
                                , logs = List.append old.logs new.logs
                                , messages = List.append old.messages new.messages
                                }
                            )
                        |> Task.mapError
                            (\new ->
                                { error = new.error
                                , logs = List.append old.logs new.logs
                                , messages = List.append old.messages new.messages
                                }
                            )
                )


{-| When an error has occurred, "fix" it with an artificial task chain result.
-}
catchWith : (err -> TaskChainPiece u a b) -> TaskChain err u a b -> TaskChain err u a b
catchWith onErr f =
    onError (\e -> succeed <| onErr e) f


{-| Creates a task that always fails.
-}
fail : err -> TaskChain err u a b
fail e _ =
    Task.fail { error = e, logs = [], messages = [] }


{-| Optionally run a task that doesn't need to succeed.

If the provided chain fails, it will be ignored. This way, the chain can be
executed without breaking the whole chain if it fails. This can be useful for:

1.  Sending information to the Matrix API and not caring if it actually arrives
2.  Gaining optional information that might be nice to know, but not necessary

Consequently, the optional chain cannot add any information that the rest of
the chain relies on.

-}
maybe : IdemChain err u a -> IdemChain err2 u a
maybe f =
    { contextChange = identity
    , logs = []
    , messages = []
    }
        |> succeed
        |> always
        |> onError
        |> (|>) f


{-| When an error occurs, this function allows the task chain to go down a
similar but different route.
-}
onError : (err -> TaskChain err2 u a b) -> TaskChain err u a b -> TaskChain err2 u a b
onError onErr f =
    \context ->
        f context
            |> Task.onError
                (\old ->
                    { contextChange = identity
                    , logs = old.logs
                    , messages = old.messages
                    }
                        |> succeed
                        |> andThen (onErr old.error)
                        |> (|>) context
                )


{-| Creates a task that always succeeds.
-}
succeed : TaskChainPiece u a b -> TaskChain err u a b
succeed piece _ =
    Task.succeed piece


{-| Once the chain is complete, turn it into a valid task.
-}
toTask : IdemChain Never u a -> APIContext a -> Task.Task Never (Backpacked u {})
toTask chain context =
    chain context
        |> Task.onError (\e -> Task.succeed <| never e.error)
        |> Task.map
            (\backpack ->
                { messages = backpack.messages
                , logs = backpack.logs
                }
            )
