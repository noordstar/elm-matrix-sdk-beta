module Internal.Api.Now.Api exposing (getNow)

{-|


# Now

Get the current time.

@docs getNow

-}

import Internal.Api.Api as A
import Internal.Config.Log exposing (log)
import Internal.Values.Context as Context
import Internal.Values.Envelope as E
import Task
import Time


getNow : A.TaskChain a { a | now : () }
getNow =
    \_ ->
        Task.map
            (\now ->
                { messages = [ E.SetNow now ]
                , logs =
                    [ "Identified current time at Unix time "
                    , now |> Time.posixToMillis |> String.fromInt
                    ]
                        |> String.concat
                        |> log.debug
                        |> List.singleton
                , contextChange = Context.setNow now
                }
            )
            Time.now
