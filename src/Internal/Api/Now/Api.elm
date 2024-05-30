module Internal.Api.Now.Api exposing (getNow)

{-|


# Now

Get the current time.

@docs getNow

-}

import Internal.Api.Api as A
import Internal.Config.Log exposing (log)
import Internal.Config.Text as Text
import Internal.Values.Context as Context
import Internal.Values.Envelope as E
import Task
import Time


{-| Get the current time and place it in the context.
-}
getNow : A.TaskChain a { a | now : () }
getNow =
    \_ ->
        Task.map
            (\now ->
                { messages = [ E.SetNow now ]
                , logs =
                    now
                        |> Time.posixToMillis
                        |> Text.logs.getNow
                        |> log.debug
                        |> List.singleton
                , contextChange = Context.setNow now
                }
            )
            Time.now
