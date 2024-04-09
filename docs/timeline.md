# Timeline

Given the complex nature of the Timeline design, it deserves some explanation of
the design. This document aims to describe how the Elm SDK designs the Timeline,
so that other projects may learn from it.

## API endpoint disambiguations

Generally speaking, there are a few API endpoints with similar design:

- The [`/sync` endpoint](https://spec.matrix.org/v1.9/client-server-api/#get_matrixclientv3sync),
which gets the events that the homeserver received most recently.
- The [`/messages` endpoint](https://spec.matrix.org/v1.9/client-server-api/#get_matrixclientv3roomsroomidmembers),
which gets any events in the topological order.

As noted in the Matrix spec:

> Events are ordered in this API according to the arrival time of the event on
> the homeserver. This can conflict with other APIs which order events based on
> their partial ordering in the event graph. This can result in duplicate events
> being received (once per distinct API called). Clients SHOULD de-duplicate
> events based on the event ID when this happens. 

For this reason, the Elm SDK maintains **two independent timelines** that are tied
together when necessary to form a coherent timeline.

## Elm design

For those unfamiliar, the Elm Architecture breaks into three parts:

- **Model** - the state of the application
- **View** - a way to turn your state into meaningful information
- **Update** - a way to update your state based on the Matrix API

Since these concepts are compartmentalized, it is impossible to make an API call
while executing the **view** function; the Elm SDK must at all times find a way
to represent its state.

## Timeline

Concerning the Matrix timeline, it is meant to create a representation
(**Model**) of the timeline, find a way to represent (**View**) it, and find a
simple way to adjust it with every incoming Matrix API result. (**Update**)

First, we define what a timeline batch is.

### Timeline batch

A timeline batch is something that most Matrix API endpoints return. It is a
little piece of the timeline and contains the following four pieces of
information:

1. A list of events that are part of the timeline.
2. A Filter for which all provided events meet the criteria.
3. An end batch token that functions as an identifier.
4. _(Optional.)_ A start token. If not provided, it indicates the start of the
    timeline.

Here's an example of such a timeline batch:

```
       |-->[■]->[■]->[●]->[■]->[■]->[●]-->|
       |                                  |
       |<---   filter: only ■ and ●   --->|
       |                                  |
     start:                              end:
    <token_1>                          <token_2>
```

When the Matrix API later returns a batch token that starts with `<token_2>`,
we know that we can connect it to the batch above and make a longer list of
events!

At first, this seems quite simple to connect, but there are some difficulties
that come up along the way.

### Challenge 1: different filters, different locations

When two timeline batches have different filters, we do not know their
respective location. For example, the following two timeline batches COULD
overlap, but it is also possible they don't:

```
       |-->[■]->[■]->[●]->[■]->[■]->[●]-->|
       |                                  |
       |<---   filter: only ■ and ●   --->|
       |                                  |
     start:                              end:
    <token_1>                          <token_2>


                |-->[★]->[★]->[★]->[★]-->|
                |                          |
                |<--   filter: only ★   -->|
                |                          |
              start:                      end:
            <token_3>                    <token_4>
```

Realistically, there is currently no way of knowing without making more API
calls. However, just making more API calls isn't a solution in Elm because of
its architecture.

> **SOLUTION:** As described in the **View** function, we may assume that
overlapping timeline batches have overlapping events. If they overlap yet have
no overlapping events, then their filters must be disjoint. If the filters are
disjoint, we do not care whether they're overlapping.

### Challenge 2: same filters, same spot

Suppose there is a known timeline batch, and we're trying to **Update** the
timeline to represent the timeline between `<token_1>` and `<token_2>` for a
different filter:

```
       |-->[■]->[■]->[●]->[■]->[■]->[●]-->|
       |                                  |
       |<---   filter: only ■ and ●   --->|
       |                                  |
     start:                              end:
    <token_1>                          <token_2>
```

If we wish to know what's in there for a different filter `f`, then:

1. If `f` equals the filter from the timeline batch, we can copy the events.
2. If `f` is a subfilter of the batch filter (for example: `only ■`) then we can
    copy the events from the given batch, and then locally filter the events
    that do no match filter `f`.
3. If the batch filter is a subfilter of `f`, then we can use an API call
    between the same batch tokens `<token_1>` and `<token_2>`. In the worst
    case, we receive the exact same list of events. In another scenario, we
    might discover far more events and receive some new batch value `<token_3>`
    in-between `<token_1>` and `<token_2>`.
4. If neither filter is a subfilter of the other and the two are (at least
    partially) disjoint, then they do not need to correlate and any other batch
    values can be chosen.

