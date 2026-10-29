# Code and documentation style

**Status:** adopted (2026-10-29). Bram owns this file; Claude follows it for
every module, function and type it writes (architecture decision D18). If
something here doesn't sound right, change the file. Don't work around it.

The short version:

1. Every module starts with a module doc that has `@docs`, as if it were about
   to be published.
2. Every function and type has a `{-| -}` doc comment, exposed or not.
3. Definitions are sorted alphabetically, unless their order tells a story
   (model → update → view).
4. Docs hold the reader's hand, the way `elm/core` and `elm/browser` do:
   friendly, with examples, pointing to what to read next.

---

## 1. Code organisation

These rules apply to **every** module, including `Internal.*` and tests.

- **Module documentation.** Every module begins with a `{-| -}` module doc that
  explains what the module is for and groups its exposed values with `@docs`.
  It should be good enough to publish to the Elm registry and render with the
  Elm doc generator as-is, even for internal modules that never will be.
- **Doc comments on all functions and types.** That includes unexposed helpers.
  A helper's doc can be one or two sentences, but it's never missing.
- **Order inside a file:** imports, then type definitions sorted alphabetically,
  then functions sorted alphabetically.
- **The one exception** is when the order *is* the explanation: a model →
  update → view flow, or a sequence of steps that only makes sense top to
  bottom. Say so in a comment above the first definition:
  `-- Ordered by flow: model, update, view.`
- **Exposing lists** follow the `@docs` groups, not the alphabet, because that's
  the order the reader sees.
- **Formatting:** `elm-format`, always. CI checks it.

---

## 2. Voice: learn from `elm/core` and `elm/browser`

The most-read Elm packages are friendly and patient. They're not formal or
definitive. These are the habits worth copying, with examples from their source:

| Habit | Example |
|---|---|
| **Say what it's for, in human terms, before the details.** | `Maybe`: "A `Maybe` can help you with optional arguments, error handling, and records with optional fields." |
| **Show an example right away**, often several inputs side by side with their results. | `withDefault 100 (Just 42) -- 42` and `withDefault 100 Nothing -- 100` |
| **Explain with a tiny, realistic story.** | `Maybe.andThen` walks through parsing a month from user input. |
| **Reassure and encourage.** | `Browser.element`: "Try it out on something small. If it works, great, do more! If not, revert, no big deal." |
| **Point to what to read next.** | `Json.Decode.field`: "Check out `map2` to see how to decode multiple fields!" |
| **Warn about overuse, kindly**, in a `**Note:**`. | `withDefault`: "This can be overused! Many cases are better handled by a `case` expression." |
| **Link to the guide** for concepts too big for a doc comment. | `Json.Decode`: "There is a whole section in `guide.elm-lang.org` about decoders, so check it out…" |
| **Admit trade-offs honestly.** | `Json.Decode` on decode-pipeline: "…easier to handle large objects, but produces lower quality type errors." |

This SDK's own 3.x docs already do some of this: `Matrix.Room` explains what a
"room" is before anything else, and `Matrix.Settings` weighs a long sync time
against a short one. The v4 type modules (`Matrix.Types.UserId`) add a tight
structure per function. The templates in §5 combine all three.

**The reader** is an Elm developer who wants to build something, a game for
instance, and has never read the Matrix spec. They should never need it to use
this package; the spec link is for the curious.

---

## 3. Writing rules

1. **Explain a Matrix term the first time a module uses it.** "Every user in a
   room has a _membership_: …"
2. **Use "you", plain words and short sentences.** Exclamation marks are welcome
   where `elm/core` would use one.
3. **Use British spelling** ("behaviour", "favour"), as most of Bram's text does.
4. **The first sentence stands on its own,** because Elm shows it in the module
   overview.
   - Functions start with a verb: "Get…", "Check whether…", "Create…",
     "Encode…", "Decode…". Endpoint functions name their action: "Kick a user
     out of a room."
   - Types start with a noun phrase: "The ID of a Matrix room…".
5. **Don't restate the type signature.** Say what the value *means* and when
   you'd want it.
6. **Every `Maybe` says when it's `Nothing`,** and every defaulted value says
   what a missing field becomes.
7. **Say what, not how.** No decoders, internal records or helper names in
   exposed docs. The exception is the spec field name (rule 8).
8. **Accessors name their spec field:** "In the spec, this is the
   `unsigned.prev_content` field." It helps readers who know Matrix, and tests
   check it against the ledger.
9. **Warn about untrusted data where it's exposed:** event content, strings
   chosen by other users (display names, room names, reasons) and
   `origin_server_ts`. One or two sentences, never a lecture.
10. **Version notes use fixed wording,** generated from the ledger:
    - "Available since Matrix v1.3. Older servers answer with
      [`Unsupported`](Matrix-Http#Error)."
    - "Only servers on Matrix v1.11 or newer send this."
11. **Paraphrase the spec, never copy it.** Link to it instead.
12. **No TODOs in doc comments.** Use a normal `--` comment.
13. **Fix typos when porting a file.**

---

## 4. Formatting and links

- **Examples** are Elm code indented four spaces. Results go in a comment, and
  opaque values are written as `<typeName>`:

        UserId.fromString "@alice:example.org"
        -- Just <userId>

        UserId.fromString "alice"
        -- Nothing

- **Lists** use the `  - ` form (two spaces, a dash), each item starting with
  what it describes: ``  - `reason`: …``.
- **Callouts** are `**Note:**`, as in `elm/core`.
- **Module doc sections** are `#` or `##` headings, each with a one- or
  two-sentence introduction, then `@docs`.
- **Long URLs** may use reference-style links at the end of the comment
  (`[guide]: https://…`), as `elm/browser` does.

| Linking to | Form | Example |
|---|---|---|
| The same module | `[name](#name)` | `[prevBatch](#prevBatch)` |
| Another module | `[name](Module-Name#name)`, with **dashes** | `[fetchServerVersion](Matrix-Api-Versions#fetchServerVersion)` |
| The spec | the newest spec version, exact anchor | `https://spec.matrix.org/v1.19/client-server-api/#post_matrixclientv3roomsroomidkick` |

The dotted form (`Matrix.Api.Versions#…`), used in a few v4 modules, is broken
on package.elm-lang.org. Spec links are generated from the ledger, which checks
that the anchor exists. That fixes the outdated spec links of issue #41.

---

## 5. Templates and worked examples

### Module documentation

```elm
module Matrix.Api.RoomMembership exposing
    ( joinRoom, joinRoomById, leaveRoom, forgetRoom, getJoinedRooms
    , inviteUser, kick, ban, unban
    )

{-| Everyone in a room has a _membership_: they're invited, they've joined,
they've left, they've knocked on the door, or they've been banned. This module
changes memberships, both your own (joining and leaving) and other people's
(inviting, kicking and banning).

New to rooms? Start with [joinRoom](#joinRoom). It's the first thing most
games and chat clients do after logging in.


# Joining and leaving

@docs joinRoom, joinRoomById, leaveRoom, forgetRoom, getJoinedRooms


# Moderation

Moderating needs enough _power_ in the room. Who has how much power is decided
by the room's `m.room.power_levels` state event, which you can read with
[Matrix.Api.RoomState](Matrix-Api-RoomState).

@docs inviteUser, kick, ban, unban

-}
```

### Opaque type

```elm
{-| The ID of a Matrix room, such as `!abc123:example.org`.

Unlike room names and aliases, room IDs never change, so they make good `Dict`
keys. Two room IDs are equal exactly when they point to the same room, so
`(==)` just works.

-}
type RoomId
```

### Accessors (the most common kind)

Template:

```elm
{-| Get <what, in plain words>.

<What it means and when you'd use it: one or two sentences.>
<When it's `Nothing`, or what a missing value becomes.>

In the spec, this is the `<json.path>` field.

-}
```

Worked examples:

```elm
{-| Get the batch token to use as `since` in your next sync.

Think of it as a bookmark. Pass it to your next [sync](Matrix-Api-Sync#sync),
and the server only tells you what happened after this response.

    Sync.sync creds { since = Just (Sync.nextBatch response), timeout = Nothing, filter = Nothing }

In the spec, this is the `next_batch` field.

-}
nextBatch : SyncResponse -> BatchToken


{-| Check whether the server left out events from this timeline.

When a room was busy while you weren't syncing, the server sends only the most
recent events and sets this to `True`. That's no big deal: you can fetch the
missed events with [prevBatch](#prevBatch), or simply start the room's history
from here.

In the spec, this is the `limited` field. A missing value means `False`.

-}
timelineLimited : Timeline -> Bool


{-| Get the number of users who have joined the room, if the server sent it.

The server only includes this number when it changed since your last sync, so
`Nothing` means "no change", not "zero". Keep the last number you saw!

In the spec, this is the `m.joined_member_count` field of the room summary.

-}
joinedMemberCount : RoomSummary -> Maybe Int


{-| Get the event's content: the JSON that the sender wrote.

This is where your game's moves, messages and other data live. Anyone in the
room can send any JSON they like, though, so decode it with your own decoder
and be ready for it to fail:

    case D.decodeValue moveDecoder (ClientEvent.content event) of
        Ok move ->
            applyMove move game

        Err _ ->
            game -- someone sent something we don't understand; ignore it

Use [eventType](#eventType) to decide which decoder to try.

In the spec, this is the `content` field.

-}
content : ClientEvent -> Json.Decode.Value


{-| Get the time at which the sender's server says it received the event.

It's usually close to when the event was sent, which makes it great for
showing "5 minutes ago". Don't rely on it for anything that matters, though:
network lag can make it old, and a malicious server can set it to any time,
even in the future.

**Note:** In a game, don't use this to decide who moved first. Use the order in
which events appear in the timeline instead.

In the spec, this is the `origin_server_ts` field.

-}
originServerTs : ClientEvent -> Time.Posix
```

### Conversions (`fromString`, `toString`, `encode`, `decoder`)

These follow `Matrix.Types.UserId` as it is. Say whether `fromString` checks
its input. If it does, show both outcomes side by side, and say when you'd need
`toString`.

### Endpoint functions

Template:

```elm
{-| <Action, in plain words.>

<What happens and who notices: one to three sentences. Permissions needed.>

    <A short, realistic example ending in |> Matrix.Http.send>

<The input fields, as a list.>
<Endpoint-specific outcomes, if any.>
<Version note, if any (generated wording).>
<Error codes the spec lists for this endpoint (generated).>

Spec: [`METHOD /path`](<spec link>)

-}
```

Worked example:

```elm
{-| Kick a user out of a room.

The user leaves the room, but they can come back if the room lets them. To keep
them out for good, [ban](#ban) them instead. You need enough power in the room
to kick, and everyone in the room can see the `reason`.

    RoomMembership.kick creds
        { roomId = roomId
        , userId = troll
        , reason = Just "Spamming the chat"
        }
        |> Matrix.Http.send GotKick

  - `roomId`: the room to kick the user from.
  - `userId`: the user to kick.
  - `reason`: optional. Everyone in the room can see it.

On servers older than r0.1.0, the SDK sends a membership state event instead,
which has the same effect.

The server may answer with `M_FORBIDDEN` when you aren't allowed to kick this
user.

Spec: [`POST /rooms/{roomId}/kick`](https://spec.matrix.org/v1.19/client-server-api/#post_matrixclientv3roomsroomidkick)

-}
```

### Custom type constructors

Each constructor gets one list item that says what it means *and* what to do
about it:

```
  - `RateLimited` means you sent too many requests in a short time. No
    worries: wait for `retryAfter` (or a few seconds, if the server didn't
    say) and try again.
```

### Unexposed helpers

Short, but never missing:

```elm
{-| Read a field that newer spec versions moved into `unsigned`, trying the new
location first.
-}
unsignedOrTopLevel : String -> D.Decoder a -> D.Decoder (Maybe a)
```

---

## 6. What gets checked automatically

| Check | Tool | Needs |
|---|---|---|
| Formatting | `elm-format --validate` | elm-format |
| Every exposed value has documentation | `elm make --docs=docs.json` | Elm |
| Links and `@docs` sections are valid; unexposed values are documented too | elm-review with `jfmengels/elm-review-documentation` (`Docs.NoMissing`, `Docs.ReviewLinksAndSections`, `Docs.ReviewAtDocs`) | Node.js |
| Spec links point to the newest version, and their anchors exist | ledger test | Python |
| "In the spec, this is the `x` field" names a real field of that response | ledger test | Python |

What no machine can check is whether it reads like a friendly guide. That's
the part Bram reviews.
