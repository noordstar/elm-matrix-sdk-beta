# Elm Matrix SDK 4.0: Architecture

**Status:** accepted, with the open questions answered (2026-10-29). This
replaces the `Vault` design of 3.x. The decision log at the bottom is the place
where future changes of mind get written down *before* they reach the code.

**Related documents:**
- [roadmap.md](roadmap.md): the work list, phase by phase.
- [documentation-style.md](documentation-style.md): code organisation and
  documentation rules.

Two plans are written at the start of their phases: the code design plan
(Phase 1) and the verification plan (Phase 2). This document only fixes the
shape that both have to fit into.

---

## 1. Goals and non-goals

**Goals for 4.0.0**

- Every Matrix endpoint is one function that returns a `Request a`, which turns
  into a `Cmd msg` or a `Task` with one more function call.
- Every stable spec release, from `r0.0.0` up to and including `v1.19`, is
  handled, and the handling is checked against the spec's machine-readable files.
- A game developer can log in, sync and send events without learning how the
  Matrix API works on the inside.
- Changing your mind about a higher layer never forces changes to a lower layer.

**Not in 4.0.0**

| Not doing | Why |
|---|---|
| A `Vault` or any other stateful client | It belongs in a higher layer (§2), later, possibly in a separate package. |
| Sync as a `Sub` | Only `elm/*` and `elm-explorations/*` packages may define the effect managers that custom subscriptions need. |
| Automatic retries, waiting or token refresh inside `send` | Hidden behaviour is what made 3.x's `TaskChain` heavy. Helpers may come later, opt-in only. |
| End-to-end encryption | Needs Olm/Megolm (vodozemac) through ports or WASM. |
| Unstable (MSC) features | They have no machine-readable source. They're opt-in, one at a time, with hand-written tests. |
| Endpoints outside Tier 1 | Tier 2 arrives in 4.x minor releases (see §3). |

---

## 2. Layers

```
  Layer 3  (not in 4.0)   stateful client / Vault-like convenience
     │                     may change its mind as often as it likes
     ▼
  Layer 2  (empty in 4.0) optional helpers, no state: retry policy, sync-loop helpers, ...
     │
     ▼
  Layer 1                 Matrix.Api.*      one module per spec section, one function per endpoint
     │
     ▼
  Layer 0                 Matrix.Http       Request, send, toTask, Error
                          Matrix.Types.*    IDs, events, shared response types
     │
     ▼
  Internal                Internal.Endpoint (variant selection, request building)
                          Internal.Decode   (accept-every-shape decoder helpers)
                          Internal.Grammar.* (ID parsers)
```

**Import rules.** A module may only import from its own layer or the layers below it.

| Module group | May import |
|---|---|
| `Internal.*` | `elm/*`, other `Internal.*` |
| `Matrix.Types.*` | `Internal.*`, other `Matrix.Types.*` |
| `Matrix.Http` | `Internal.*`, `Matrix.Types.*` |
| `Matrix.Api.*` | everything above, but **never another `Matrix.Api.*` module** |

That last rule keeps endpoint modules independent: deleting or rewriting one
can never break another. A type that two endpoint modules need moves down to
`Matrix.Types`.

---

## 3. Module map (Tier 1)

The grouping follows the spec's own sections (decision D13).

| Elm module | Spec operations (`operationId`) | Starts from |
|---|---|---|
| `Matrix.Api.Discovery` | `getWellknown`, `getWellknownSupport` | port `Matrix/Api/Discovery.elm` |
| `Matrix.Api.Versions` | `getVersions` | port `Matrix/Api/Versions.elm` |
| `Matrix.Api.Capabilities` | `getCapabilities` | new |
| `Matrix.Api.Authorization` | `getAuthMetadata` | port `Matrix/Api/Authorization.elm` |
| `Matrix.Api.Login` | `getLoginFlows`, `login` | port `Matrix/Api/Login.elm` |
| `Matrix.Api.Sessions` | `logout`, `logout_all`, `getTokenOwner` (whoami) | port `Matrix/Api/Sessions.elm` + new |
| `Matrix.Api.Tokens` | `refresh` | port `Matrix/Api/Tokens.elm` |
| `Matrix.Api.Registration` | `checkUsernameAvailability`, `registrationTokenValidity` | port `Matrix/Api/Registration.elm` |
| `Matrix.Api.RoomCreation` | `createRoom` | new |
| `Matrix.Api.RoomDirectory` | `getRoomIdByAlias` | new |
| `Matrix.Api.RoomMembership` | `joinRoom`, `joinRoomById`, `getJoinedRooms`, `inviteUser`, `kick`, `ban`, `unban`, `leaveRoom`, `forgetRoom` | port `Matrix/Api/RoomMembership.elm` + new |
| `Matrix.Api.Sync` | `sync` | port `Matrix/Api/Sync.elm` |
| `Matrix.Api.Filtering` | `defineFilter`, `getFilter` | new |
| `Matrix.Api.Events` | `getOneRoomEvent`, `getRoomEvents` (`/messages`), `sendMessage`, `redactEvent` | port `Matrix/Api/Events.elm` + new |
| `Matrix.Api.RoomState` | `getRoomState`, `getRoomStateWithKey`, `setRoomStateWithKey`, `getMembersByRoom` | new |
| `Matrix.Api.AccountData` | `getAccountData`, `setAccountData`, `getAccountDataPerRoom`, `setAccountDataPerRoom` | new |
| `Matrix.Api.Profile` | `getUserProfile`, `getProfileField`, `setProfileField` | new |
| `Matrix.Api.SendToDevice` | `sendToDevice` | new |

That's 43 operations. About 16 of them already exist in some form.

**`Matrix.Types`** keeps the existing modules (`AccessToken`, `RefreshToken`,
`BatchToken`, `DeviceId`, `EventId`, `RoomId`, `UserId`, `HomeserverUrl`,
`ServerVersion`, `DiscoveryInformation`, `ClientEvent`, `SyncEvent`,
`StrippedStateEvent`, `RawEvent`, `DeviceEvent`, `Sync`) and adds the ones Tier 1
needs: `RoomAlias`, `ServerName` (for `via`), `TransactionId`, `MxcUri`
(avatars) and `Filter` (a builder for filter JSON).

**Where response types live.** A type used by exactly one endpoint module lives
in that module (`LoginResponse` stays in `Matrix.Api.Login`). A type used by
two or more lives in `Matrix.Types`.

**`Matrix.Api` (the re-export hub) is deleted** (decision D15). It only existed
to make the import list look less daunting, and it repeats every signature a
second time, so every change had to be made twice. The README and the generated
coverage table take over its "table of contents" role.

### Where every value comes from

A check on the design: every input a later call needs comes either from the
user or from the output of an earlier call.

| Value | Comes from |
|---|---|
| `HomeserverUrl` | user input, or `Discovery` (`.well-known`) |
| `ServerVersion` | `Versions.fetchServerVersion`, or `ServerVersion.naive` |
| `AccessToken`, `UserId`, `DeviceId`, `RefreshToken` | `Login`, `Tokens.refresh` |
| `RoomId` | `RoomCreation`, `RoomMembership.join*`, `RoomDirectory`, sync |
| `EventId` | `Events.sendMessage`, sync, `/messages` |
| `BatchToken` | sync (`next_batch`, `prev_batch`), `/messages` |
| `TransactionId` | the user (a counter is enough; it's scoped to one access token) |

---

## 4. Core contracts

### 4.1 Credentials are plain records

There's no `Session` type. Each endpoint's first argument describes exactly
what that endpoint needs, as an extensible record:

```elm
-- Matrix.Http exposes three aliases, purely so the docs read well:
type alias Anonymous a     = { a | baseUrl : HomeserverUrl }
type alias Public a        = { a | baseUrl : HomeserverUrl, serverVersion : ServerVersion }
type alias Authenticated a = { a | baseUrl : HomeserverUrl, serverVersion : ServerVersion, accessToken : AccessToken }

kick : Authenticated a -> { roomId : RoomId, userId : UserId, reason : Maybe String } -> Request ()
```

- `Anonymous`: you know where the server is, but nothing else yet. Today only
  `fetchServerVersion` uses it, because that's how you find out the
  `serverVersion`. The `.well-known` endpoints take the server's domain instead.
- `Public`: you know where the server is and which spec versions it speaks, but
  you aren't logged in.
- `Authenticated`: you're logged in.

**The rule that keeps this from growing:** the first argument only ever holds
what's needed to *address and authorize* an HTTP request. That's `baseUrl`,
`serverVersion` and `accessToken`, a set fixed by HTTP rather than by
convenience. Everything else (room IDs, `since` tokens, transaction IDs) goes in
the second argument. The aliases have no constructor and no functions. Users
pass their model, their own record, or an inline record literal.

- A forgotten token is a compile error at the call site. There are no phantom
  types and no client-side `MissingAccessToken` error.
- A server that *asks* for a token is a runtime answer (`NotLoggedIn`, §4.3).
  That's always possible, even on endpoints the spec calls public. Synapse, for
  example, requires auth for `/publicRooms` unless
  `allow_public_rooms_without_auth` is on.
- For endpoints where the token is optional, `Matrix.Http.withAccessToken` adds one.

### 4.2 `Request a`: one HTTP round trip, nothing more

```elm
-- Internal representation (not exposed)
type Request a
    = Request
        { method : String
        , url : String -- base URL + path chosen for this server version + query
        , headers : List ( String, String ) -- Authorization: Bearer … when given
        , body : Maybe Json.Encode.Value
        , timeout : Maybe Duration
        , outcomes : List (Outcome a) -- this endpoint's own answers (rule 0, §4.3)
        , expect : Expect a -- JSON decoder, or "no body expected"
        }
    | Refused { endpoint : String, requires : String } -- becomes Unsupported when sent
```

**The entire public surface of `Matrix.Http`**, as a fixed list. Adding to it
requires an entry in the decision log.

| Function | Purpose |
|---|---|
| `send : (Result Error a -> msg) -> Request a -> Cmd msg` | Run it as a command. |
| `toTask : Request a -> Task Error a` | Run it as a task, for chaining with `Task.andThen`. |
| `map : (a -> b) -> Request a -> Request b` | Change the result. |
| `withAccessToken : AccessToken -> Request a -> Request a` | For endpoints where auth is optional. |
| `withTimeout : Duration -> Request a -> Request a` | HTTP timeout. |
| `withDecoder : Decoder b -> Request a -> Request b` | Read a successful body with your own decoder; error handling stays the same. |
| `custom : { … } -> Request Value` | Escape hatch for any endpoint the SDK doesn't cover. |
| `Error(..)`, `errorToString`, `retryAfter`, `MatrixError` and its accessors | §4.3 |
| `Anonymous`, `Public`, `Authenticated` | §4.1 |

**Rules (the anti-TaskChain constitution):**

1. A `Request` is exactly one HTTP request. There's no `andThen`, `map2` or
   `sequence` on `Request`, ever. Chaining uses `Task.andThen` on `toTask`.
2. The version is chosen once, when the `Request` is built, by a plain function
   of `ServerVersion` (§4.4).
3. `send` and `toTask` never retry, refresh, wait, log or change state.
4. The list above is fixed (see decision D2).
5. The example in §4.7 must keep fitting on one screen. Every design change is
   checked against it.

Under the hood, `send` uses `Http.request` with `Http.expectStringResponse`,
`toTask` uses `Http.task` with `Http.stringResolver`, and both share one
`Http.Response String -> Result Error a` function. A `Refused` request never
touches the network: it's `Task.fail (Unsupported …)`.

**Why a value and not a `Cmd`:** elm-test can't look inside a `Cmd`. A
`Request` is data, so tests can check its method, URL and body for every spec
version without a server (§6). The same pattern is used by
`dillonkearns/elm-graphql` (`queryRequest … |> Graphql.Http.send GotResponse`).

**A cost to document:** a `Request` contains functions. If you use the Elm
debugger's import/export, keep the *input record* in your model or messages and
rebuild the `Request` when retrying. Rebuilding costs nothing.

### 4.3 Errors: one representation per answer

```elm
type Error
    = Unsupported { endpoint : String, requires : String } -- never sent: server too old
    | Unreachable UnreachableReason -- nothing came back
    | RateLimited { retryAfter : Maybe Duration }
    | NotLoggedIn { softLogout : Bool }
    | Rejected MatrixError -- the server understood, and said no
    | BadResponse { status : Int, body : String, problem : String }

type UnreachableReason
    = NetworkError
    | Timeout
    | BadUrl String
```

**Rule 1: `Error` only contains answers that *any* request can get.** An answer
that's a normal outcome of one endpoint is part of that endpoint's result type.
Examples: `M_USER_IN_USE` on `checkUsernameAvailability` becomes `Ok False` (as
it already does today), and user-interactive auth on registration becomes
`Ok (NeedsMoreAuth session)`. No endpoint ever has an impossible error
constructor such as "media not found" on `/login`.

**Rule 2: one classification function, first match wins.**

| # | If the response… | Result |
|---|---|---|
| 0 | matches one of the endpoint's own declared outcomes | `Ok` (that endpoint's result type) |
| 1 | was never sent (the server's versions don't include the endpoint) | `Unsupported` |
| 2 | never arrived (network error, timeout, bad URL) | `Unreachable` |
| 3 | has errcode `M_LIMIT_EXCEEDED`, or HTTP status 429 | `RateLimited` |
| 4 | has errcode `M_UNKNOWN_TOKEN` or `M_MISSING_TOKEN` | `NotLoggedIn` (with the `soft_logout` flag) |
| 5 | has any other errcode | `Rejected` |
| 6 | is a success status and the body decodes | `Ok` |
| 7 | is anything else | `BadResponse` |

- An errcode in the body always wins over the HTTP status. That protects
  against servers that use odd status codes.
- So `BadResponse` can never carry a readable errcode, and `Rejected` can never
  carry the codes from rules 3–4. A table test and a fuzz test check this.
- Error codes are plain `String`s. Each endpoint's docs list the codes the spec
  mentions for it, generated from the ledger.

**Rate limits in browsers.** The spec's recommended CORS headers
(`Access-Control-Allow-Origin/Methods/Headers`) don't include
`Access-Control-Expose-Headers`, and `Retry-After` isn't one of the response
headers browsers expose by default. So a browser app will usually *not* see
`Retry-After`. `retryAfter` reads the header when it's visible (delta-seconds
form only) and otherwise falls back to the body's `retry_after_ms`, which the
spec deprecated in v1.10 but which is often the only value a browser can read.

### 4.4 Choosing the variant for a server version

- **Spec versions are ordered numerically.** `"r0.6.1"` and `"v1.19"` are
  parsed into comparable values, every `r0.x` sorts before every `v1.x`, and an
  unknown future version such as `"v1.20"` still sorts correctly. Strings that
  don't parse are ignored.
- **Each variant declares where it starts.** The newest variant whose `since`
  is at or below the newest version the server advertises wins. A variant can
  also be enabled by an unstable feature flag, for the rare cases that need one.
- **`ServerVersion.naive`** (nothing known about the server) picks the newest
  variant.
- **Server too old for every variant:** the request becomes `Refused`, and
  sending it gives `Unsupported`.

```elm
-- Internal.Endpoint sketch: joinRoom is one of the few Tier 1 endpoints whose
-- *request* changed in v1.x (`via` added in v1.12, `server_name` removed in v1.14).
joinRoom creds input =
    let
        join prefix viaParam =
            Endpoint.post (joinPath prefix input.room)
                |> Endpoint.query viaParam (List.map ServerName.toString input.via)
                |> Endpoint.jsonBody (joinBody input)
                |> Endpoint.expectJson (D.field "room_id" RoomId.decoder)
    in
    Endpoint.choose creds
        [ Endpoint.since "v1.12" (join "v3" "via")
        , Endpoint.since "v1.1" (join "v3" "server_name")
        , Endpoint.since "r0.0.0" (join "r0" "server_name") -- oldest version: whatever the ledger says
        ]
```

When a new spec version comes out, nothing needs editing unless the ledger
reports a request-side change for an endpoint.

### 4.5 Decoding: accept every shape the spec ever allowed

Requests are versioned; responses are not. Every decoder accepts the response
as it looked in *any* spec version.

- **A field is required only if the response is useless without it**
  (`next_batch`, `event_id`, `access_token`). Everything else is a `Maybe`, or
  defaults to what the spec says a missing value means (usually "empty").
- **Moved fields are read from both places**, new location first. For example,
  `prev_content` is read from `unsigned` (v1.2+) or from the top level (older).
- **One bad item never sinks a whole response.** Events in a sync come from
  other people's servers. Lists of events decode item by item and drop the ones
  that don't parse, instead of failing the entire sync. ID decoders accept
  historical user IDs (see the open PR "Decode invalid Matrix user IDs").
- **Unknown fields are ignored but never lost:** every response type has
  `raw : T -> Json.Value`.

That removes every `DecoderProfile` and `include*` switch from the current code.

### 4.6 Response types and input records

- **Responses are opaque types with accessor functions, plus `raw`** (decision
  D7). Adding a field the spec introduces is then a minor release. If
  `LoginResponse` stayed a record alias, adding a field would be a major release.
  Opaque types mean many small exposed functions, each of which needs
  documentation. Claude writes those, following
  [the documentation style guide](documentation-style.md) (decision D18).
- **Time:** durations in public signatures are `Duration` from
  `ianmackenzie/elm-units`, and timestamps are `Time.Posix` (decision D14).
- **Inputs are records.** Optional fields are `Maybe`.
- **Event content is always the user's to decode:** `content : ClientEvent -> Value`.

So there are three ways to read any answer, from easy to free: accessors, `raw`,
or `Matrix.Http.withDecoder`.

### 4.7 The one-screen check

If this stops fitting on one screen, the design has drifted.

```elm
type alias Creds =
    { baseUrl : HomeserverUrl, serverVersion : ServerVersion, accessToken : AccessToken }

-- 1. Find out which spec versions the server speaks.
Versions.fetchServerVersion { baseUrl = url }
    |> Matrix.Http.send GotVersions

-- 2. Log in.
Login.loginWithPassword { baseUrl = url, serverVersion = version }
    { identifier = Login.MatrixUser { username = "alice" }
    , password = password
    , deviceId = Nothing
    , initialDeviceDisplayName = Just "My game"
    }
    |> Matrix.Http.send GotLogin

-- 3. Keep syncing.
GotSync (Ok response) ->
    ( { model | game = applyMoves response model.game }
    , Sync.sync model.creds { since = Just (Sync.nextBatch response), timeout = Just (Duration.seconds 30), filter = Nothing }
        |> Matrix.Http.send GotSync
    )

GotSync (Err (Matrix.Http.RateLimited _)) ->
    ( model, Process.sleep 5000 |> Task.perform (\_ -> SyncAgain) )

-- 4. Send a game move.
Events.sendMessage model.creds
    { roomId = room
    , eventType = "me.noordstar.game.chess.move"
    , content = encodeMove move
    , transactionId = TransactionId.fromInt model.nextTxn
    }
    |> Matrix.Http.send SentMove
```

---

## 5. Spec versions

- **Supported:** every stable release, `r0.0.0` to `v1.19`.
- **Unstable features** (the `unstable_features` from `/versions`): not
  supported unless a specific one is added deliberately.
- **What a quick probe found for Tier 1** (a throwaway script; the real ledger
  will replace these numbers):
  - Across v1.1 to v1.19, only seven of the 43 operations changed their
    *request*: `login` (`refresh_token` flag, v1.3), `joinRoom` (`via` v1.12,
    `server_name` removed v1.14), `getRoomEvents` (`from` optional since v1.3),
    `sync` (`use_state_after`, v1.16), `getRoomStateWithKey` (`format`, v1.16),
    `defineFilter` (`unread_thread_notifications`, v1.4), and the profile
    endpoints (generic fields in v1.16; before that only `displayname` and
    `avatar_url`, under different operation IDs).
  - Everything else was new response fields (handled by §4.5), the `r0`/`v3`
    path prefix, or the spec describing existing behaviour more precisely.
  - Endpoints that don't exist before some version: `getAuthMetadata` (v1.15),
    `getWellknownSupport` (v1.10), `refresh` (v1.3), `registrationTokenValidity`
    (v1.2). In the r0.x files that carry operation IDs (r0.3.0 onwards),
    `getWellknown`, `getLoginFlows`, `logout_all`, `checkUsernameAvailability`
    and `getOneRoomEvent` first appear in r0.4.0, and `getCapabilities`,
    `getAccountData` and `getAccountDataPerRoom` in r0.5.0. "First appears"
    means first *documented*. For r0.0.0–r0.2.0, the ledger has to match by path.

---

## 6. Verification architecture (outline; details in the verification plan)

```
 matrix-spec tags ──► tools/spec-ledger ──► ledger.json ──┬─► tests/Generated/*.elm  (facts + examples)
  (28 releases)        (Python 3 + PyYAML)       ▲         ├─► docs/supported.md      (coverage table)
                                                 │         └─► scaffold drafts        (one-time, then owned by people)
                                        spec/triage.yaml
                                        (hand-reviewed labels)
```

**What gets committed** (decision D16): only what people read or edit.

| File | Committed? | Why |
|---|---|---|
| `spec/triage.yaml` | yes | Hand-written judgment; it can't be regenerated. |
| `docs/supported.md` | yes | A result people read on GitHub. CI fails if it's out of date. |
| Scaffold drafts | yes | They become hand-owned source files the moment they're written. |
| `ledger.json` | no (git-ignored) | An intermediate result, regenerated from immutable spec tags. |
| `tests/Generated/*.elm` | no (git-ignored) | Regenerated before every test run, locally and in CI. |

- **Sources.**
  - For v1.2 and later, the bundled files (`https://spec.matrix.org/v1.N/client-server-api/api.json`)
    are used, because their references are already resolved.
  - For v1.1 and all r0.x versions, the per-file YAML at each git tag is used,
    because the v1.1 bundle loses the `/_matrix/client/v3` path prefixes.
- **Endpoint identity is method + path template, not `operationId`.** The
  r0.0.0–r0.2.0 files have no operation IDs at all, and some IDs were renamed
  (for example `getDisplayName` became `getProfileField` in v1.16). A small
  alias table records the renames.
- **`spec/triage.yaml`.** The ledger's change list is a *worklist*, not the
  truth. Many changes are the spec documenting existing behaviour more
  precisely. Each change gets a label (`request`, `response` or `docs-only`).
  This is the one place where judgment is recorded, and it's short and
  reviewable.
- **Generated code lives only in `tests/`, never in `src/`.**
  - Request tests check each endpoint's method, path, query and body keys for
    every spec version.
  - Example tests decode every example response from every spec version.
  - Classification tests cover the rules in §4.3, with a fuzz test for the
    invariants.
- **Later, not in 4.0:** smoke tests against a real homeserver in Docker.

---

## 7. Repository layout

```
CLAUDE.md                orientation for new Claude sessions
docs/
  architecture.md        this file
  roadmap.md             the work list, phase by phase
  documentation-style.md code organisation and documentation rules
  supported.md           generated coverage table (committed: people read it)
  CONTRIBUTING.md
examples/
  chat/                  minimal client: login → sync loop → send
  game/                  tiny multiplayer game (own elm.json, uses ../../src)
spec/
  triage.yaml            human-reviewed change labels (committed)
src/
  Internal/              Endpoint, Decode, Grammar
  Matrix/
    Http.elm
    Api/*.elm
    Types/*.elm
tests/
  Generated/             written by tools/spec-ledger; git-ignored
  ...                    hand-written tests
tools/
  spec-ledger/           Python 3 + PyYAML; writes ledger.json and cache/ (git-ignored)
.github/workflows/ci.yml
```

---

## 8. Dependencies

| Dependency | Decision |
|---|---|
| `elm/core`, `elm/json`, `elm/http`, `elm/url`, `elm/time` | keep |
| `elm/parser` | keep (ID grammar) |
| `ianmackenzie/elm-units` | keep; every public duration is a `Duration` (D14) |
| `micahhahn/elm-safe-recursion`, `miniBill/elm-fast-dict`, `noordstar/elm-iddict` | remove, together with the 3.x code |
| `elm/bytes` | not in 4.0; arrives with media in Tier 2 |
| `elm-explorations/test` | keep (test dependency) |

**Tooling.**

- Installed on this machine: `elm` 0.19.1, `elm-format` 0.8.5, Python 3 with PyYAML.
- Not installed: Node.js, which `elm-test`, `elm-review` and `elm-doc-preview`
  need. Until it's installed, the tests only run in CI.

---

## 9. Migration from the current tree

**Port, don't rewrite.** Files are moved with `git mv` and then edited, so the
diffs show modifications rather than replacements. Proposing a rewrite of
anything outside the "Rewrite" row is a red flag, and you can veto it.

| What happens | Files |
|---|---|
| Keep, few or no edits | `Matrix.Types.{AccessToken, RefreshToken, DeviceId, EventId, RoomId, RawEvent, StrippedStateEvent, DiscoveryInformation}`; `BatchToken` and `DeviceEvent` (these two exist only in `stash@{0}`); `Internal.Grammar.*` and their tests; `Internal.Tools.{DecodeExtra, EncodeExtra, ParserExtra}` |
| Targeted fix | `HomeserverUrl` (keep the path prefix), `ServerVersion` (version ordering), `UserId` (wrap `Grammar.UserId` directly instead of `Internal.Values.User`) |
| Port mechanically | the 10 `Matrix.Api.*` modules: only the signature shape changes; paths, bodies and docs stay |
| Merge | `ClientEvent` and `SyncEvent` share one internal decoder; the decoder profiles go away |
| Port with surgery | `Matrix.Types.Sync`: start from the **committed** version, which already decodes `next_batch`, device lists and all four room sections. Keep its accessors; switch it to §4.5 decoding. The stashed manual refactor rewrote this file; use its ideas, not the file. |
| Rewrite (only these) | `Matrix.Http` internals; `VersionedEndpoint`, which becomes `Internal.Endpoint` |
| Delete (kept in tag `3.6.1`) | `Matrix.elm`, `Matrix.{Event, Invite, Room, Settings}`, `Matrix.Api` (re-export hub), `Types.elm`, `Internal.Api.*`, `Internal.Values.*`, `Internal.Config.*`, `Internal.Filter.*`, `Internal.Tools.{Hashdict, Mashdict, Timestamp, VersionControl, Json, StrippedEvent}`, and their tests |

**Order of the first steps (Phase 0).**

1. Leave `stash@{0}` ("Manual refactor attempt") as it is. It won't be
   restored, but `BatchToken`, `DeviceEvent` and `RoomId.equals` are borrowed
   from it. The working tree is back at the last commit, which compiles.
2. Commit the planning documents.
3. Fix the known bugs: the `Bearer:` header, the errcode-only error body, and
   the dropped base URL path.
4. Delete the 3.x code and dependencies listed above.
5. Add CI.
6. Start Phase 1 (`Matrix.Http`, `Internal.Endpoint`, `Internal.Decode`).

---

## 10. Branches and releases

- **`v4-refactor` is the only trunk until 4.0.0.** An experiment is a branch or
  a scratch module with a time limit, and what you learn from it goes into the
  decision log. No new folders.
- **After that, the existing flow stays** (decision D17): feature branches go to
  `develop`, and only `develop` merges to `main`.
- **CI (GitHub Actions)** runs, for every push:
  - `elm make --docs=docs.json`
  - `elm-format --validate`
  - regenerating the ledger and `tests/Generated`, then `elm-test`
  - failing when `spec/triage.yaml` has no label for a detected change, or when
    `docs/supported.md` is out of date
  - compiling `examples/`
- **Release:** merge `develop` to `main`, `elm bump`, tag, `elm publish`. The
  package stays `noordstar/elm-matrix-sdk-beta`. `docs/before-merge.md` shrinks
  to whatever CI can't check.

---

## 11. Decision log

| # | Decision | Date |
|---|---|---|
| D1 | 4.0 is a low-level toolbox: one function per endpoint, no `Vault`. | 2026-10-29 |
| D2 | Endpoints return `Request a`: one HTTP round trip, no sequencing, a fixed public surface (§4.2). | 2026-10-29 |
| D3 | Credentials are plain extensible records holding only `baseUrl`, `serverVersion` and `accessToken`. No `Session` type, no phantom types. | 2026-10-29 |
| D4 | `Error` has six constructors with ordered classification; endpoint-specific answers live in result types. | 2026-10-29 |
| D5 | Requests are versioned; decoders accept every shape the spec ever allowed. | 2026-10-29 |
| D6 | Spec versions are ordered numerically; variants declare `since`. | 2026-10-29 |
| D7 | Responses are opaque with accessors and `raw`; inputs are records. | 2026-10-29 |
| D8 | All stable releases r0.0.0–v1.19 are supported; unstable features are opt-in. | 2026-10-29 |
| D9 | Generated code lives in `tests/`, never in `src/`. | 2026-10-29 |
| D10 | Existing files are ported, not rewritten (§9). | 2026-10-29 |
| D11 | 4.0.0 ships as `noordstar/elm-matrix-sdk-beta`. | 2026-10-29 |
| D12 | One repo and one trunk (`v4-refactor`) until 4.0.0. | 2026-10-29 |
| D13 | Endpoint modules follow the spec's sections, as in §3. | 2026-10-29 |
| D14 | Durations in public signatures use `Duration` (elm-units); timestamps use `Time.Posix`. | 2026-10-29 |
| D15 | The `Matrix.Api` re-export hub is deleted (it stays in git history). | 2026-10-29 |
| D16 | Commit what people read or edit (`triage.yaml`, `supported.md`, scaffold drafts); git-ignore what can be regenerated (`ledger.json`, `tests/Generated`). | 2026-10-29 |
| D17 | Keep `develop`: feature branch → `develop` → `main`. | 2026-10-29 |
| D18 | Claude writes the documentation for exposed functions, following `docs/documentation-style.md`. | 2026-10-29 |

## 12. Parking lot

Ideas that are deliberately *not* being worked on. Writing them down here is how
they stay out of the code.

- A stateful client / `Vault` (layer 3, after 4.0).
- Standard event content decoders (`m.room.member`, power levels, ...),
  generated from the spec's event schemas.
- A retry helper module (layer 2).
- OAuth 2.0 login (v1.15+). It needs entropy from JavaScript, because
  `elm/random` isn't cryptographically secure.
- Media (Tier 2): downloads through `Http.expectBytesResponse` and `data:` URLs
  for avatars and thumbnails.
- Smoke tests against Synapse in Docker.

## 13. Resolved questions

| Question | Answer |
|---|---|
| A. Module names and grouping | As proposed in §3 (D13). |
| B. `Duration` or plain milliseconds | `Duration` (D14). |
| C. Delete the `Matrix.Api` hub | Yes (D15). |
| D. Credential alias names | `Anonymous`, `Public`, `Authenticated` (§4.1). |
| E. Commit generated files | Only results people read; intermediates are git-ignored (D16). |
| F. Branch model after 4.0.0 | Keep `develop` (D17). |
