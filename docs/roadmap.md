# Roadmap to 4.0.0

This is the work list. [architecture.md](architecture.md) says *what* we're
building and why, and [documentation-style.md](documentation-style.md) says how
the code and docs should read. This file says what to do next.

**How to use it:**
- Tick boxes as work lands.
- Every session ends with a tree that compiles. Unfinished work goes on a
  branch, never into a broken `v4-refactor`.
- A phase is finished when its "Done when" line is true, not when every idea
  for it has been tried.

**Division of labour:** Claude does the tedious parts (endpoints, decoders,
accessors and their docs, the ledger, tests). Bram decides API shape and naming,
reviews the voice of the docs, and presses the release button. Pull requests
stay small: one spec section, or one phase step, each.

---

## Phase 0: Back to green

Starting point: `v4-refactor` at commit `77a1334`, which compiles.
`stash@{0}` ("Manual refactor attempt") is left as it is. Its pieces may be
borrowed, but it won't be restored.

- [x] Commit `docs/architecture.md`, `docs/documentation-style.md`,
      `docs/roadmap.md`, `CLAUDE.md` and `.gitignore` (commit `e9c2321`).
- [x] Borrow from the stash: `BatchToken.elm` and `DeviceEvent.elm` (from
      `git show stash@{0}:<path>`), and `RoomId.equals`.
- [x] Fix the `Authorization` header: `"Bearer: "` became `"Bearer "`.
- [x] Make the error body's `error` field optional. `errorHuman` is now a
      `Maybe String`; only `errcode` is required by the spec.
- [x] Make `HomeserverUrl` keep the base URL's path prefix, which restores the
      3.6.1 fix from #45. This also fixed a double `??` in query strings, and
      the `.well-known` lookups now always use the domain root. Tests are in
      `tests/Test/Types/HomeserverUrl.elm`.
- [x] Make `Matrix.Types.UserId` wrap `Internal.Grammar.UserId` directly.
- [x] Delete the 3.x code and the re-export hub, their tests, and the
      dependencies `elm-safe-recursion`, `elm-fast-dict` and `elm-iddict`.
      Update `exposed-modules` in `elm.json`.
- [x] Add `.github/workflows/ci.yml`: `elm-format --validate src tests`,
      `elm make --docs=docs.json` and `elm-test`.
- [ ] Commit and push, then check that CI passes on GitHub.

**Done when:** CI is green on `v4-refactor` and `src/` contains only v4 code.
Everything passes locally: `elm make --docs`, `elm-format --validate`, and all
19 tests (run in a browser, see `CLAUDE.md`). CI itself hasn't run yet.

## Phase 1: Foundation

- [ ] **Write the code design plan** (`docs/code-design.md`) before starting:
  - naming of endpoint functions and accessors, including how `operationId`s
    become Elm names
  - the endpoint module template
  - the public API of `Internal.Endpoint` and `Internal.Decode`
  - the test layout
- [ ] `Matrix.Http`:
  - `Request`, `send`, `toTask`, `map`, `withAccessToken`, `withTimeout`,
    `withDecoder` and `custom` (architecture §4.2)
  - `Error` and its classification rules, with table tests and a fuzz test
    (architecture §4.3)
  - `retryAfter`, which reads the header when it's visible and otherwise
    `retry_after_ms`
- [ ] `Internal.Endpoint`: spec version ordering, `since`, `choose`, and
      `Refused` (architecture §4.4).
- [ ] `Internal.Decode`: decoders that accept every shape, read moved fields
      from both places, and decode lists item by item (architecture §4.5).
- [ ] `ServerVersion`: version ordering and the newest advertised version.
- [ ] Port the existing 16 endpoints to `Authenticated a -> input -> Request a`
      (architecture §9, "Port mechanically").
- [ ] Merge the `ClientEvent` and `SyncEvent` decoders, and remove the decoder
      profiles.

**Done when:** the 16 existing endpoints compile in the new shape and have
hand-written request tests.

## Phase 2: Spec ledger and verification

- [ ] **Write the verification plan** (`docs/verification.md`) before starting:
  the ledger schema, endpoint identity (method + path template, plus the alias
  table), the `triage.yaml` format, and what the generated tests check.
- [ ] `tools/spec-ledger` (Python 3 + PyYAML). It reads:
  - the per-file YAML at each git tag, for r0.x and v1.1
  - `api.json` bundles, for v1.2 and later

  It writes `tools/spec-ledger/ledger.json`, which is git-ignored.
- [ ] `spec/triage.yaml`, with every detected change labelled.
- [ ] Generated `tests/Generated/*.elm` (git-ignored): request facts per
      endpoint and version, example responses, spec field names, spec anchors.
- [ ] Generated `docs/supported.md` (committed).
- [ ] Add the ledger to CI.

**Done when:** CI regenerates the ledger, and the 16 ported endpoints pass the
generated tests for every spec version.

## Phase 3: Tier 1 endpoints

One pull request per module, in this order. Each one is reviewed by Bram for
API shape and voice.

- [ ] `Discovery`, `Versions`, `Capabilities`, `Authorization`
- [ ] `Login`, `Sessions` (with whoami), `Tokens`, `Registration`
- [ ] `RoomMembership`, `RoomCreation`, `RoomDirectory`
- [ ] `Events` (get, `/messages`, send, redact)
- [ ] `RoomState`
- [ ] `Filtering`, then `Sync`
- [ ] `AccountData`, `Profile`, `SendToDevice`

**Done when:** `docs/supported.md` shows all 43 Tier 1 operations implemented
and verified.

## Phase 4: Try it ourselves, then release

- [ ] `examples/chat`: login → sync loop → send.
- [ ] `examples/game`: a tiny multiplayer game.
- [ ] README rewritten for 4.0.
- [ ] A documentation pass with `elm-doc-preview`.
- [ ] `develop` → `main`, `elm bump`, publish 4.0.0, post on Discourse.

**Done when:** 4.0.0 is on the Elm registry and the game example runs against
a real homeserver.

## Phase 5: Tier 2, in 4.x minor releases

Driven by what users ask for. Candidates: media, relations, threads, search,
directory, presence, typing, receipts, devices, push rules, knocking, upgrades,
report content, registration with user-interactive auth, and OAuth 2.0 login.
See the parking lot in architecture §12.
