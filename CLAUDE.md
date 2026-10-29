# elm-matrix-sdk-beta

An Elm package (`noordstar/elm-matrix-sdk-beta`) for talking to Matrix
homeservers. Version 4.0.0 is being built on the `v4-refactor` branch as a
low-level toolbox: one function per Matrix endpoint, returning a `Request a`
that becomes a `Cmd` or a `Task`. There's no `Vault` or other stateful client.

## Read these before writing code

1. [docs/roadmap.md](docs/roadmap.md): which phase we're in, and the next
   unticked task.
2. [docs/architecture.md](docs/architecture.md): layers, the module map, the
   contracts for `Request`, `Error` and credentials, and the decision log
   (D1–D18). Don't contradict a decision silently. Propose a new log entry
   instead.
3. [docs/documentation-style.md](docs/documentation-style.md): code organisation
   and documentation rules. Every module has a publishable module doc with
   `@docs`. Every function and type, exposed or not, has a `{-| -}` doc comment.
   Definitions are sorted alphabetically, unless the order is a
   model → update → view flow. The tone follows `elm/core` and `elm/browser`:
   friendly and hand-holding.

## Working agreements

- **Port, don't rewrite.** Move existing files with `git mv` and edit them, so
  diffs stay small. Only the files in the "Rewrite" row of architecture §9 may
  be rewritten.
- **Never take Matrix spec details from memory.** Check paths, parameters,
  fields and version boundaries against the machine-readable spec:
  - v1.2 and later: `https://spec.matrix.org/v1.N/client-server-api/api.json`
  - v1.1 and r0.x: the YAML files in `github.com/matrix-org/matrix-spec` at each
    release tag
- **Keep the tree compiling.** Every session ends with code that compiles.
  Unfinished work goes on a branch.
- **Git is Bram's.** Don't commit, push or touch `stash@{0}` unless Bram asks.
  The stash may be read and borrowed from (see the roadmap).
- **Bram makes the creative calls:** API shape, naming, and the voice of the
  docs. Claude does the labour-intensive parts and keeps them verifiable.

## Tools

| Purpose | Command | Notes |
|---|---|---|
| Build and check docs | `elm make --docs=docs.json` | `docs.json` is git-ignored |
| Format | `elm-format --yes src tests` | CI uses `elm-format --validate src tests` |
| Tests | `npx elm-test` | Needs Node.js, which isn't installed on Bram's Windows machine yet, so tests run in CI |
| Spec ledger (Phase 2) | `python tools/spec-ledger/...` | Python 3 with PyYAML is installed |

The machine runs Windows. Both Git Bash and PowerShell are available.
