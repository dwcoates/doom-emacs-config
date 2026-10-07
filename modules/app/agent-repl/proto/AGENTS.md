# proto/

The agent-shim protocol definitions. The `.proto` files ARE the contract,
including behavioral semantics as normative comments.

## The six surfaces

Every message belongs to exactly one, and **the package boundary IS the surface
boundary** — nothing straddles, so which surface a message is on is a fact the
compiler checks rather than a convention a reviewer holds. The six are ROOT
namespaces with no umbrella prefix. The design record is
`../docs/protobuf-design/figma-to-idl-redesign.md`; the earlier record it
supersedes is kept as `DESIGN-protobuf-surfaces.superseded.md`, and every
decision there stands unless the new record reopens it by name.

| package | directory | holds | importable by |
|---|---|---|---|
| `conversation.v1` | `conversation/v1/` | the message model — `MessageEntry`, `MessagePayload`, the content model, `TokenUsage`. Imports nothing; the leaf everything shares | everyone |
| `frontend.v1` | `frontend/v1/` | the UI components the webapp renders — `feed`, `topbar`, `sidebar`, `footer` | daemon, webapp |
| `agentrepl.v1` | `agentrepl/v1/` | the agent-repl API surface — `service AgentRepl` (the endpoints clients call), the frame envelope a subscriber receives, the connect snapshot, and the acks | daemon, webapp |
| `shim.v1` | `shim/v1/` | what traverses the daemon↔shim boundary and only that boundary: handshakes, commands, receipts, health, replay and page requests, `BookkeepingEntry`, and the `ExternalEntry`/`EntryDelivery`/`MessagePage` read half | shim, daemon |
| `store.v1` | `store/v1/` | the producer-side internal half: which plane observed a record, the store's write identity, anything a producer could not convert | shim, sidecar, store ONLY |
| `state.v1` | `state/v1/` | daemon-internal only, including the schema the daemon marshals into its own SQLite store | daemon |

**What decides membership is who produces a message and where it is routed** —
never what it is about. Subject is a judgment call, which is how a message whose
subject was ambiguous ended up with no home at all.

`conversation.v1` **imports nothing**, which is what makes it shareable: it is
the leaf every other surface depends on. So a shared type lives in the most
upstream package that needs it, and there is no vocabulary package.

`BookkeepingEntry` is `shim`, not `conversation`, and that is the routing
test doing real work: it is produced by the shim, consumed by the daemon, and
STOPS there — a client sees it only after the daemon resolves it into a view.
`ExternalEntry` is `shim` for the same reason, and it does NOT collapse now
that bookkeeping is beside it, because the store persists bookkeeping too.

## `store.v1` import discipline — the daemon gets the external half only

A stored record has two halves. `shim.v1`'s `ExternalEntry` may cross the
shim→daemon wire; `store.v1`'s internal half — which observation plane
produced the record, the store's dedup key, and anything the producer could not
convert — may not.

The internal half is its own PROTO PACKAGE, not merely its own file. That
distinction is the whole enforcement: every file in one proto package generates
into ONE Go package, so an `entry.proto` sitting beside `external.proto` would
put `Plane` and `dedup_key` in the same namespace as everything the daemon
legitimately imports — free for the taking. A separate package is a separate Go
import path and a separate TS module.

**No daemon or webapp source may import `store.v1`.** The producers and the
store import it freely; it is theirs. `make conversation-isolation` (invariant
I7) refuses the import at codegen time, in .go, .ts and .proto form, with prose
deliberately exempt.

The store's `seq` is in neither half. A position is the store's addressing, so it
rides `shim/v1/entry-delivery.proto`, whose `stored`/`live` oneof also
replaces the old `retention` field: a live record has no field to put a position
in, so nothing can advance a resume cursor past a position the store never
assigned.

The gate hangs off `codegen-gate`, which `make go`, `make ts` and `make lint`
all require, so no Makefile route can emit bindings for a tree that has drifted.
It has a self-test (`make test-check-conversation-isolation`) that drives the
real script against fixtures in both directions — a gate nobody has watched fail
is not a gate.

## The schema is TREATED as vendor-agnostic

No consumer may special-case a vendor, and new code is written against the
schema as if any vendor's shim could produce it. `conversation.v1` makes that
structural rather than aspirational.

**Remediation strategy for adding a new vendor (e.g. codex):** when a new
vendor's reality does not fit the schema, RESOLVE the incongruity by revising
the API — a breaking schema change is the expected and acceptable remedy (no
downstream customers exist), and it raises the package version (see "A package
version goes up only for a breaking change"). Do not bolt vendor-specific side-channels onto
the protocol. Breaking changes require explicit user approval first (see the
repo-root AGENTS.md wire-protocol rule).

## A package version goes up only for a breaking change

Owner ruling, 2026-10-01. A package's version (the `v1` in `frontend.v1`) goes
up for EVERY breaking change and ONLY for one:

- Breaking: a field's type or meaning changed, a tag reused, a message or arm
  removed without `reserved`, anything an older peer would misread.
- Not breaking, so the version stays: a new message, field or oneof arm, and a
  field retired with `reserved`. These are frequent and are hot reloaded.
- A version change is never hot reloaded: a deploy that finds one is blocked,
  and the footer asks for a full Emacs restart.
- The daemon database follows the same rule with its `wsm schema version`
  (`AGENTS.md`).

## Which package does a new message go in?

Ask **who produces it, and where is it routed** — the routing test from the
surface table above. It has one answer per message and is decidable by
inspection, which "what is it about" is not.

Worked example, spelled out in full in `agent-shim/AGENTS.md`: the daemon-held
prompt queue is entirely `frontend.v1`. `QueueView` describes a prompt the
daemon is holding back, which no vendor ever saw; the queue commands are a
user-facing representation of an interject, whose MECHANISM (`Interrupt` then
`SubmitPrompt`) is what actually crosses the shim wire, and those are
`shim.v1`.

Second worked example: `QueryLifecycle` is ABOUT the SDK query object rather
than about the conversation, which left it homeless under a subject test. It is
produced by the shim and routed to the daemon, so it is `shim.v1`, and there
is nothing further to decide.

## Codegen

`make` generates Go (`gen/go`, consumed by `daemon/`,
`agent-shim/shim-store/`, `agent-shim/claude/shim-sidecar/`) and TS
(`gen/ts`, consumed by `agent-shim/claude/shim/`, `webapp/`). `make lint`
syntax-checks without emitting. The structural invariants protoc cannot see
(below) are enforced by `codegen-gate`, a prerequisite of `go`, `ts`, and
`lint` alike.

Dependencies: protoc, protoc-gen-go, protoc-gen-connect-go,
@bufbuild/protoc-gen-es.

### Hand-written Go helpers (`helpers/go`)

`gen/go` is generated output only (`make clean` deletes it), so Go helpers
over the generated types that more than one module needs live in their own
module, `helpers/go` (`agentrepl/protohelpers`), which every consumer requires
and replaces by path exactly as it does `agentrepl/proto`.

- `rosterwalk` is THE ONE WALK over a `frontend.v1.WorkspaceRoster`'s rows:
  `FlattenRows` (a rows region, children depth-first) and `AllRows` (every
  grouping). The daemon and the sidecar both read the roster through it, and
  its test fails if any Go file in agent-repl walks `RosterRow.children` by
  hand, which is how the merge-queue verb once missed every child workspace.
- It sits under `proto/`, so the deploy staleness pathspec that already covers
  the proto tree covers it too; `bin/build-frontend.sh` checks it is present.

### Generator version pins

Every generator that touches the committed `gen/` tree is pinned to an exact
version (`package.json` for `protoc-gen-es`; `proto/Makefile` for
`PROTOC_GEN_GO_VERSION` and
`PROTOC_GEN_CONNECT_GO_VERSION`), because an unpinned fetch (`npx` without a
version, or whatever happens to be on `PATH`) regenerates the whole tree the
moment a new release ships, unrelated to the change actually being made. That
is exactly what happened the first time this was left unpinned: an unpinned
`npx @bufbuild/protoc-gen-es` picked up a new release mid-task and rewrote 107
files' generated-by headers with no schema change behind it.

`@bufbuild/protoc-gen-es` is a proto-local dev dependency installed by
`npm ci`, and the Makefile invokes `./node_modules/.bin/protoc-gen-es`
directly. `protoc-gen-go` and
`protoc-gen-connect-go` are plain Go binaries resolved off `PATH` instead, so
there is no per-invocation pin site for them. Install the exact pinned
versions with:

```
go install google.golang.org/protobuf/cmd/protoc-gen-go@v<PROTOC_GEN_GO_VERSION>
go install connectrpc.com/connect/cmd/protoc-gen-connect-go@v<PROTOC_GEN_CONNECT_GO_VERSION>
```

(substituting the versions currently pinned in `proto/Makefile`).

`make lint` runs `check-gen-version`, which fails with a clear message when
the `@generated by protoc-gen-es vX.Y.Z` headers under `gen/ts` or the
`protoc-gen-go vX.Y.Z` headers under `gen/go` disagree with the pinned
versions. `protoc-gen-connect-go` emits no version header, so it cannot be
gated the same way. `check-generated`'s byte-for-byte diff of a fresh regen
against the committed tree is what catches drift from that generator instead.

**Upgrade procedure**, for any of the three generators: update its exact pin;
for `protoc-gen-es`, update the dev dependency and lockfile; install the Go generator when applicable;
run `make all` to regenerate; and commit the version bump together with the
regenerated `gen/` tree in a single commit. Never let the pin and generated
bytes land separately.

## Enforced structural invariants

**I7 — shim-side isolation.** `store.v1` is the half of a stored record that
never crosses the shim wire. `check-conversation-isolation.sh` enforces it as a
`codegen-gate` target, which `make go`, `make ts`, and `make lint` all require —
so every Makefile route to bindings refuses a drifted tree instead of emitting
for the coupling and leaving the refusal to review. Prose is unconstrained:
comments are stripped before matching, because naming the forbidden package in
the comments of the code that must not use it is how the invariant is taught, and
a gate that punished the documentation would be deleted in a week.
`test-check-conversation-isolation.sh` (run by `make validate`) drives the real
script against fixture trees in both directions — a gate nobody has watched fail
is not a gate.

New structural gates hang off `codegen-gate`, so they inherit that coverage
without each having to be wired into every emitting target.

## Validation and coverage

For every `.proto` or `gen/` change, run:

```bash
make coverage
```

`make coverage` runs `bin/report-nonlisp-coverage.sh` with no arguments, so
it covers exactly the script's default components and never restates them. The
first of those is `proto`, which runs `make validate`: it lints, regenerates,
and rejects stale committed or untracked stubs. Every downstream Go and
TypeScript coverage suite follows. The command must pass. Generated `gen/go` and `gen/ts` files are contract artifacts,
never coverage subjects or threshold inputs; coverage applies only to
handwritten downstream Go and TypeScript source.
