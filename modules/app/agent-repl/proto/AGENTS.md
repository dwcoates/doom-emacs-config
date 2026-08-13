# proto/

The agent-shim protocol definitions. The `.proto` files ARE the contract,
including behavioral semantics as normative comments.

## The five surfaces

Every message belongs to exactly one, and **the package boundary IS the surface
boundary** — nothing straddles, so which surface a message is on is a fact the
compiler checks rather than a convention a reviewer holds. The five are ROOT
namespaces with no umbrella prefix; `agentshim` names the surface holding
shim-side internals, so it cannot also be what the other four hang under.
`DESIGN-protobuf-surfaces.md` is the authority.

| package | directory | holds | importable by |
|---|---|---|---|
| `agentshim.v1` | `agentshim/v1/` | shim-side internals: which plane observed a record, the store's write identity, anything a producer could not convert | shim, sidecar, store ONLY |
| `conversation.v1` | `conversation/v1/` | the message model — `MessageEntry`, its payloads, the content model, `TokenUsage`. Nothing about sessions, turns or machinery | everyone |
| `protocol.v1` | `protocol/v1/` | what traverses the daemon↔shim boundary and only that boundary: handshakes, commands, receipts, health, replay and page requests, `BookkeepingEntry`, and the `ExternalEntry`/`EntryDelivery` envelopes | shim, daemon |
| `frontend.v1` | `frontend/v1/` | what reaches a frontend client — forwarded `conversation` records AND novel messages the daemon synthesizes | daemon, webapp |
| `state.v1` | `state/v1/` | daemon-internal only, including the schema the daemon marshals into its own SQLite store | daemon |

**What decides membership is who produces a message and where it is routed** —
never what it is about. Subject is a judgment call, which is how a message whose
subject was ambiguous ended up with no home at all.

`conversation.v1` **imports nothing**, which is what makes it shareable: it is
the leaf every other surface depends on. So a shared type lives in the most
upstream package that needs it, and there is no vocabulary package.

`BookkeepingEntry` is `protocol`, not `conversation`, and that is the routing
test doing real work: it is produced by the shim, consumed by the daemon, and
STOPS there — a client sees it only after the daemon resolves it into a view.
`ExternalEntry` is `protocol` for the same reason, and it does NOT collapse now
that bookkeeping is beside it, because the store persists bookkeeping too.

## `agentshim.v1` import discipline — the daemon gets the external half only

A stored record has two halves. `protocol.v1`'s `ExternalEntry` may cross the
shim→daemon wire; `agentshim.v1`'s internal half — which observation plane
produced the record, the store's dedup key, and anything the producer could not
convert — may not.

The internal half is its own PROTO PACKAGE, not merely its own file. That
distinction is the whole enforcement: every file in one proto package generates
into ONE Go package, so an `entry.proto` sitting beside `external.proto` would
put `Plane` and `dedup_key` in the same namespace as everything the daemon
legitimately imports — free for the taking. A separate package is a separate Go
import path and a separate TS module.

**No daemon or webapp source may import `agentshim.v1`.** The producers and the
store import it freely; it is theirs. `make conversation-isolation` (invariant
I7) refuses the import at codegen time, in .go, .ts and .proto form, with prose
deliberately exempt.

The store's `seq` is in neither half. A position is the store's addressing, so it
rides `protocol/v1/entry-delivery.proto`, whose `stored`/`live` oneof also
replaces the old `retention` field: a live record has no field to put a position
in, so nothing can advance a resume cursor past a position the store never
assigned.

Both gates hang off `codegen-gate`, which `make go`, `make ts` and `make lint`
all require, so no Makefile route can emit bindings for a tree that has drifted.
Each has a self-test (`make test-check-conversation-isolation`) that drives the
real script against fixtures in both directions — a gate nobody has watched fail
is not a gate.

## `agentshim.data.v1` was DELETED

`data.v1` held a direct transliteration of the Claude SDK's JSONL: 267
messages whose names, arms, and fields were the vendor's own surface wearing a
protobuf hat. Nothing in the system was better off for it — a vendor shape
crossing the wire only moves the vendor knowledge downstream to the daemon and
the webapp, which is exactly where it must not live.

The replacement inverts the direction. The shim and the sidecar convert the
vendor's output to a vendor-AGNOSTIC model at the point of production, the
store persists only that model, and the daemon and webapp never learn a vendor
name. Anything a producer cannot convert is persisted as an explicit
unsupported arm rather than as raw vendor material. The contract is written
out in full in `FROZEN-conversation-v1.md`.

## The schema is TREATED as vendor-agnostic

The retired `data.v1` shapes were derived from the Claude harness, so the
schema was not FACTUALLY vendor-agnostic — but it was BELIEVED and TREATED as
vendor-agnostic everywhere: no consumer may special-case a vendor, and new
code is written against the schema as if any vendor's shim could produce it.
`conversation.v1` makes that belief structural instead of aspirational.

**Remediation strategy for adding a new vendor (e.g. codex):** when a new
vendor's reality does not fit the schema, RESOLVE the incongruity by revising
the API — a breaking schema change is the expected and acceptable remedy (no
downstream customers exist). Do not bolt vendor-specific side-channels onto
the protocol. Breaking changes require explicit user approval first (see the
repo-root AGENTS.md wire-protocol rule).

## Which package does a new message go in?

Ask **who produces it, and where is it routed** — the routing test from the
surface table above. It has one answer per message and is decidable by
inspection, which "what is it about" is not.

Worked example, spelled out in full in `agent-shim/AGENTS.md`: the daemon-held
prompt queue is entirely `frontend.v1`. `QueueView` describes a prompt the
daemon is holding back, which no vendor ever saw; the queue commands are a
user-facing representation of an interject, whose MECHANISM (`Interrupt` then
`SubmitPrompt`) is what actually crosses the shim wire, and those are
`protocol.v1`.

Second worked example: `QueryLifecycle` is ABOUT the SDK query object rather
than about the conversation, which left it homeless under a subject test. It is
produced by the shim and routed to the daemon, so it is `protocol.v1`, and there
is nothing further to decide.

## Codegen

`make` generates Go (`gen/go`, consumed by `daemon/`,
`agent-shim/shim-store/`, `agent-shim/claude/shim-sidecar/`) and TS
(`gen/ts`, consumed by `agent-shim/claude/shim/`, `webapp/`). `make lint`
syntax-checks without emitting. The structural invariants protoc cannot see
(below) are enforced by `codegen-gate`, a prerequisite of `go`, `ts`, and
`lint` alike.

Dependencies: protoc, protoc-gen-go, @bufbuild/protoc-gen-es.

## Enforced structural invariants

**I6 — durable isolation — IS RETIRED, along with `check-durable-isolation.sh`
and its self-test.** The gate existed only because the persistence evidence
layer sat INSIDE `frontend/v1` while being definitionally not frontend, so the
one thing separating the two was a grep over comments-stripped source. That layer
is `state.v1` now — its own package, its own Go import path, its own TS module —
and the compiler enforces what the script used to check. A gate that duplicates
the compiler is a gate that can only ever disagree with it.

**I7 — shim-side isolation.** `agentshim.v1` is the half of a stored record that
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

`make coverage` first runs `make validate`, which lints, regenerates, and
rejects stale committed or untracked stubs. It then runs the downstream daemon,
shim-store, shim-sidecar, wire, Claude shim, and webapp coverage suites. The
command must pass. Generated `gen/go` and `gen/ts` files are contract artifacts,
never coverage subjects or threshold inputs; coverage applies only to
handwritten downstream Go and TypeScript source.
