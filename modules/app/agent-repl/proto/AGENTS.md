# proto/

The agent-shim protocol definitions. The `.proto` files ARE the contract,
including behavioral semantics as normative comments. Three packages:
`agentshim.core.v1` (envelope, lifecycle, control plane, store plumbing),
`agentshim.data.v1` (the full-fidelity data vocabulary: stream messages,
transcript lines, tool shapes, journal records), `agentshim.frontend.v1`
(the daemon→frontend resolved surface, protojson on the wire).

## The schema is TREATED as vendor-agnostic

The `agentshim.data.v1` shapes were derived from the Claude harness, so the
schema is not FACTUALLY vendor-agnostic — but it is BELIEVED and TREATED as
vendor-agnostic everywhere: no consumer may special-case a vendor, and new
code is written against the schema as if any vendor's shim could produce it.

**Remediation strategy for adding a new vendor (e.g. codex):** when a new
vendor's reality does not fit the schema, RESOLVE the incongruity by revising
the API — a breaking schema change is the expected and acceptable remedy (no
downstream customers exist). Do not bolt vendor-specific side-channels onto
the protocol. Breaking changes require explicit user approval first (see the
repo-root AGENTS.md wire-protocol rule).

## Which package does a new message go in?

`core.v1` / `data.v1` are SHIM-WIRE packages: they describe what a vendor
produced. `frontend.v1` is the daemon's resolved surface. Most frontend
messages compose shim material, which makes the boundary easy to blur.

The test is **is there vendor material under it?** — not which component sends
it. A message with no underlying vendor message, that never crosses the shim
UDS, belongs on the frontend surface; putting it in a shim-wire package claims
the vendor produced something it never did.

Worked example, spelled out in full in `agent-shim/AGENTS.md`: the daemon-held
prompt queue is entirely `frontend.v1`. `QueueView` describes a prompt the
daemon is holding back, which no vendor ever saw; the queue commands are a
user-facing representation of an interject, whose MECHANISM (`Interrupt` then
`SubmitPrompt`) is what actually crosses the shim wire. Contrast
`HeartbeatView`, which is `frontend.v1` but embeds `core.v1.HeartbeatProgress`
— the vendor material stays in `core.v1`, only the envelope is per-frontend.

## Codegen

`make` generates Go (`gen/go`, consumed by `daemon/`,
`agent-shim/shim-store/`, `agent-shim/claude/shim-sidecar/`) and TS
(`gen/ts`, consumed by `agent-shim/claude/shim/`, `webapp/`). `make lint`
syntax-checks without emitting, and also enforces the structural invariants
protoc cannot see (below).

Dependencies: protoc, protoc-gen-go, @bufbuild/protoc-gen-es.

## Enforced structural invariants

**I6 — durable isolation.** `frontend/v1/durable.proto` is the persistence
evidence layer. No other `frontend/v1` schema may import it or name a message
it declares; what a frontend needs from that evidence reaches it already
resolved (`ResponseUsageStamp`, `FooterAccountingCell`, `TokenBreakdownView`).
`check-durable-isolation.sh` enforces it, `make lint` runs it, so codegen
refuses a drifted schema instead of emitting bindings for the coupling. Prose is
unconstrained — comments are stripped before matching, because naming the
durable types is how the files that must not use them explain why.
`test-check-durable-isolation.sh` (run by `make validate`) drives the gate
against fixture trees in both directions, so the gate cannot silently degrade
into one that matches nothing.

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
