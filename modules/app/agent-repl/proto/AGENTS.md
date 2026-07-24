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
syntax-checks without emitting.

Dependencies: protoc, protoc-gen-go, @bufbuild/protoc-gen-es.
