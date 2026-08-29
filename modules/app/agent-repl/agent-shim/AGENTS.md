# agent-shim/

The shim ecosystem. Its responsibility is EXCLUSIVELY facilitating agent-backend
interaction: driving a vendor's agent SDK/harness and surfacing everything it
produces on the wire. Frontend serving, merge/workspace state, and render-state
derivation never live here.

Layout: one directory per VENDOR (`claude/`, a future `codex/`), each holding
that vendor's shim and its vendor-facing services — `claude/shim/` (the
per-session SDK subprocess, the STREAM plane) and `claude/shim-sidecar/` (the
FILE plane: it reads the vendor's own on-disk transcripts and spools) — plus
the vendor-neutral `shim-store/` (the durable event store) and `logging/go`
(the canonical structured logging API every one of these runtimes records
through) at this level. `wire/` is retired from the store and the sidecar and
survives only for the daemon's remaining import; see its own AGENTS.md.

The vocabulary is `store.v1` and `conversation.v1`, not the old `agentshim.*` /
`protocol.v1` packages. `store.v1.ShimStore` is a Connect service the shim and
the sidecar call over a UNIX domain socket — WriteBatch, OpenAgentSession,
WatchAgentSession, ReadAgentPage, GetWorkflow, GetLiveWork, GetSidecarCursors,
with no dial protocol and deliberately no health verb. Its `StoreEntry`
envelope adds only storage concerns (plane, dedup `write_id`, `upsert_key`
identity, pageability) around the `conversation.v1` facts it carries; the store
never interprets or re-derives that content. THE DAEMON NEVER IMPORTS OR CALLS
`store.v1` — the daemon's read path is `shim.v1`, and the isolation is enforced
at codegen.

## What belongs in a shim-wire package, and what does not

`agent-shim/` owns `agentshim.core.v1` and `agentshim.data.v1` — the packages
that describe what a VENDOR produced. `agentshim.frontend.v1` is the daemon's
resolved surface and is NOT ours, even though frontend messages routinely embed
ours.

The test is not "which component sends it" but **is there vendor material under
it?**

- A message that CARRIES or DESCRIBES something the vendor's SDK produced
  belongs in the shim-wire packages, even when a frontend is the only reader.
- A message with NO underlying vendor message, that never crosses the shim UDS,
  belongs on the frontend surface — putting it here would claim the vendor
  produced something it never did.

### Worked example: the daemon-held prompt queue

When the user submits a prompt during a turn, the daemon queues it, has a
classifier judge whether it should interject, and — if it should — interrupts
the turn and submits it. The frontend shows the queue with per-entry
classification, and offers force / accept / cancel.

Every message in that feature lives in `agentshim.frontend.v1`, and none of it
comes near `core.v1` or `data.v1`:

- **`QueueView`** — the entries and their classifications. There is no vendor
  message for "a prompt the user typed that we have not sent yet". It is an
  artifact of the DAEMON's decision to hold the prompt back; the vendor never
  saw it and would not recognize it.
- **`QueueForceCmd` / `QueueAcceptCmd` / `QueueCancelCmd`** — none of these
  crosses the shim UDS. They are a user-facing *representation* of a lower-level
  operation the shim wire already has: an interject is an `Interrupt` followed
  by a `SubmitPrompt`, both of which ARE shim-wire messages. The queue command
  is the intent; the shim wire carries the mechanism. Adding a queue arm to
  `core.v1` would invent a vendor concept out of a UI affordance.

Contrast `HeartbeatView`: also a frontend-only frame, also invisible to the
shim's own consumers — but it EMBEDS `core.v1.HeartbeatProgress`, which the
vendor really did emit. The vendor material stays in `core.v1`; only the
per-frontend envelope around it is `frontend.v1`.

The practical consequence: **a shim-wire package should never grow a message
because a frontend needed somewhere to put something.** If the shim would not
produce it and the shim would not consume it, it is not shim material.

Dependencies: the protos under `proto/src/` — `store/v1` and
`conversation/v1` for everything at this level — with the generated Go in
`proto/gen/go/` (module `agentrepl/proto`; the Connect handlers and clients
live in `proto/gen/go/store/v1/storev1connect`).
