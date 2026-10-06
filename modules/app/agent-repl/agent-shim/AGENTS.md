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
through) at this level. `wire/` is DELETED: the store and the sidecar speak
store.v1, and the daemon dropped its last import of it.

The vocabulary is `store.v1` and `conversation.v1`, not the old `agentshim.*` /
`protocol.v1` packages. `store.v1.ShimStore` is a Connect service the shim and
the sidecar call over a UNIX domain socket — WriteBatch, OpenAgentSession,
WatchAgentSession, ReadAgentPage, GetWorkflow, GetLiveWork, GetAgentByVendorTask,
GetDetachedWork, GetSidecarCursors,
with no dial protocol and deliberately no health verb. Its `StoreEntry`
envelope adds only storage concerns (plane, dedup `write_id`, `upsert_key`
identity, pageability) around the `conversation.v1` facts it carries; the store
never interprets or re-derives that content. THE DAEMON NEVER IMPORTS OR CALLS
`store.v1` — the daemon's read path is `shim.v1`, and the isolation is enforced
at codegen.

## What belongs in a shim-wire package, and what does not

The packages, and who owns each:

- **`conversation.v1`** — the shared conversation model: what a vendor's agent
  actually did. Produced by the shim (the stream plane, live from the SDK) and
  the sidecar (the file plane, read from the vendor's own transcripts). It is
  also everything the daemon is entitled to see.
- **`store.v1`** — the storage envelope around those facts, and the
  `ShimStore` Connect service. Callers are the shim and the sidecar, never the
  daemon; `store.proto`'s import-discipline header makes it a separate proto
  package precisely so a daemon import cannot get it for free, and
  `check-conversation-isolation.sh` refuses that import at codegen time.
- **`shim.v1`** — the daemon↔shim service, and only that boundary.
- **`agentrepl.v1`** — the daemon's client-facing service: the verbs a client
  calls and the streams it watches.
- **`frontend.v1`** — the daemon's RESOLVED views, the ones a client draws.

`agent-shim/` owns the first two. `frontend.v1` is the daemon's surface and is
NOT ours, even though frontend views routinely embed our material.

The test is not "which component sends it" but **is there vendor material
under it?**

- A message that CARRIES or DESCRIBES something the vendor's agent actually
  produced is `conversation.v1`, even when a frontend is the only reader.
- A message with NO underlying vendor fact, that never crosses to the store or
  the shim, belongs on the daemon's surfaces — putting it here would claim the
  vendor produced something it never did.

### Worked example: the daemon's held-prompt tray

When the user submits a prompt the daemon cannot deliver yet, the daemon HOLDS
it, classifies it, and shows it in a tray at the feed's tail with force /
cancel / accept actions.

None of that is ours. `frontend.v1`'s `daemon_hold.proto` says it directly: a
queued prompt is not a conversation record, it is daemon-owned pending intent
the vendor never saw, drawn beside the feed and never in it. So:

- **`DaemonHoldTray` / `HeldPrompt` / `HeldOffer`** are `frontend.v1`. There is
  no vendor fact for "a prompt the user typed that we have not sent yet"; it is
  an artifact of the DAEMON's decision to hold it back. It is never written to
  the store, so it has no `store.v1` envelope either.
- **The tray's actions** are `agentrepl.v1` requests (`UpdateHeldPrompt`,
  `AnswerHeldOffer`). None of them crosses to the store. They are the user's
  INTENT; the MECHANISM is an `Interrupt` followed by a `SubmitPrompt`, and
  that mechanism is what actually crosses the shim boundary. Giving the store
  or `conversation.v1` a held-prompt arm would invent a vendor concept out of a
  UI affordance.

Contrast a **subagent spawn announcement**. It is also surfaced to a client,
also invisible to the vendor's own transcript readers until it lands — but the
vendor really did spawn a subagent, so `AgentSubagent` / `AgentSubagentStart`
are `conversation.v1`, they ride a `store.v1` envelope into the store, and the
frontend view that draws the spawn merely embeds them. The vendor material
stays in the conversation model; only the per-client envelope around it is the
daemon's.

The practical consequence: **a shim-wire package never grows a message because
a frontend needed somewhere to put something.** If neither the shim nor the
sidecar would produce it, and the store would not hold it, it is not shim
material.

Dependencies: the protos under `proto/src/` — `store/v1` and
`conversation/v1` for everything at this level — with the generated Go in
`proto/gen/go/` (module `agentrepl/proto`; the Connect handlers and clients
live in `proto/gen/go/store/v1/storev1connect`).
