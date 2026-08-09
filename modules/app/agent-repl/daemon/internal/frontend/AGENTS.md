# daemon/internal/frontend/

The daemon's frontend surface: serves `agentshim.frontend.v1` frames as
protojson over UDS (Emacs) and WebSocket (webapp). Responsibility: translate
internal events and SSM state into the resolved frontend vocabulary
(`WorkspaceState`, `ConversationDelta`, `TypingDelta`, `TaskCatalog`,
`SessionView`), send `StateSnapshot` on (re)connect, and dispatch inbound
`FrontendCommand`s with `CommandAck`s. Frontends render what this package
sends and never derive state themselves.

## Delivery deltas are unsequenced, freshness is leased

Every connected frontend receives every emission immediately: same order, same
content, no frontend's render pace gating another's delivery. `PushWorkspaceState`
fans out under the client-registry lock, so an emission, a connect and a
disconnect are serialized against each other, and that is the whole mechanism.

## Commands run on per-workspace lanes, and the ack still means DONE

A connection's inbound commands used to run one at a time, inline in its read
loop, so a connection's throughput was the sum of every command's cost. A
workspace bring-up costs seconds, and Emacs opens every restored workspace at
startup: sixteen `open_workspace` commands behind each other starved later
commands past the client's ack deadline, and each starved command opened a
`client.command_unacked` failure card for a command the daemon had not even
read yet.

Acking on receipt was NOT the fix. The Emacs client treats a successful
`CommandAck` as COMPLETION: `openWorkspace`'s async bridge resolves on it,
`createSession` unblocks its await loop on it, `restartSession` reports the
session came back from it. An early ack would report an open workspace before
any shim existed.

So the ack contract is untouched and the CONCURRENCY changed instead
(`lanes.go`). Each command is routed onto a lane keyed by its workspace; a
lane runs its commands strictly in arrival order, and lanes run in parallel
with one another. A workspace's `open_workspace` therefore still precedes
every session command that followed it on the wire, while a wedged bring-up
can no longer delay another workspace's ack, and a startup costs
max(bring-up) rather than sum(bring-ups). Every workspace-less command (the
roster publish, the daemon-global controls) shares one global lane, which is
never behind any workspace's bring-up.

Concurrency across workspaces is not a new demand on `CommandHandler`: the
Emacs UDS connection and every webview connection already ran their read loops
in parallel. What is new is that ONE connection may have several commands in
flight, always for different workspaces.

A lane's queue is unbounded on purpose — every frame in it has already been
read off the socket and owes the client an answer — and closing a connection
DRAINS its lanes before disconnecting, for the same reason.

## Acks leave on a control lane, and the alarm measures the drain

A `CommandAck` used to share one FIFO with every bulk push. At connect that
lane holds the whole snapshot — 104 workspaces in the observed case — so an ack
the daemon produced in ~12ms reached Emacs ~15s later. The client's 10s
deadline fired first, opening a `client.command_unacked` failure card and a
degraded-link verdict for a command that had SUCCEEDED; the late ack then
healed it, which is why the class read as flaky rather than as one mechanism.
The same signature appeared on `open_workspace`, `submit_prompt` and
`publish_workspace_roster`.

Each outbox now has TWO lanes and drains the control one first. The bulk lane
keeps every property described below verbatim — push order, compaction keeping
the newest occurrence's position, snapshot-before-delta. The control lane
carries ONLY correlated, request-keyed replies: a command's `CommandAck` and the
one response frame a command may produce. Those are matched by `request_id`,
never applied as state, and no frontend derives anything from where they sit
relative to a push, so the FIFO's load-bearing property does not extend to them.
Within the control lane order is still push order, which is what keeps a health
view ahead of the ack that completes it.

ONE ordering pair is preserved explicitly. A `resync`'s ack still travels the
BULK lane, behind the snapshot the resync enqueued: the resync's answer IS the
snapshot, an ok=true ack arriving first would report the client current while it
still held the state it asked to replace, and the snapshot cannot join the
control lane without overtaking the deltas queued before it. Resync is the only
command that enqueues a bulk frame of its own.

Both lanes share ONE set of bounds. Soft, hard and grace are judged over the
combined depth, so the memory ceiling and the slow-consumer eviction are exactly
as strict as they were, and a control frame is refused at the ceiling like any
other. What changed is that a refused, stranded or unwritable frame now reports
itself UNDELIVERED to whoever queued it, rather than vanishing.

The control lane bounds how many frames sit AHEAD of a correlated reply, and
that is ALL it can bound. It cannot preempt a write already in progress: there
is one socket and one writer goroutine, so a consumer that stops reading blocks
the writer inside a single `writeFrame` and every queued ack waits behind it
whichever lane it is on. Emacs blocks its own event loop for seconds restoring
workspaces at startup, and one observed boot blocked this writer ~13s and then
delivered eighteen `open_workspace` acks within one millisecond of the host
reading again — five of them past the client's 10s deadline, each carded as
`client.command_unacked` against a daemon that had answered every one of them
successfully in under 4s.

Nothing said so. The commands' own overdue records named eighteen slow commands
and not the one blocked write they were all waiting on. So the write announces
itself WHILE it is stuck (`writeFrameWatched`, `server.go`), on the same
`ackDeadline` budget, naming `client_kind` and the held frame's lane, and it
reports its resolution — a stall announced with no end is indistinguishable
from a daemon that exited still blocked. The alarm only observes; the write's
error handling is untouched. The client end of the same mechanism is
`agent-repl--uds-drain-before-verdict` in `lisp/frontend-uds.el`: an ack alarm
reads the link before it declares anything lost, so a verdict is never passed
on bytes Emacs had been given but had not yet looked at.

The daemon's slow-ack alarm was measuring the wrong interval: it stopped at the
ack's ENQUEUE, so the dominant term — the drain — was invisible to the exact
alarm built to catch it. A latency sample now carries both numbers. `Enqueue` is
receipt through ack hand-off (the daemon's own share); `Delivery` is receipt
through those bytes reaching the socket, which is what the client's deadline
measures. `Slow()` judges `Delivery`, and the record is written by whichever of
the two halves — dispatch finishing, ack delivery — completes second. Reading
them together names the fault: a small `enqueue_ms` under a large `duration_ms`
is the outbound queue, not the handler.

## A saturated queue is compacted before the connection is given up on

Each connection's outbound queue is bounded. A hidden or backgrounded webview
consumes slowly and used to fill it during a busy turn, and the forced
reconnect-plus-full-snapshot replay that followed was a leading cause of slow
workspace switching.

A full queue is now COMPACTED first: every queued frame that a LATER queued
frame supersedes is replaced by that newer version, and the survivor keeps the
newest occurrence's position so nothing overtakes content that preceded it.
Only `WorkspaceState` (per workspace), `ProgressView` and `QueueView` (per
workspace+session) and `HeartbeatView` (per workspace+session+tool) supersede
anything — each is applied by both frontends as a WHOLESALE assignment, so the
newest value alone leaves the consumer in the state the whole run would have.

`TypingDelta` is deliberately excluded: it carries a `core.v1.ContentDelta`
chunk that GROWS an open block, so dropping one deletes prose. `StateSnapshot`
is excluded too — the lease is the browser's bounded freshness proof, and a
full lease queue stays a hard disconnect rather than a silent skip.

The hard disconnect remains for a queue still full of frames nothing may
replace. Its log line reports how many frames compaction freed, so "we gave up"
is always distinguishable from "we never tried".

## The host's queue is elastic, because Emacs is single-threaded

Compaction is no help when the backlog is all `ConversationDelta` and
`TypingDelta`, and that is exactly what the Emacs host accumulates while it is
busy. Emacs blocks its own event loop for seconds at a time restoring
seventeen workspaces at startup or mounting a webview, and every one of those
blocks used to fill the flat 256-frame queue with irreplaceable frames and get
the host evicted — five evictions in one observed boot-plus-probe cycle, each
costing a visible `uds-link: DOWN`, a reconnect, and a full snapshot replay.
That busyness is inherent to a single-threaded UI, not a defect in it.

So `ClientKindHost` alone gets an ELASTIC outbox: two bounds instead of one.
`soft` (the configured buffer, 256) is the depth at which the consumer is
declared behind; `hard` (`soft * hostBufferElasticity`, 4096) is the absolute
frame ceiling. Between them the queue keeps accepting, for up to
`hostStallGrace` (30s), PROVIDED the consumer drains at least one frame in that
window. Every other kind is built with `soft == hard` and no grace, which is
literally the old flat queue — a backgrounded webview's shedding behavior is
unchanged.

Eviction is preserved, not weakened; only the threshold for calling a busy
consumer a dead one moved. A refused push names which of the two limits it hit:
`hard_ceiling` (the memory bound, unconditional — no amount of recent drain
progress buys a queue past it) or `stalled` (under the ceiling, but not one
frame drained for the whole grace period, which is what a genuinely wedged
consumer looks like).

Severity is asymmetric on purpose. Evicting the HOST is a user-visible service
degradation — there is exactly one, it owns the UI, and Emacs logs its own
warning for the same event — so that record is `warn`. Every other kind's is
`info`: shedding a slow webview is the contract working. Transient pressure that
resolves without eviction is verbose, once per episode rather than once per
frame.

`WorkspaceRoster` is the one coalescable frame with a GLOBAL key: there is
exactly one roster for the whole editor, it is always whole and never a delta,
so any newer one is the entire truth.

## The workspace roster is retained here, and it is editor-global

Emacs is the roster's single author; this package is its retainer and fan-out.
`PublishWorkspaceRoster` validates a publication (positive revision, a set
`view` arm, a set `status` arm on every row and every descendant row), refuses
one whose revision does not advance — naming both revisions, never a silent
drop — retains it, and delivers it to every connected client. Retention and
fan-out are ONE operation under the delivery lock, the same lock a connect
registers under, so a connect racing a publication gets the roster exactly once
and never gets the older one after the newer.

The roster is EDITOR-GLOBAL, not workspace state. It carries no session or
workspace routing key, so `scopeFrame` passes it to every connection
explicitly rather than by falling through the default arm, and a session-scoped
webview renders the same sidebar the Emacs host does. It is not a
`StateSnapshot` field either: connect sends it as its own frame, and OMITS that
frame entirely when nothing has been published, so "no roster yet" stays
distinguishable from an empty roster.

Retention is IN-MEMORY ONLY, deliberately. The revision is monotonic per Emacs
BOOT, so a roster outliving its publisher would hold a revision a restarted
Emacs could not beat. A restarted daemon has no roster until Emacs republishes
on reconnect.

GUI stream connections also receive a renewable authoritative `StateSnapshot`
lease. Socket-open does not attest current state. The browser becomes current
only after it has decoded and atomically adopted a snapshot, and it expires all
live-state projections if three lease intervals pass without another snapshot.
The lease snapshot is enqueued under the same delivery lock as deltas, so its
revision gives the browser a bounded freshness proof without reintroducing a
viewer-acknowledgement gate. A full lease queue is a hard disconnect, never a
silent skip.

It used to be a GATE. A `WorkspaceState` travelled resolver → the frontends that
PAINT it → their acknowledgment → the frontends that merely OBSERVE it, so the
Emacs tab bar could never show a state the webview had not drawn. That bought
surface agreement at the price of a viewer-based attestation model, and it had a
hole of its own: an observer's reconnect snapshot was filtered to states a
painter had SETTLED, so a workspace whose first emission was still held was
OMITTED from Emacs entirely.

Both are gone. The SSM emits the same composite session connectivity, session
status, controller generation, and active faults to every frontend; no
viewer's render pass has a claim on those facts.

Emacs remains the authority for workspace membership. In a session webview,
the revisioned `WorkspaceState` is the sole authority for both the footer and
the current workspace's sidebar status. Non-current sidebar rows still use the
Emacs roster. This prevents two asynchronously delivered copies from presenting
different phases for the same current session.

`ClientKind` remains, and it was never about painting: it names the frontend
product behind a connection, fixed at accept from the endpoint that accepted it,
and it is the authority for the host-only frames and commands Emacs alone may
see.

- `/workspace-stream?workspace=<dir>` — `ClientKindGUIStream`, the rendering
  webview addressed by workspace. `WorkspaceScopeFromQuery` (scopequery.go)
  derives its `Scope` and admits only a workspace the daemon holds state for;
  every other outcome is a typed `*ScopeRefusal` and the socket is never
  upgraded, because an unscoped connection carries every workspace's frames.
- `/sessions/{id}/stream` — `ClientKindGUIStream`, the rendering webview
  addressed by one session.
- the frontend UDS — `ClientKindHost`, the Emacs tab bar.
- `/frontend` — `ClientKindGUIBootstrap`, the webapp's short-lived bootstrap
  socket, which creates a session and closes.

Dependencies: `proto/agentshim/` (generated Go), `daemon/internal/ssm/`.
