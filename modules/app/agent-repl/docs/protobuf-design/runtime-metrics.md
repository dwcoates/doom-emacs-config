# Runtime metrics reporting contract

The always-on metrics (`docs/PLAN-runtime-metrics.md`) need measurements that
only Emacs, the webapp and the shim can take: how long a workspace switch
took until the page showed it, how long a page took to boot, how long the
vendor took to answer, how many bytes went to the vendor. VictoriaMetrics
scrapes only the daemon, the store and the sidecar, so these runtimes report
to the daemon, which exposes the series. This record is the contract for
that reporting.

The owner delegated every design decision in this change on 2026-10-18 ("you
do all the proto design yourself without my input"), so the selections this
process normally puts to the owner were made by the orchestrator and are
recorded here with their reasoning.

## Context

- Objective: every runtime's performance is measured all the time, so an
  agent can find the version where a latency regressed from stored history.
- What is built: a reporting path from Emacs, the webapp and the shim to the
  daemon's metrics surface.
- Frontend component: none. Nothing here is drawn; the webapp and Emacs only
  report. figma→idl does not apply.
- Systems: daemon (consumer), webapp and Emacs (producers over
  `agentrepl.v1`), shim (producer over `shim.v1`).
- Non-additive changes: none. Both changes are additions (a new rpc, a new
  stream arm); no existing field changes meaning.

## Iteration sequence

Transport first (which existing channel each producer uses), then the
client rpc, then the shim arm. Each was decided by the orchestrator under the
owner's delegation.

## Core design principles

### The runtime that sees an interaction end measures it

- Principle: a latency is computed by ONE runtime, the one that observes the
  interaction's end, from a start instant handed to it through the channel
  that already carries the interaction. The daemon never pairs a start
  reported by one runtime with an end reported by another.
- Consequences: every report carries a finished duration, never an edge.
  Nothing in the contract names a start or end instant.
- Why: pairing edges in the daemon needs either a time window (two
  switches inside it pair wrongly, a dropped edge pairs with the next one) or
  a correlation id threaded through every channel the interaction crosses. A
  finished duration from the runtime that saw the end makes a wrong pair
  unrepresentable. All runtimes share the host's wall clock, so a start
  instant carried from Emacs into the page is directly comparable.
- Does not claim: that every latency in `bin/profile-report.go` is reported.
  A latency whose start cannot reach the runtime that sees its end through an
  existing channel is not reported until one does (see prompt echo below).

### Nothing measured is lost to a broken connection

- Principle: a measurement survives a dropped stream, a failed rpc, and a
  daemon handover.
- Consequences:
  - The shim reports CUMULATIVE counters (bytes, histogram buckets) since a
    stated instant, so a lost frame is subsumed by the next one, and a
    restarted shim is told apart by that instant rather than by a counter
    going down.
  - A client keeps a batch until the daemon acknowledges it; the rpc's
    success arm is the acknowledgment.

## Landed changes

### 1. `agentrepl.v1.AgentRepl.ReportLatencies`

- What: a unary rpc in the DIAGNOSTICS suite beside `ClientLog`, declared in
  `endpoint_report_latencies.proto`. A client sends a batch of finished
  latencies, each a kind, the workspace it happened in, and its duration in
  seconds.
- Kinds: `switch_repeat`, `switch_first`, `page_boot`, the client-measured
  entries of `proto/vocab/latencies.json`. The enum is a closed set of
  interaction names, not a state, and no sibling field means anything for
  only some kinds, so it stands as an enum.
- Outcome: success holds a `oneof`:
  - `recorded`: every latency was recorded;
  - `partly_recorded`: some named a workspace the daemon no longer knows (a
    workspace closed between the interaction and the report), and the arm
    states how many were dropped rather than a total;
  - `metrics_off`: the daemon runs without a metrics surface and recorded
    nothing. A client keeps reporting; there is nothing to act on.
  - These are answers to a valid report, so none is an error.
- Error: `workspace_ref_mismatch`, a ref whose dir disagrees with the
  registry's for its id, the same client defect `ReportPageDrawn` refuses.
  Validation (an unspecified kind, a negative or non-finite duration, an
  unset ref) is Connect InvalidArgument, as everywhere in this package.
- Why a dropped workspace is not an error: closing a workspace while its
  report is in flight is an ordinary schedule. Recording it would recreate a
  series the daemon deleted when the workspace closed.
- Consequences:
  - The daemon owns the client latency histograms, labeled by `kind` and
    `workspace`, and deletes a workspace's series when the workspace leaves
    its registry.
  - Emacs needs an encoder for the new rpc in its Connect JSON layer
    (`lisp/rpc.el`), and the webapp a call beside `ClientLog`.

### 2. `shim.v1.WatchSessionResponse.frame.measurements`

- What: arm 3 of the session stream's frame, `ShimMeasurements`, declared in
  `endpoint_watch_session.proto` because no other endpoint uses it.
  - `vendor_traffic`: bytes sent to and received from the vendor API since
    `counted_since_unix_ms`, the instant this shim process started counting.
  - `latencies`: one cumulative histogram per shim-measured kind
    (`first_output`, prompt forwarded to the first response text), each with
    its bucket upper bounds stated, its cumulative bucket counts, its count
    and its sum, all since the same instant.
- Why cumulative: see the second principle. A delta frame lost on a
  reconnect would be lost for good.
- Why the buckets are stated in every frame: the daemon checks them against
  `proto/vocab/latencies.json` and refuses a frame whose buckets disagree,
  loudly, so a shim built against another vocabulary can never be summed into
  the wrong buckets.
- Cadence (producer obligation, stated at the arm): sent once right after
  every new watch attaches (a daemon that adopted the shim learns its totals
  at once), then whenever anything changed, at most once per 15 seconds.
- Consequences:
  - The daemon folds each shim's cumulative values into per-workspace
    monotonic counters, the way `procmetrics` folds process counters: a new
    `counted_since_unix_ms` is a new counting run whose values are added
    whole.
  - The shim counts vendor bytes in memory at its HTTP layer, never through
    kernel network-statistics sockets
    (`docs/protobuf-design/drop-vendor-traffic.md`).

### 3. Prompt echo is not reported yet (superseded by entry 8)

- What: `prompt_echo` (Emacs submit to the webapp drawing the prompt) has no
  kind in either enum.
- Why: its start is in Emacs and its end in the webapp, and the only channel
  joining them is the feed's user-prompt row (`frontend.v1`). Carrying the
  submit instant there is a change to a drawn component's props, which is a
  figma→idl decision about the feed, not part of this reporting contract.
  Pairing in the daemon by prompt id would break the first principle.
- What would close it: a feed-row element carrying the prompt's submit
  instant, after which the webapp measures and reports a `prompt_echo` kind.

### 4. Vendor traffic is optional, and no shim sets it yet

- What: `ShimMeasurements.vendor_traffic` is `optional`; unset means the
  shim could not count the bytes, and the daemon then publishes no traffic
  series for that workspace instead of a zero.
- Why: verified in `agent-shim/claude/shim/src/sdk/real-query.ts` (code
  tier): the shim drives the agent SDK, which spawns its own bundled `claude`
  binary (`spawnClaudeCodeProcess`), and that child makes every vendor HTTP
  request. The shim's Node process never carries the traffic, so it has
  nothing to count in memory. Recording a zero would be a sentinel saying
  "no traffic" when the truth is "not measured".
- What would set it: either a byte-counting proxy in the shim that the child
  is pointed at (`HTTPS_PROXY`), which puts a new mechanism on the vendor
  path of every session, or per-process kernel network statistics, which are
  ruled out by `docs/protobuf-design/drop-vendor-traffic.md`. Choosing the
  proxy is the owner's call (`docs/METRICS-JUDGEMENT-CALLS.md`, call 6).
- Correction kept visible: entry 2 above stated the shim counts vendor bytes
  "in memory at its HTTP layer". That rested on a name-tier reading of
  "shim drives the vendor" and was wrong; the code shows the child process
  owns the HTTP.

### 5. The first measurements frame comes at the first tick, not at the open

- What: the measurements arm is sent on a 15-second tick that starts when a
  watch opens, whenever the figures differ from the last frame that watch
  carried. A new watch's first tick sends whatever is already counted. It is
  never sent among the opening frames.
- Why: the opening frames (diagnostics first, then the re-announcement, a
  standing death, the last rollback's settle) are the readiness protocol, and
  the shim's session tests pin their exact order. A measurements frame among
  them changed that order for no gain; an adopting daemon learning the totals
  within 15 seconds instead of at once costs nothing measurable.
- Correction kept visible: entry 2 stated "sent once right after every new
  watch attaches". That was replaced by this entry before any consumer
  depended on it.

### 6. Emacs reports nothing yet; switches carry their instant to the page

- What: no Emacs encoder for `ReportLatencies` exists. A switch's start
  instant travels from Emacs to the page through the webhost's `show` call
  and the host protocol's `presence` message (`switchStartedAtMs`, absent
  unless the show was a user's switch), and the page reports
  `switch_repeat` / `switch_first`.
- Why: the first principle (the runtime that sees the end measures). The
  switch ends in the page, and the webhost's `show` is the channel that
  already carries the switch from Emacs to the page.
- Correction kept visible: entry 1's consequences named an Emacs encoder
  in `lisp/rpc.el`; none is needed until Emacs measures a latency itself
  (`docs/METRICS-JUDGEMENT-CALLS.md`, call 8).

### 7. The shim measures its own event loop and heap

- What: `shim.v1.ShimLatencyKind` gains `event_loop_delay`, carried in the
  existing cumulative latency histograms under the vocabulary entry
  `shim_event_loop_delay`; `shim.v1.ShimMeasurements` gains an optional
  `ShimHeap` (used, total and external bytes), a snapshot per frame.
- Why: the owner asked for heap-size snapshots and event-loop delay for the
  shim (2026-10-20); the daemon's process collector already reads each
  shim's CPU and resident memory from the kernel, but a shim that feels
  stuck is usually a busy event loop, which only the shim can see.
- Why event-loop delay is a latency kind and not a new field: it is a
  duration with a distribution (how late a steady timer ran), which is
  exactly what the cumulative histogram arm carries, so the daemon's
  validation, folding and series need nothing new. The shim samples it with
  its own steady timer rather than Node's `monitorEventLoopDelay`, whose
  histogram exposes percentiles but not per-bucket counts.
- Why the heap is a snapshot and not cumulative: heap size is a level, not a
  count, so the daemon publishes it as a gauge per workspace and drops it
  when the workspace leaves the registry or its shim's counting run changes.
- Decided under the owner's delegation; additive, so no consumer breaks.
- Consequences: the daemon's `ShimKinds` table gains the kind (its vocabulary
  test pins the buckets), and the daemon exposes `agentrepl_shim_heap_bytes`
  by workspace and kind (`used`, `total`, `external`).

### 8. Prompt echo rides the web stream as an event, keyed by turn

- What:
  - `agentrepl.v1.SubmitPromptRequest` gains `optional int64
    submitted_at_unix_ms`, which Emacs sets on a person's send and leaves
    unset on a retry.
  - `agentrepl.v1.WatchWebWorkspaceResponse` gains the push arm
    `prompt_echo = 4`, carrying `agentrepl.v1.WebWorkspacePromptEcho` (the
    minted `conversation.v1.TurnId` and the submit instant).
  - `agentrepl.v1.ReportedLatencyKind` gains `PROMPT_ECHO`, and
    `prompt_echo` in `proto/vocab/latencies.json` is now `measured_by:
    webapp` with navigation buckets.
- Why an event and not an element on the feed row (entry 3's proposal): the
  accepted prompt's row is upserted into feed state and replayed to every
  page that opens later, so a stamp on the row would be timed again by a
  reload against a stale instant. The echo is an event on `publish.Events`,
  never replayed: a page that did not see the prompt land times nothing.
- Why keyed by turn and not by row id: the row's id is minted by the feed
  resolver from the turn's placement, while every user-prompt row already
  carries `frontend.v1.FeedRow.turn`. The daemon answers the turn it minted,
  and the page matches it against the row without a second identity.
- Why only a prompt drawn at submit: a delivered prompt and one folded into
  the running turn are drawn by the acceptance mirror before `SubmitPrompt`
  answers; a held prompt draws when it is delivered later, so its time to
  draw is the hold, not the echo.
- The page pairs the push and its row's first draw in either order, because
  they travel different streams; both waits are bounded.
- Decided under the owner's delegation; additive, so no consumer breaks.

### 9. Emacs reports its startup milestones in the latency batch

- What:
  - `agentrepl.v1.ReportLatenciesRequest` gains `repeated ReportedEditorStartup editor_startup = 2`.
  - `agentrepl.v1.ReportedEditorStartup` carries a milestone (`EditorStartupMilestone`: init, daemon linked, first tab, all tabs), how the editor came to a daemon (a `oneof` of spawned and running), and the seconds since the editor process started.
  - The daemon records each milestone as the `editor_start` restart: the milestone is the phase and the daemon arm is the cause.
- Why: the owner asked for solid visibility into every area of startup (2026-10-20), and Emacs's own launch, from its process start until every tab is open, had no metric (`docs/METRICS-JUDGEMENT-CALLS.md`, call 9 recorded it as not timed).
- Why milestones from the process start and not step durations: the editor's startup is one timeline from launch, and the user's question is "how long until I can work", which is the cumulative time. The daemon's boot phases take the same shape for the same reason.
- Why the milestone is an enum and the daemon is a `oneof`: the milestone is a closed set of moments that no sibling field depends on, while how the editor came to a daemon is a condition of the startup, which this project's design principles model as a `oneof`.
- Why it rides `ReportLatencies` rather than a new RPC: the batch already has the acknowledgment and resend-with-the-next-batch semantics a report needs to survive a daemon handover mid-startup, and a second endpoint would be a second copy of that protocol.
- Consequences:
  - `ReportLatenciesRecorded.recorded` keeps counting latencies only, since a milestone names no workspace and is never dropped.
  - The daemon's `server.LatencyRecorder` gains the restart recorder, and Emacs gains its first `ReportLatencies` client (call 8 said Emacs reports nothing yet; that call is superseded for the startup).
- Decided under the owner's delegation; additive, so no consumer breaks.

### 10. Emacs states its pid on its WatchDaemon stream

- What: `agentrepl.v1.WatchDaemonEmacs` gains `optional EditorProcess process = 4`, carrying the pid of the Emacs that holds the stream.
- Why: the process collector found Emacs as the macOS responsible process of the daemon itself, which holds only while the daemon is one this Emacs spawned. A daemon outlives Emacs (deploys hand it to successors, and Emacs restarts link to the running one), so from the first Emacs restart on the `emacs` and `webkit_*` groups reported nothing (observed 2026-10-20: silent from 14:59 UTC, the Emacs relaunched at 15:36 UTC).
- Why on the stream: the daemon already scopes Emacs's facts to its WatchDaemon stream (its focus), so the measured Emacs is the one connected, released when the stream ends, and a reconnect supersedes it (`streamslot.Slot`, shared with the focus).
- Why optional and not refused when absent: the field is metrics only, and refusing a stream from an Emacs whose elisp predates it would shut that Emacs out of the daemon that must push it the reload.
- Consequences: the daemon's process collector reads the pid from the server's stream-scoped holder instead of its own ancestry, and Emacs sends `(emacs-pid)`.
- Decided under the owner's delegation; additive.
