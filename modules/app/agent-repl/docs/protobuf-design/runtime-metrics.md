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

### 3. Prompt echo is not reported yet

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
