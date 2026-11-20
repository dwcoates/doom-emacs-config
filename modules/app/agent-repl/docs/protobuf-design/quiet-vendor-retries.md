# Quiet vendor-error retries

A main turn that fails on a failure agent-repl retries itself (the shim's
vendor-error retry: a retryable API failure on a timer, or an `unreachable`
failure waiting on the network) drew a red turn-error row in the feed while the
retry waited. The owner ruled (2026-11-15) that a failure the shim will retry
draws no error row: the footer says the retry is pending, a retry that succeeds
leaves no trace of the failure, and the row is drawn only when the retry chain
ends without success.

Designed autonomously (the owner pre-approved the proto changes).

## Assumptions

- The feed resolver draws a turn's failure from its terminal
  (`conversation.v1.AgentFailure`), which reaches the daemon on the agent
  stream and, on replay, from the store.
- The retry's schedule (`conversation.v1.SessionVendorErrorRetry`) reaches the
  daemon on the session stream. Verified in
  `daemon/internal/sessionwatcher/watcher.go` (`runSession` and `runAgent` are
  separate gRPC streams): there is no ordering between the two.
- The shim decides whether to retry synchronously, from the terminal alone, in
  `agent-shim/claude/shim/src/engine/vendor-error-retry.ts`
  `VendorErrorRetry.turnEnded`, before anything else happens to the session.
- The automatic retry's own prompt already draws no bubble: the shim writes its
  turn's prompt row as `PROMPT_ORIGIN_VENDOR_STARTED` with nothing said, which
  `daemon/internal/resolve/feed/prompt.go` suppresses, and the sidecar
  withholds every `sdk-cli` prompt (`shim-sidecar/internal/convert/user.go`).
  Checked against the store and logs for both the evidence workspace
  (`sidebar-fold-workspace-selection`, 13:37:20 UTC) and `quiet-vendor-retries`
  (13:41:31 UTC): no store entry carries the retry's words, and the only
  prompt row the webapp drew after each retry was the conversation's earlier
  prompt, redrawn on a page boot. Part 2 therefore needed no contract change.

## Landed changes

### `conversation.v1.AgentFailure.retry_promised`

- WHAT: an optional `AgentFailureRetryPromised { attempt }` on the failure
  terminal, set by the shim when its retry chain decides at that terminal that
  a retry follows.
- WHY: the feed must know, the moment it sees a failed terminal, whether the
  failure is retried. Reading it off the session stream's schedule is a race
  (the two streams are unordered), so the row would be drawn and then taken
  back; and a replayed turn has only its terminal. Stating it on the terminal
  makes the flash structurally impossible and makes replay correct.
- Alternative rejected: the feed withholding a failure until the session
  stream's schedule names its turn. Unordered streams make that "very
  improbable" at best, and after a daemon restart the schedule restated on
  open cannot say whether a chain that already ended had been promised.
- CONSEQUENCES: the shim must decide before it writes the terminal entry
  (`engine/session.ts` called `vendorErrorRetry.turnEnded` after
  `deps.persistence.write`; the call moves ahead of the write). A chain that
  gives up at a terminal (its two-hour window, the usage wait's tail, a failure
  never retried) sets no promise, so that terminal's failure draws as before.
  The earlier promised failures of a chain that later gives up stay hidden:
  the last failure stands for the chain.

### `conversation.v1.SessionVendorErrorRetryAbandoned` (arm `abandoned` of `SessionVendorErrorRetry.stated`)

- WHAT: a statement of the retry level naming the failed turn whose promised
  retry was never made, and why.
- WHY: a chain can end after its failed turn's terminal promised a retry: the
  retry prompt is refused or cannot be delivered, or the shim stands down.
  Without a statement the failure would stay hidden while nothing retries it.
- A prompt the user submits during the wait ends the chain without
  abandonment: the user took the session over, and their prompt either
  continues the work or fails on its own terms.
- CONSEQUENCES: the footer treats `abandoned` as no retry pending. The feed
  draws the named turn's withheld failure. Being a level arm, it is restated
  on every WatchSession open, so a daemon restart still draws it; a shim
  replaced after abandoning does not restate it (accepted: the chain's shim is
  busy while a chain stands, so a replacement waits for it).

### `frontend.v1.FooterStatusActivityNetworkOffline.retry`

- WHAT: an optional `FooterStatusActivityNetworkOfflineRetry { attempt }` on
  the offline line, set while a retry waits for the network.
- WHY: under a network fault the footer draws the network fault's line, which
  outranks the retrying line; with the feed no longer drawing the failure, the
  offline line is where the pending retry is accounted for ("offline ·
  retrying when the network returns"). A timed retry already draws the
  retrying line's countdown.

## Obviated-declaration sweep

Nothing obviated: every change is additive.
