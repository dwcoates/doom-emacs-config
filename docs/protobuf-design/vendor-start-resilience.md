# Vendor-start resilience and the immediate workspace restart

## Problem

On 2026-10-02 at 11:25 a deploy rollout relaunched the `doom` workspace's shim.
The relaunched shim's vendor (the Claude SDK child) did not answer its first
control request (`supportedModels`) within the shim's 3s liveness bound, so the
start failed once and was never retried. The workspace was left on a live shim
with no session: the footer read `disconnected · start failed`, a prompt the
user sent was drawn in the feed and then refused by the shim with `no_session`
(no held-prompt row was written, so it was silently lost), and `SPC o C-c`
answered "restart scheduled" — a graceful restart that waits for freeness, which
a never-started session can never reach (`sessionwatcher.freeLocked` requires
`started`). The same 3s failure hit other workspaces in the same rollout and in
the two previous rollouts.

The owner wants:

- the system to be very resilient to transient vendor/network failures — a
  failed vendor start is retried on a capped backoff for a long window, and
  only exhausting that window is a failure;
- the footer to say plainly which of three situations stands — retrying, a
  non-retryable vendor rejection, or retries exhausted — with the restart
  binding named in the activity line;
- a prompt sent while the session is not up to be HELD (reason "after
  reconnect"), never drawn-then-lost;
- `SPC o C-c` to restart the workspace's backend (shim) and webapp page
  IMMEDIATELY, always, hard-killing whatever turn and detached work stand in
  the way, with no graceful/forced split;
- one consolidated way to stop turns and detached work, and to detect that they
  are stopped, so every client (the webapp's expanded footer in particular)
  reliably shows killed work as dead.

## Owner rulings carried in from the triage conversation (2026-10-02)

1. Backoff is multiplicative ×1.5 from 200ms, capped at 5000ms, for a ten-minute
   window (200, 300, 450, 675, 1013, 1519, 2278, 3417, 5000, 5000, …).
2. The ten-minute window is wall time measured from the START of the most
   recent CONTIGUOUS sequence of failures: the first failure after a success
   (or after the session was first asked to start) opens the window, every
   retry inside it leaves the anchor where it is, and a successful start
   closes the sequence so the next failure opens a fresh window.
3. Only failures that can be retried are retried; a non-retryable failure
   surfaces as a DIFFERENT footer substatus ("vendor rejection" vs "vendor
   failed").
4. Footer substatus is "vendor retry" from the first retry until the last; the
   activity line carries the detected rejection/failure detail transiently.
5. After the ten-minute window the substatus is "vendor failed".
6. A restart is a clean stop that interrupts whatever is in the way first, but
   because the vendor is likely unreachable, turns and detached work will
   likely have to be hard-killed: kill the connection and let the shim
   hard-kill what is in flight before the rest of cleanup.
7. A running turn is interrupted with no gentleness — restarts happen because
   something is stuck.
8. All detached work is actively stopped, and how stopped work is surfaced to
   clients is consolidated so the webapp never keeps reporting killed detached
   work as live.
9. There is no graceful restart; force is the only mode, and `C-u` goes away.
10. A restart reloads the workspace's webapp page.
11. `SPC o C-c` is for bouncing the workspace-specific frontend/backend pieces
    (shim, webapp page) to unstick a workspace without rebooting everything.
    It is NOT an advised routine action: a fresh shim/webapp may speak an API
    the older running daemon does not.
12. The shim's 3s liveness bound stays as is; retries absorb slow starts.
13. Logging is added for what the vendor was doing when a start times out.
14. Footer wording: substatus "vendor failed"; activity "Claude SDK failed to
    start · restart: SPC o C-c".

## Scope (settled 2026-10-02)

- Objective: a vendor start that fails transiently is retried on a long capped
  backoff, the footer says plainly where it stands, prompts are never lost while
  the session is down, and `SPC o C-c` always restarts a stuck workspace at once.
- Frontend component: yes — the footer strip and the webapp's expanded footer,
  so figma→idl governs those views.
- Systems: shim, daemon, webapp, Emacs lisp; store and sidecar presumed
  untouched until the stop/liveness stage says otherwise.
- Non-additive changes known up front: the empty
  `StartSessionVendorStartFailed` arm is replaced by a cause that separates
  retryable from rejected; the `disconnected · start_failed` substatus gives way
  to vendor retry / vendor rejection / vendor failed; the drop-held-prompts-on-
  bring-up-failure semantics of `FooterStatusActivityStartFailed.dropped_prompts`
  and `HeldPromptSessionStartingHold` are reversed (prompts stay held);
  `RestartWorkspaceRequest.force` and the graceful mode are removed, and
  `RestartWorkspaceSuccess`'s "scheduled" wording with them.

## Iteration sequence (settled 2026-10-02, accepted as proposed)

1. Shim start-failure vocabulary (retryable vs rejection) and the owner of the
   retry loop.
2. Footer substatus arms (vendor retry / vendor rejection / vendor failed) and
   their activity lines.
3. The "after reconnect" held-prompt hold, replacing drop-on-failure.
4. RestartWorkspace as a single immediate mode.
5. Consolidated turn/detached-work stop and liveness surfacing to every client.

A later stage is entered only once the earlier ones are settled; any stage may
be reopened, which reopens every decision downstream of it.

## Core design principles

1. **The shim is the source of truth for whether a start failure can be
   retried.** (Owner, 2026-10-02: "shim should label — it's the expert/source
   of truth".) Consequence: the retry/rejection verdict rides the shim's
   StartSession failure; the daemon never re-derives it from `detail` text.
   Does NOT claim the shim runs the retry loop.
2. **The daemon owns loops on state; it is the orchestrator.** (Owner,
   2026-10-02.) Consequence: the backoff schedule, attempt count, ten-minute
   window, footer state and held prompts all live in the daemon. Does NOT
   claim the daemon decides retryability.

## Landed changes

### 1. Shim start-failure label (shim.v1, endpoint_start_session.proto)

- WHAT: `StartSessionVendorStartFailed` gains `oneof retry { retryable;
  rejected; }` (`StartSessionVendorStartRetryable`,
  `StartSessionVendorStartRejected`). The other StartSession failure arms
  state their fixed verdict in their comments: `unknown_session` and
  `conversation_owned` are not retryable, `lock_holder_unavailable` is
  retryable, `already_started` is a caller defect (attach, do not start), and
  `cold` is an answer, not a failure.
- WHY: owner rulings 3 and principle 1. A failed vendor start is retried only
  when retrying can help.
- Shim classification (owner accepted, 2026-10-02): liveness-bound timeout
  (`supportedModels` not answered in 3s), init-silence timeout (45s), and a
  process/stream that ended before ready are RETRYABLE; an opening error
  result is classified by its API status — auth rejection and model missing
  are REJECTED, overloaded/5xx/network are RETRYABLE, any other error result
  (including a refused resume) is REJECTED; a blocking hook is REJECTED; a
  failed cold remediation is labeled by its own underlying cause.
- Retrying happens on the SAME shim process (existing code keeps the
  identity across a refused start precisely so a retry re-announces it —
  `session.ts` around the vendor-start catch). Not yet observed end to end:
  see the vetting register.
- Consequences: daemon `askToStartSession` (internal/workspace/sessions.go)
  branches on the label; the shim's `failures.ts` StartSessionCause gains the
  label; every fake shim that answers `vendor_start_failed` must set it.

### 2. Vendor-start footer states (frontend.v1 footer.proto; agentrepl.v1 endpoint_session_health.proto)

- WHAT: `FooterStatusDisconnected.substatus` gains `vendor_retry` (7),
  `vendor_rejection` (8), `vendor_failed` (9). `start_failed` now means the
  shim PROCESS (or a non-vendor bring-up step) failed. The disconnected
  salient oneof gains `vendor_start` (8) =
  `FooterStatusActivityVendorStart { string text }`, composed by the daemon:
  retry "Claude SDK did not start (attempt N): <cause> · retrying",
  rejection "Claude SDK refused to start: <cause> · restart: SPC o C-c",
  failed "Claude SDK failed to start · restart: SPC o C-c".
  `SessionFault.kind` gains `vendor_start_retrying` (16,
  failed_attempts/cause/failing_since_ms), `vendor_start_rejected` (17,
  cause), `vendor_start_failed` (18, failed_attempts/last_cause/
  failing_since_ms).
- WHY: owner rulings 3–5 and 14.
- Daemon mechanism (orchestrator's judgement under "implement everything"):
  three new fault kinds `vendor_start_retrying` / `vendor_start_rejected` /
  `vendor_start_failed` in the health package, each partitioned to
  `disconnected` with the matching new substatus bucket; all three close at
  `EdgeSessionStarted`. The retrying fault is REPLACED per failed attempt
  (open the new one, then close the old) so it always carries the latest
  attempt; the run's anchor (`failing_since_ms`) is held by the fleet in
  memory and copied into each fault's evidence. The single place is
  `Fleet.startSession` (internal/workspace/sessions.go), which every bring-up
  — cold start, rollout relaunch `Resume`, cold-gate re-open — already
  passes through. Backoff ×1.5 from 200ms capped at 5s; window 10 min of
  wall time from the run's first failure; a successful start or a restart
  clears the anchor.
- Roster (sidebar) keeps its existing arms: no roster arm was added. The
  retrying state draws as the roster's existing bring-up state, and
  rejection/exhaustion as `start_failed` (judgement: the roster is a color
  ladder, and the footer carries the distinction).
- `FooterStatusActivityStartFailed.dropped_prompts` (tag 2) is RETIRED with
  the drop-on-failure semantics (see 3).
- Consequences: webapp `footer/activity.ts` gains the `vendorStart` case;
  the webapp strip's substatus words derive from arm names and need no
  table; elisp decodes the three new SessionFault arms wherever it switches
  on fault arms.

### 3. The reconnect hold (frontend.v1 daemon_hold.proto)

- WHAT: `HeldPromptSessionStartingHold session_starting = 11` is RENAMED
  `HeldPromptReconnectHold reconnect = 11` (same tag) and its semantics
  change: a failed bring-up NEVER drops these entries; they are delivered
  when a session next comes up on the workspace, however it comes up. The
  card badge reads "after reconnect".
- WHY: the 2026-10-02 incident drew a prompt in the feed and then lost it to
  a `no_session` refusal; the owner wants such prompts held with reason
  "after reconnect".
- Daemon consequences: (a) `dropRevivalHolds` and `Footer.AddDroppedPrompts`
  go away; (b) a submission that finds a shim whose session is NOT started
  (fleet `sessionStarted` false) is held under the reconnect hold instead of
  delivered; (c) a delivery the shim refuses with `no_session` is converted
  into a reconnect hold instead of retired; (d) every place a session comes
  up (`sessionUp`, rollout `Resume`) releases reconnect holds and delivers.
- The name was changed because "session starting" stops being true once a
  start has failed and the prompt waits on a restart.

### 4. RestartWorkspace has one mode (agentrepl.v1 endpoint_restart_workspace.proto)

- WHAT: `RestartWorkspaceRequest.force` (tag 2) and
  `RestartWorkspaceError.no_session` (tag 5) are RETIRED. A restart is
  always immediate: interrupt the running turn and stop all detached work
  with BOUNDED calls, then bounce the shim (whose forced teardown hard-kills
  whatever did not stop), resume the same session with a fresh vendor-start
  run, release reconnect holds, reload the workspace's webapp page. A
  workspace with no session is restarted by bringing one up.
- WHY: owner rulings 6–11. "Restart scheduled" on a stuck workspace is
  nonsensical; the 2026-10-02 incident deadlocked because a graceful bounce
  waited on freeness that `sessionwatcher.freeLocked` can never grant when
  `started` is false.
- Consequences: `internal/workspace/restart.go` always forces and no longer
  aborts when the force-end call fails (the failure is logged at ERROR and
  the bounce proceeds — the bounce is the remedy); a vendor-start retry loop
  in flight for the workspace is cancelled first; elisp drops the `C-u`
  prefix and the "(C-u = force)" description; user and agent docs describe
  what `SPC o C-c` is for.

### 5. Stop and liveness consolidation

- Not yet landed as a contract change. Dispatched as an investigation-and-
  implementation item: one shared path for stopping turns and detached work,
  one source of truth for whether detached work is alive, and the webapp's
  expanded footer never listing work that a kill or a shim death ended. Any
  proto change it needs comes back to the orchestrator as a question.

