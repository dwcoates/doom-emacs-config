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
2. The ten-minute window is wall time measured from "the most recent failure"
   — exact anchor under clarification (see open questions).
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

## Core design principles

(none recorded yet)

## Landed changes

(none yet)
