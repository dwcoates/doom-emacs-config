# Network-fault retry latch

During a machine-wide DNS outage (2026-11-08, 14:00 to 14:16), the shim's
vendor-error retry of an `unreachable` failure fired every 30 seconds on a
timer while the network fault stood. Each retry opened an adopted turn that
failed again, so every workspace flipped between a running turn and its
network fault, read as stuck in a running turn, and held the user's prompts
behind an adopted turn they could never join. The retry of a failure that
never reached the API must instead wait on the network-reachability probe: a
latch, not a timer.

Design mode: autonomous (the user handed the change over and pre-approved
contract changes).

## Assumptions (step 1, autonomous)

- Objective: a workspace sits cleanly in its network fault until the network
  returns, then retries once.
- Built: a third way a pending retry is stated (waiting for the network), and
  the footer line's matching clause.
- Frontend: yes, the footer's retrying line (`webapp/src/footer/activity.ts`).
- Systems: shim (producer of the retry statement), daemon footer resolver
  (consumer, producer of the footer line), webapp (renderer).
- Non-additive: both retry messages had their exclusive states spelled as
  sibling fields (`scheduled` beside `attempting`; `next_attempt` beside a
  `now` bool). Both become oneofs.

## Iteration sequence

One stage per message: the session statement first (it is what the footer
line is resolved from), then the footer line.

## Landed changes

### 1. `SessionVendorErrorRetry` states its pending retry as a oneof, with a network arm

- WHAT: the two `optional` siblings `scheduled` and `attempting` move into
  `oneof stated` (same tags, wire compatible), and a third arm
  `awaiting_network` (`SessionVendorErrorRetryAwaitingNetwork`, attempt and
  status, no instant) is added.
- WHY: the retry of an unreachable failure has no instant to name; stating a
  fake instant is what drew a countdown to a timer that cannot cure the fault.
  The arms were always exclusive ("at most one is set"), and that was enforced
  only by a comment.
- CONSEQUENCES:
  - The shim's `vendorErrorRetryUpdate` builds the oneof
    (`agent-shim/claude/shim/src/engine/vendor-error-retry.ts`).
  - The daemon's Go getters (`GetScheduled`, `GetAttempting`) are unchanged
    by the oneof; `observeVendorRetry`
    (`daemon/internal/resolve/footer/vendorretry.go`) gains the network arm.
  - Producer obligation: the network arm is stated only while the session's
    network fault stands, so the footer draws the network fault's line.

### 2. `FooterStatusActivityRetrying` draws its next-attempt clause from a oneof

- WHAT: `next_attempt` (tag 3) and the bool `now` (tag 5) become
  `oneof next { next_attempt = 3; retrying_now = 6; awaiting_network = 7; }`;
  tag 5 and the name `now` are reserved.
- WHY: the `now` bool selected how the line's clause is drawn beside the data
  it governed (the countdown), the shape the design principles forbid; a third
  clause ("waits for the network") made a second bool the only alternative.
- CONSEQUENCES:
  - `retryState.line()` (`daemon/internal/resolve/footer/resolver.go`) sets
    the arm; `drawFooterStatusActivityRetrying`
    (`webapp/src/footer/activity.ts`) switches on it.
  - The renamed `now` changes wire type, accepted: daemon and webapp deploy
    together.

## Obviated-declaration sweep

Nothing obviated: every touched message is still referenced.
