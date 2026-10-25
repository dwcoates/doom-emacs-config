# A hidden workspace's cache lapses before its session stands down

Owner request, 2026-10-13. Contracts were delegated in full ("as for contracts,
do them all yourselves").

## The behavior

- Hiding a workspace no longer stands its session down at once.
  - The daemon HOLDS its keep-alives (`shim.v1.SetKeepalive`), so the prompt
    cache lapses on its own clock.
  - Until that moment the user can show the workspace again with its cache
    still warm, and the daemon RUNS the cadence again.
- At the lapse the daemon stands the session down, exactly as the hide used to.
- The hidden section's when-column counts DOWN to the lapse ("12m 34s"), and
  reads "cold cache" once it is reached.
  - Its colour is a continuous gradient: green an hour or more out, the
    deepest red at the lapse.
- A hidden workspace whose cache lapsed above the cold gate's context floor
  wears `frontend.v1.RosterRowStatusColdGate`, so its name is struck through.
  - Showing it again opens it through the normal path, where `StartSession`
    raises the real gate.

## Landed contract

### `shim.v1`

- `rpc SetKeepalive(SetKeepaliveRequest) returns (SetKeepaliveResponse)`.
  - `SetKeepaliveRequest.cadence` is a oneof of `SetKeepaliveHold` and
    `SetKeepaliveRun`.
  - The outcome is a success arm, or `SetKeepaliveError.no_session`.
- `StartSessionRequest.hold_keepalive` (`optional StartSessionHoldKeepalive`).
  - A hidden workspace's session started by anything (a rollout's restart, a
    prompt) is BORN held, so no keep-alive can fire between its start and a
    later hold. The race is unrepresentable rather than unlikely.

### `conversation.v1`

- `SessionUpdate.cache_lapse = 36`, a `SessionCacheLapse`:
  - `lapses_at_ms`.
  - `after_lapse` oneof: `SessionCacheLapseGated{context_tokens}` or
    `SessionCacheLapseUngated`.
  - Stated on a hold, on every change while held, and on every
    `WatchSession` open while held. Never while the cadence runs.

### `frontend.v1`

- `RosterRowWhenHidden.cache` (`optional RosterRowWhenHiddenCache{lapses_at_ms}`).
  - Unset when the daemon knows no lapse, and the column then counts up from
    `at_ms` as before.
- `RosterRowStatusColdGate`'s comment names the hidden, lapsed, gated case.

## Decisions

- The SHIM owns the projection, and the DAEMON owns the clock.
  - The shim is the one owner of the cache anchor and the cold gate's floor,
    so the verdict is never computed twice.
  - The daemon persists the projection on the workspace record and arms one
    deadline per hidden workspace. The deadline survives a daemon restart and
    a shim that is no longer running, because the record outlives both.
- The verdict is projected, not observed: the gate's floor is a constant over
  the context size, which a held session cannot change.
- Collapsing a repository section still stands its workspaces down at once.
  The request covered hidden workspaces only. (Amended 2026-10-25: a fold
  spares a one-shot and a workspace with a merge pending; see open-tabs.md.)
- "Cold cache" needs no arm of its own: a lapse at or before now IS cold, and
  the daemon republishes the roster at the deadline.
- The workspace the user is looking at keeps its cache warm even hidden
  (owner ruling, 2026-10-17; `wsm.Workspace.KeepaliveHeld`).
  - Selecting it clears its recorded lapse, and the store refuses a
    projection for it, so no deadline can stand it down while it is shown.
  - Moving off it is a visibility edge: the keeper holds its keep-alives
    again, and the shim's fresh projection arms the deadline.

## Implementation notes

- Shim (`agent-shim/claude/shim/src/engine/keepalive.ts`).
  - `KeepaliveCadence.hold()` and `run()`; a held cadence projects the lapse
    on every hold and every anchor change.
  - The verdict is `engine/cold.ts`'s `sessionCacheLapse`, through the same
    `coldGateAsks` floor `judgeCold` applies.
  - The projection is a replayed arm of `SessionPushes`, withdrawn on `run`.
- Daemon.
  - `internal/cachelapse.Keeper` owns the cadence and the deadline in one loop.
  - `wsm` layout 48 stores the projection (`cache_lapses_at`,
    `cache_lapse_gated_tokens`); `SetCacheLapse` writes only a hidden row,
    and `SetHidden` clears it.
  - Every bring-up site sets `StartSessionRequest.hold_keepalive` from the
    record it read (`source.HoldKeepalive`).
  - The roster draws `cold_gate` for a lapsed, gated hidden row above the
    footer projection, as `inactive` is drawn, because no footer is drawn for
    a hidden workspace.
- Webapp: `src/sidebar/cache-lapse.ts` and the shared `src/heat.ts` gradient.
- Emacs: the strict roster decoder accepts `RosterRowWhenHidden.cache`.

## A closed workspace gets one last round, then the same lapse

Owner request, 2026-10-17: "closed workspaces should only keepalive in shim for
just one more keep alive round. after that, they should compaction gate just
like normal".

- Closing a workspace still leaves its session up, but no longer keeps its
  cache warm forever.
  - The daemon asks the shim for `shim.v1.SetKeepaliveLastRound`
    (`SetKeepaliveRequest.last_round = 3`).
  - The shim's cadence sends the ONE beat it already had scheduled from the
    conversation's last request, then holds exactly as `SetKeepaliveHold`
    does, stating the projected lapse (`conversation.v1.SessionCacheLapse`).
  - A cache no beat can keep warm (already lapsed, or a tier no longer than
    the interval) is held at once.
  - A spent round is never granted again: only `SetKeepaliveRun` restarts the
    keep-alives.
- From the hold on, the daemon treats the closed workspace as it treats a
  hidden one (`wsm.Workspace.LetsCacheLapse`).
  - The projection is recorded on its row, and the session is stood down when
    the lapse comes, so reopening it afterwards meets the cold gate.
  - Reopening it before the lapse runs the cadence again, and clears the
    recorded lapse in the same statement (`wsm.Reopen`).
  - A hidden workspace that is closed stays held: the hide wins, and grants no
    round.
- Every close and reopen reaches the keeper through the one state client every
  close path writes through (`cachelapse.ObserveClosures`), so no close path
  can skip the round.

## A workspace parked at its cold gate is already cold

Owner request, 2026-10-17: "if a workspace enters the HIDDEN section already
compacted, it shouldn't countdown to compaction gate ... if already compacted,
that is trivially 0".

- A session parked at its cold gate holds no cadence, so no shim ever states a
  projection for it.
  - Before this, a hidden parked workspace counted UP from the hide, and its
    row drew `severed` once the hide dropped its participants (seen on
    `metrics-o11y` and `agent-repl-test-governor`, 2026-10-17 20:11).
- The keeper now records the STANDING GATE'S OWN LAPSE on a hidden or closed
  row (`cachelapse.gateLapse`, from `workspace.Fleet.StandingColdGate`).
  - It is when the conversation went cold: a cache lifetime after the last
    request, or the last request itself for a model switch.
  - It is gated at the gate's context, so the row keeps `cold_gate` and the
    column reads "cold cache" at once.
  - It is taken on every reconcile (a hide, a close, a selection moving off)
    and on the gate being raised (`Keeper.ColdGateRaised`), so a gate raised
    while hidden is caught too.
- The lapse is due at once, so the parked session is stood down as any lapsed
  one is, and showing the workspace again raises the gate afresh.
- A gate an answer has taken is not standing: its remediation is bringing the
  session back warm, and the shim projects from the fresh anchor.
