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
  The request covered hidden workspaces only.
- "Cold cache" needs no arm of its own: a lapse at or before now IS cold, and
  the daemon republishes the roster at the deadline.

## Implementation notes

- Shim (`agent-shim/claude/shim/src/engine/keepalive.ts`).
  - `KeepaliveCadence.hold()` and `run()`; a held cadence projects the lapse
    on every hold and every anchor change.
  - The verdict is `engine/cold.ts`'s `sessionCacheLapse`, through the same
    `coldGateAsks` floor `judgeCold` applies.
  - The projection is a replayed arm of `SessionPushes`, withdrawn on `run`.
- Daemon.
  - `internal/cachelapse.Keeper` owns the cadence and the deadline in one loop.
  - `wsm` layout 46 stores the projection (`cache_lapses_at`,
    `cache_lapse_gated_tokens`); `SetCacheLapse` writes only a hidden row,
    and `SetHidden` clears it.
  - Every bring-up site sets `StartSessionRequest.hold_keepalive` from the
    record it read (`source.HoldKeepalive`).
  - The roster draws `cold_gate` for a lapsed, gated hidden row above the
    footer projection, as `inactive` is drawn, because no footer is drawn for
    a hidden workspace.
- Webapp: `src/sidebar/cache-lapse.ts` and the shared `src/heat.ts` gradient.
- Emacs: the strict roster decoder accepts `RosterRowWhenHidden.cache`.
