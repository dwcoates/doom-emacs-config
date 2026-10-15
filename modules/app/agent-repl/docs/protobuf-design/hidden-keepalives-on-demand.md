# Keep every hidden workspace's cache warm on demand

Owner request, 2026-10-14: a command bound to `SPC j R` sends a keep-alive to
every hidden workspace, so each one's cache lapse (and the hidden section's
countdown to it) moves later. Contracts are the owner's standing delegation
(decide and carry proto changes through, report after).

## Premise, verified

- A hidden workspace's session is kept running until its prompt cache lapses
  (`docs/protobuf-design/hidden-cache-lapse.md`,
  `daemon/internal/cachelapse/keeper.go`), with its keep-alives HELD.
  - The owner called the lapse "the compaction gate"; the code calls it the
    cold gate. Same moment: the lapse is where the cold gate would ask.
- A held shim restates its projected lapse on every anchor change
  (`agent-shim/claude/shim/src/engine/keepalive.ts`, `KeepaliveCadence.arm`),
  and every main-thread API response re-anchors (`noteCacheRequest`), a
  keep-alive's included. So one keep-alive is enough to move the countdown:
  no new projection path is needed.

## Landed changes

### `shim.v1.SendKeepalive`

- WHAT: a new rpc that submits one ordinary keep-alive turn now and leaves
  the cadence (held or running) untouched.
  - Outcome: success with `sent` or `skipped{main_agent_busy |
    detached_work_live | cache_lapsed}`, or error `no_session` or
    `submit_failed{detail}`.
- WHY: the hidden workspace's cadence is held on purpose; the user asking for
  one more keep-alive must not un-hold it.
- Decisions.
  - A new rpc rather than a third `SetKeepaliveRequest.cadence` arm: a beat
    is an action, not a cadence, and the cadence is left as it was.
  - The skip reasons are exactly the ones the scheduled beat already honors,
    so an asked-for keep-alive can never do what a scheduled one may not.
  - The 5-minute-tier rule of the cadence (no beat when the tier is no longer
    than the 52-minute interval) does NOT apply: one beat into a still-warm
    cache keeps it warm whatever its tier. Only a lapsed cache is refused.
  - A submission that fails is ANSWERED (`submit_failed`), never thrown, so
    the daemon maps it like any outcome; the shim also raises its standing
    `keepalive_failed` fault exactly as a failed scheduled beat does.
  - `no_session` also covers a session whose vendor query has ended: nothing
    can carry a keep-alive, and the cadence has stopped.
  - The answer carries no lapse. The new lapse arrives as
    `conversation.v1.SessionCacheLapse` on the session stream once the
    keep-alive's response re-anchors the cache, through the one existing path.

### `agentrepl.v1.SendHiddenKeepalives`

- WHAT: a daemon-scoped rpc (SIDEBAR section) with an empty request.
  - The daemon reads the hidden section (`wsm.Workspace.InHiddenSection`),
    asks each served shim concurrently, and answers one
    `SendHiddenKeepalivesWorkspace` per hidden workspace: `sent`, `skipped`,
    `not_running`, or `failed{detail}`.
  - Error arm: `registry_unreadable`, when the registry list itself fails.
- WHY: `SPC j R` reports how it went in the echo area; the countdowns
  themselves move on the roster stream.
- Decisions.
  - `not_running` is an answer, not a failure: a hidden workspace whose lapse
    already came has no session and no cache to keep. A shim answering
    `no_session` maps to it too.
  - The skip vocabulary is re-spelled in `agentrepl.v1` rather than importing
    `shim.v1`, because `shim.v1` is the daemon↔shim boundary only
    (`proto/AGENTS.md`). The daemon owns the one mapping
    (`daemon/internal/cachelapse`), with a test that fails when a shim skip
    reason is unmapped.

## Consequences

- Shim: `KeepaliveCadence` gains a one-shot judged beat; `keepaliveBeat`
  reports why it did not send, so the rpc can answer.
- Daemon: `cachelapse.Keepalives` gains `Beat`; the Keeper gains the fleet
  fan-out; the server gains the handler and its validation and request log.
- Emacs: a command on `SPC j R`, its wire verb, and the fake daemon's handler.
  - Owner ruling, 2026-10-14: `SPC j R` was `agent-repl-reload-config`; reload
    moved to `SPC j C-r` so the keep-alive takes the key asked for.
