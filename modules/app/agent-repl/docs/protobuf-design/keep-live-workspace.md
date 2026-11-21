# KeepLiveWorkspace: an agent verb that warms one workspace through the keep-alive subroutine

On 2026-11-20 an agent switched 37 parked workspaces to sonnet with SetModel;
each call started the workspace's shim and nothing stood those shims down for
hours (about 390MB each, 24 idle at once). The owner asked for an agent API
verb, `agent-repld workspace keep-live WS`, that touches a workspace's session
through the daemon's normal keep-alive subroutine and always ends with the
shim down again: start it if parked and not cold-gated, send the keep-alive,
wait for the vendor's ack, stand it down.

Design ran autonomously (no co-design was asked for).

## Assumptions (autonomous scoping)

- Objective: one bounded, safe verb an agent can run in bulk that leaves no
  shim up that the normal rules would not keep up.
- Systems: daemon only (proto, server, CLI, cache keeper, drain); no frontend.
- Non-additive changes: `cachelapse.Deps.CycleDown` gains a completion
  callback; the drain gains a reporting variant of `CycleDown`. No proto
  removal.
- Iteration sequence: one rpc, one endpoint file, canonical
  `oneof result { success; error; }` (the package's spelling).

## Landed changes

### `agentrepl.v1.KeepLiveWorkspace` (endpoint_keep_live_workspace.proto)

- WHAT: a new AGENT API rpc. Request names one `workspace.v1.WorkspaceRef`.
  Success is `beaten` (how the session came up: `started` or `already_up`;
  the vendor `ack`: `answered` or `request_failed`; where it ended:
  `stood_down` or `left_up{rule}`) or `busy` (`main_agent` or
  `detached_work`). Errors: the four resolution arms every workspace verb
  carries, `closed`, `cold_gated`, `owes_no_keepalive{reason}`,
  `cache_lapsed{lapsed_at_ms}`, `start_failed`, `beat_failed`,
  `stand_down_failed`, `timed_out{step, bound_ms, expected_ms}`,
  `registry_unreadable`.
- WHY: the owner's rules: cold gate refused (never paid), busy answered with
  nothing sent or stopped, already-up beaten and left to the ordinary rules,
  closed and unknown typed, every wait bounded with a typed timeout.
- `busy` is a SUCCESS arm: the cache is warm, which is what the caller asked
  for.
- `owes_no_keepalive` exists because the verb runs the keeper's own beat plan
  (`beatPlanFor`): a hidden workspace, a known-cold cache, no session, a run
  at break-even, or idle past the cutoff owes no keep-alive, and the verb
  starts nothing for it. Hidden is answered this way rather than with an arm
  of its own, because hiding is only one of the plan's reasons.
- `timed_out.step` is a oneof of empty arms (no state enum); the bound and its
  measured expected time are siblings because they apply to every step.
- Consequences accepted: the verb holds the request open for the whole run
  (start, ack, stand-down), bounded by the per-step bounds; a bulk caller
  runs it per workspace, and the keeper's restore slots (three at once) pace
  the starts.

## Measured bounds (2026-11-20)

- Start (restore through the beat going out): `agentrepl_shim_start_seconds`
  with `cause="keepalive"`, `outcome="ok"`: p99 7.8s over 1,990 starts in 7
  days, p99.9 27.3s over 30 days. Bound 60s.
- Ack: the keeper's existing `AnswerBound`, 2m (p99 57s over 2,798
  keep-alives). Re-measured from the daemon logs: 105 sent-to-answered pairs,
  p99 44s, max 77s.
- Stand-down (answer to `stood an idle session down`): 87 pairs from the
  daemon logs since 2026-10-26, p50 1.0s, worst 29.3s (a burst of six at
  16:54 on 2026-11-20). Bound 60s.
- A start that waits for a restore slot has its bound scaled by the restore
  rounds ahead of it (one start bound per `MaxRestores` starts ahead).
