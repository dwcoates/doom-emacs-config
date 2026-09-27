# Held-prompt interjection as a recorded interrupt cause

When a held prompt is classified as deserving an interrupt, the daemon's prompt
queue stops the running turn so the held prompt runs next. The feed then drew
the red "the turn was interrupted" bubble, which reads as an error. It is not
one: the superseding prompt, drawn as the active prompt, already accounts for
the stop. The owner wants everything about the interjection kept exactly as it
is, minus that bubble, for this cause only.

## Core design principles

None stated for this change.

## Iteration sequence

The owner delegated this change: small additive protobuf additions are the
orchestrator's to design and land without a per-increment agreement gate
(owner instruction, 2026-09-27). The three additions were landed in one
increment, walked top-down by the direction the fact travels:
conversation record, then shim request, then frontend row.

## Landed changes

### 2026-09-27 — interjection is a recorded HOW under the user-stop cause

WHAT
- `conversation.v1.AgentInterruptedByUser` gains a `command` oneof with arms
  `direct` and `interjection` (new empty messages
  `AgentInterruptedByUserDirect`, `AgentInterruptedByUserInterjection`).
- `shim.v1.KillTurnRequest` gains `optional conversation.v1.AgentInterruptedByUser commanded_by = 3`,
  and its file now imports `conversation/v1/agent.proto`.
- `frontend.v1.FeedTurnEndedInterrupted` gains a `command` oneof with arms
  `direct` and `interjection` (new empty messages
  `FeedTurnEndedInterruptedDirect`, `FeedTurnEndedInterruptedInterjection`).

WHY
- The owner: the red interrupted bubble "seems to suggest an error of some kind
  to the user in look and feel, and it isn't an error — the prompt being
  rendered as an active prompt sufficiently conveys the interrupt".
- The cause must be DURABLE. An in-memory daemon mark would be lost on a
  daemon restart, and the feed rebuilt from the store would draw the bubble
  again. The conversation record already owns "who stopped it"
  (`AgentInterrupted.cause`), so the fact belongs there, not in a second store.

WHY THE ALTERNATIVES LOST
- A third top-level `cause` arm (`by_interjection` beside `by_user`) lost:
  an interjection IS a user decision (the user sent the prompt), and the
  existing comment makes `cause` load-bearing for recovery — user stop means
  the conversation waits/does not re-drive, host shutdown means re-drive. An
  interjection recovers exactly like a user stop, so it is a sub-kind of
  `by_user`, and every consumer that switches on `by_user` keeps working.
- Dropping the `FeedTurnEnded` row for this cause (the /clear precedent,
  `daemon/internal/resolve/feed/turnended.go:64-112`) lost: `feed.proto`
  states every turn gets one row and the row's existence is the liveness
  anchor. Option (b) — keep the row, let its arm say it draws nothing — keeps
  that invariant. Owner accepted the recommendation (b).
- `commanded_by` IMPORTS `conversation.v1.AgentInterruptedByUser` rather than
  declaring a shim-side reason type, so the shim copies the caller's
  statement verbatim into the record with no second spelling to drift.

ARCHITECTURAL CONSEQUENCES
- Producer chain:
  - daemon `promptqueue` `interject` (`daemon/internal/promptqueue/classify.go:290-316`,
    `sender.KillTurn(ctx, running, false)` at :310) must state
    `commanded_by.interjection`. The `promptqueue` `KillTurn(ctx, turn, force)`
    interface (`daemon/internal/promptqueue/api.go:384`) has no way to say
    this today and must carry it; its adapter builds `shimv1.KillTurnRequest`
    in `daemon/internal/workspace/sender.go:163`.
  - The direct user stop (`daemon/internal/workspace/interrupt.go:130`) should
    state `commanded_by.direct`.
  - Other KillTurn callers (`fleet_rollout.go:172`, `restart.go:102`,
    `sessions.go:2259`) are OUT OF SCOPE for this change and keep sending no
    `commanded_by`; whether a rollout stop is truly a user stop is a separate
    question not settled here.
  - The shim stamps `commanded_by` verbatim as `AgentInterrupted.by_user` on
    the interrupted terminal.
- Resolver: the daemon feed resolver maps `by_user.command` to
  `FeedTurnEndedInterrupted.command` in `concludedOutcome`
  (`daemon/internal/resolve/feed/turnended.go:235-240`) AND on the daemon-built
  close path (`daemon/internal/resolve/feed/turnclosed.go:105`). Both paths
  must agree, live and rebuilt.
- Webapp: `drawFeedTurnEnded` (`webapp/src/feed/rows/turn-ended.ts:120-128`)
  draws nothing for `interjection`, the existing bubble for `direct` and for
  unset.
- `stripJump` (`classify.go:332-358`) reverts a refused interjection: no kill
  is issued, so no cause is recorded. Unaffected.
- Records written before this change carry `by_user` with no `command`, and
  are drawn as direct stops (the bubble), matching what they were.
- Cost accepted: a daemon/shim version skew (new daemon, old shim) drops
  `commanded_by`; the stop then records as `by_user` with no command and draws
  the bubble. No fallback is added for that; the deploy is a rollout.

VERIFICATION EVIDENCE
- The only KillTurn call sites in the daemon are the five listed above
  (grep of `KillTurn(` under `daemon/`, non-test).
- `FeedTurnEndedInterrupted` was an empty message; `AgentInterrupted.cause`
  had only `by_user` and `host_shutdown`; `KillTurnRequest` had only `turn`
  and `force`. Nothing on the wire could tell an interjection from a direct
  stop before this change.

OBVIATED-DECLARATION SWEEP
- Nothing obviated: all three changes are additive.
