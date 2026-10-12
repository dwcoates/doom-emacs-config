# Interrupt, then roll back: the second `C-c C-k`

## Problem

`C-c C-k` in the composer interrupts the running turn. Pressed a second time,
after it interrupted a prompt, it should roll that prompt back exactly as a
rollback does today: the conversation is cut to just before the prompt and the
prompt's words land in the composer for editing. When a bubble has already
landed in the feed after that prompt, the second press must NOT roll back;
it opens an Emacs selection list (the completion backend in use) of the
prompts a rollback can reach, newest first, each with a column for the time
since it was sent and a column for the context size of the last response
before it.

Emacs holds none of that list today: the daemon owns which prompts a rollback
can reach (`daemon.internal.resolve.feed.resolver.RollbackPrompts()`), and
`agentrepl.v1.PlanRollbackRequest` can only target the daemon's selection or
the latest prompt. So the feature needs a contract change in `agentrepl.v1`.

## Core design principles

- **The second press of the stop key is the daemon's `nothing_running` answer.**
  The owner: "second press" is already relayed to Emacs as "nothing to
  interrupt"; the daemon populates a field on that answer saying whether the
  latest prompt was just received with nothing subsequent routed to the webapp.
  - Consequence: Emacs keeps no "did I just interrupt" state. Any turn stop that
    finds nothing running offers the rollback.
  - Does not claim: the detached and all_agents targets roll anything back;
    they never carry the offer.

## Landed changes

### 1. `agentrepl.v1.InterruptNothingRunning.rollback` (the offer)

- WHAT: an optional `InterruptNothingRunningRollback` with two arms,
  `immediate` (carrying a `RollbackToken`) and `choose` (empty). Unset when
  no prompt is reachable.
- WHY (owner's terms): with nothing after the prompt in the feed, the second
  press rolls it back at once with no confirmation, since nothing substantive
  is rolled back. When a response bubble or a purple-titled card already
  follows the prompt, the press opens a selection list instead.
- "Something follows the prompt": any root-feed row ordered after the prompt
  row that draws agent output, namely an `activity` row (response, thinking or
  tool card), a detached subagent, a shell head, a permission or a question.
  A `turn_ended` marker, a separation, a badge, a peer message or a command
  panel does not count. Owner: "any purple-titled card or response bubble in
  the feed after the prompt".
- WHY A TOKEN RIDES THE ANSWER, rather than Emacs calling PlanRollback after:
  the daemon decides "bare" and mints the plan in one step, and RollBack
  refuses the token as `plan_stale` when agent output has landed after the
  prompt since. A plan-after-the-fact path could roll back a prompt whose
  first bubble landed between the two calls, without the confirmation such a
  rollback needs. That race is impossible this way.
- Consequence: `daemon.internal.server.rollbackPlan` carries a `requireBare`
  flag that only interrupt-minted plans set, and `sameEffects` compares
  bareness for them. Ordinary plans keep their existing staleness rules,
  because streaming output during a confirmed plan must not make it stale.

### 2. `agentrepl.v1.PlanRollbackRequest.prompt` and the `named` target

- WHAT: an optional `frontend.v1.FeedId prompt` on the request, a
  `RollbackPlanTargetNamed named = 5` arm in `RollbackPlanTarget.chosen`, and
  a `PlanRollbackPromptNotReachable prompt_not_reachable = 5` refusal arm.
- WHY: the picker's choice must be planned by naming it. Driving the daemon's
  selection there first (SelectFeedRow, then PlanRollback) is two calls whose
  ordering another selection change could break.
- Alternatives lost: reusing `selected` would make RollBack end a selection
  that was never made.

### 3. `agentrepl.v1.ListRollbackPrompts` (new endpoint)

- WHAT: the prompts a rollback can reach, newest first. Each entry carries
  `prompt` (the row), `excerpt`, an optional `sent_at_ms` and optional
  `context_tokens`.
- `sent_at_ms` is the start of the turn the prompt opened
  (`wsm.DB.Turn().StartedAt`), unset for a prompt adopted from the vendor
  transcript (no turn record).
- `context_tokens` is every input token (cache read, cache write and
  uncached) of the main agent's last API response before the prompt. The
  feed resolver snapshots it when the prompt row is first drawn, from a running
  figure that a context cut resets to unknown.
- Raw figures, not formatted text: agentrepl.v1 is the host surface, and the
  existing rollback confirmation also composes its text in Emacs from raw
  counts. The age ticks, so only the client can draw it.
