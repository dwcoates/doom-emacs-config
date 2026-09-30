# Footer activity tiers — vetting register

These are investigations still owed now that the tiered activity contract
has landed (see `footer-activity-tiers.md`).

## V1. Main-agent reasoning text reaches the daemon as deltas

- **Assumption:** the shim streams `conversation.v1.AgentThinkingUpdate.text`
  deltas for the main agent under the owner's configuration, often enough
  that the transient `thinking` line moves during a turn.
- **Affected:**
  - `frontend.v1.FooterActivityTransient.thinking`
  - `frontend.v1.FooterActivityTransientThinkingText`
  - If reasoning is always withheld or arrives only at success, the kind
    degrades to `withheld`, or to a single line per reasoning unit.
- **Verify:**
  - Grep the live daemon log for the footer's thinking activity branch on a
    real turn.
  - Alternatively, read the shim's thinking producer and confirm it emits
    `update` frames rather than only `success`.
- **Status:** WITHDRAWN 2026-09-30. The owner dropped the live reasoning
  and response tails, so no contract rests on this assumption.

## V2. The task tracker's moves carry a subject and whole-tracker counts

- **Assumption:** the `TaskAct` activity (or the tasks panel's own state)
  gives the resolver the moved task's subject and the tracker's
  completed/total counts at each move.
- **Affected:**
  - `frontend.v1.FooterActivityTransientTask` (`subject`, `completed`,
    `total`)
- **Verify:**
  - Read `conversation.v1.AgentActivity` `TaskAct` and the resolver's tasks
    panel state in `daemon/internal/resolve/footer/chips.go`.
- **Status:** OPEN

## V3. Context-usage reports arrive often enough to be the enduring context line

- **Assumption:** `conversation.v1.SessionContextUsage` (`total_tokens`,
  `max_tokens`) is reported at least once per turn under the owner's
  configuration.
- **Affected:**
  - `frontend.v1.FooterActivityEnduring.context_window`
  - `frontend.v1.FooterActivityEnduringContextWindow`
- **Verify:**
  - Count `context_usage` session updates per turn in the daemon log.
  - Read the shim's producer to see what triggers a context-usage sample.
- **Status:** OPEN

## V4. A resumed agent keeps the work handle its wait was keyed by

- **Assumption:** after `resumed`, the vendor's continuation (delivered
  through the main agent's `SendMessage`, and adopted through the ordinary
  adoption path) either keeps the failed run's `conversation.v1.DetachedWorkId`
  or arrives as new work that the daemon can tell apart from the wait.
  - The wait's row has to leave (resumed) without a new row for the same
    agent flickering in or doubling up.
- **Affected:**
  - `conversation.v1.SessionNetworkResumeWait.work`
  - `conversation.v1.SessionNetworkResumeOutcome.work`
  - `frontend.v1.FooterAgentRow` for the resumed agent.
  - If the continuation is new work under a new handle, the `resumed` arm
    may need to name that handle.
- **Verify:**
  - Evidence so far comes from test code, not observed behavior.
    - Master commit `a0b4e56cd` ("network-resume starts the resumed task
      before the send's result") has the fake SDK start the resumed task as
      a NEW task.
    - Master commit `9f8c44f65` opens the resume's turn on the MAIN session
      through `adoptTurn`.
    - Together these suggest the continuation arrives as new detached work
      under a new handle, so the `resumed` arm may need to name that
      handle.
  - Read the adoption path the network-resume merge uses
    (`engine/network-resume-prompt.ts`, and the adoption code it names) and
    the shim integration test `test/integration/detached.test.ts` for the
    handle the resumed run's frames carry.
- **Status:** SETTLED 2026-09-28. The continuation arrives as NEW work under
  the resume's own handle, with the same agent.
  - Evidence (mocked-vendor and test code, not observed real-vendor
    behavior):
    - The fake vendor starts the resumed task under the `SendMessage` call's
      `tool_use_id` (`agent-shim/claude/shim/src/fake/scenarios/subagents.ts:847`).
    - The fold names detached work by that `tool_use_id`
      (`src/convert/detached.ts:389`, `:1057-1061`; `src/convert/ids.ts:176`).
    - The shim integration test asserts that the new work's agent is the
      spawn's `conversation.v1.AgentId`
      (`test/integration/detached.test.ts:1318`, `:1323`).
  - Contract consequence: NONE.
    - The outcome `resumed` retires the WAIT'S handle, so the waiting row
      leaves.
    - The continuation's running row arrives through the ordinary
      detached-work announcement under its own handle.
    - The footer joins nothing across the two, so the `resumed` arm does not
      name the new handle.
    - The two rows can overlap for at most the gap between the new work's
      announcement and the outcome frame. This is display only, accepted
      under the visibility-only ruling.
  - A resumed run that fails again opens its next wait under the new handle,
    which is the handle its failure terminal retired.
