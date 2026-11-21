# Interrupt: a detached stop never claims nothing runs while a turn does

A workspace (test-suite-duration-metrics, 2026-11-20) ran `agent-repld workspace interrupt <child> -all-agents` on three children whose turns were running. Every call answered `nothing_running`, so the workspace concluded that the children were idle. `agentrepl.v1.InterruptSuccess.nothing_running` was being answered by the `all_agents` target whenever no detached work was live, whatever the turn was doing. The `all_agents` and `detached` targets never stop the turn, so that answer was a false statement about the workspace.

## Assumptions (autonomous design)

- Objective: an interrupt's answer is never a false claim that nothing runs.
- Systems: the daemon (the workspace verb, the prompt queue, the server answer, the CLI), the webapp (footer stop-all and feed bubble stops) and the elisp wire decoder. Emacs sends only the `turn` target, which never yields the new arm.
- Non-additive changes: none to the wire. `nothing_running` keeps its arm number and shape, and its meaning is narrowed to "neither a turn nor detached work".

## Landed changes

### `agentrepl.v1.InterruptSuccess.turn_still_running` (arm 7) and `agentrepl.v1.InterruptTurnStillRunning`

- What: a new success arm that carries the running turn as a `conversation.v1.TurnId`. It is answered by the `detached` and `all_agents` targets when the stop reached no detached work while a turn runs. The comments on `nothing_running`, `InterruptAllAgents` and `InterruptRequest.all_agents` now say that those targets never stop the turn.
- Why: the user wants workspaces to be able to interrupt one another and to read the answer truthfully. Naming the turn tells the caller that the bare turn stop is the verb that ends it.
- Alternatives that lost:
  - Making `all_agents` also stop the turn would change what the fan-wide stop does. The footer's "stop all" sits in the agents panel and must leave the turn running.
  - Adding a `turn_running` bool to `InterruptNothingRunning` would be a bool that selects how the arm is read, which the protobuf conventions forbid.
- Consequences:
  - The daemon decides `turn_still_running` versus `nothing_running` in one helper, `agent-repld.internal.workspace.noDetachedWork()` in `daemon/internal/workspace/interrupt.go`. It covers three cases:
    - the all-agents stop with no live work;
    - the all-agents sweep that reached nothing (stopped == 0);
    - a benign shim refusal on a single detached bubble (`shimOutcome`).
  - The no-live-session branch could also answer falsely for a detached stop: a workspace whose session is still coming up shows its accepted turn on the status surfaces. The prompt queue gained a read-only `promptqueue.TurnStops.RevivalTurn()`, which shares `revivalHoldsLocked()` with `WithdrawRevivalTurn()`. A detached stop with no session up now asks it, and answers `turn_still_running` when a turn waits on the bring-up.
  - Turn stops are unchanged: their `nothing_running` already meant that no turn was open, and the bring-up turn is withdrawn before that answer is reached.
  - Webapp wording:
    - footer stop-all: "no agents running; the turn is still running";
    - feed bubble: "already stopped; the turn is still running".
  - The elisp decoder accepts the arm. `agent-repl-interrupt` falls through to its unknown-arm error for this arm, because a turn stop can never produce it.
