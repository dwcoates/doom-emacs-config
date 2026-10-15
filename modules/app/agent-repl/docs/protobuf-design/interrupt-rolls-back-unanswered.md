# The first `C-c C-k` rolls back a prompt nothing has answered

## Problem

The owner, 2026-10-15: "On the first C-c C-k, if there's nothing in the feed,
there should be an immediate rollback. There's no need for a second press. If
there's nothing else in the feed yet after the prompt, C-c C-k should
interrupt AND rollback the prompt. It also shouldn't say 'interrupted' in the
feed in this particular situation, as it normally does."

Before this change the first press only stopped the turn (drawing the
"interrupted" turn-ended marker), and a second press, finding nothing running,
was offered the immediate rollback (`interrupt-rollback-picker.md`).

## Core design principles

- **Nothing answering the prompt means the first stop press is a rollback.**
  The owner's words above. "Nothing answering" keeps the meaning the second
  press already uses (`daemon.internal.resolve.feed.resolver.PromptFollowed()`):
  an activity row, detached subagent, shell head, permission or question card
  after the prompt in the root feed.
  - Consequence: the daemon decides at the press, from the feed as it stands,
    and performs the rollback in the same Interrupt call. Emacs keeps no
    state about presses.
  - Does not claim: a stop with output already after the prompt changes. It
    still only stops the turn, and the second press still offers the picker.
- **A stop that rolls back draws no "interrupted" marker.** The rolled-back
  turn's rows never reach the feed while the rollback runs.

## Landed changes

### 1. `agentrepl.v1.InterruptTurn.roll_back_unanswered` (opt-in)

- WHAT: an optional empty presence message on the turn target.
- WHY OPT-IN rather than every turn stop: the rolled-back prompt's words go
  back into the composer, and only Emacs has one. The webapp's footer and feed
  stop buttons have no composer, so a rollback from them would make the
  prompt's words vanish. They leave the field unset and keep stopping only.
- The daemon takes the rollback path only when all of these hold:
  - the latest reachable prompt (`RollbackPrompts`) plans (`planRollback`),
  - a turn is open (`plan.running`),
  - nothing answers the prompt (`!plan.followed`),
  - no detached agent is live. The `confirm_required` challenge exists because
    stopping the query can take live agents with it, and the shim's rollback
    interrupts the turn exactly as an unforced kill does. A stop with live
    agents goes through the ordinary challenge and stops only.
- Files are always kept. Held prompts queued after the prompt are dropped, as
  the existing second-press immediate rollback already drops them.

### 2. `agentrepl.v1.InterruptSuccess.rolled_back_unanswered`

- WHAT: a new outcome arm carrying `conversation.v1.UserSaid prompt`, the
  same words `agentrepl.v1.RollBackSuccess.prompt` carries.
- WHY its own message rather than embedding `RollBackSuccess`: files are never
  restored on this path, so `files_restored` would be a field that can never
  be set here.
- Emacs handles it exactly like a RollBack success: composer to history, then
  filled with the prompt (`agent-repl.lisp.input.agent-repl--input-rollback-done()`).

### 3. `agentrepl.v1.InterruptedTurn.not_rolled_back`

- WHAT: optional, carrying `agentrepl.v1.RollBackError cause`.
- WHY: the shim refuses some cuts on a prompt with nothing after it. The most
  common one is the first prompt of a vendor conversation (`first_prompt`),
  because no chain entry precedes it to resume at. The stop is still what the
  user asked for, so the turn is stopped by the ordinary kill, and the answer
  says why the prompt stayed.
- `RollBackError` and its arm messages moved from `endpoint_roll_back.proto`
  to the shared `rollback.proto`, because two endpoints now carry it. The move
  is a move only. The Go package and the message names are unchanged.

### 4. Feed: a rollback's turns are withheld while it runs

- Not a contract change, but it is what makes the "no interrupted marker"
  promise true. The shim interrupts the open turn first, and the turn's
  interrupted terminal reaches the daemon before the shim's rollback answer
  does. Without a hold, the feed draws the marker and then retires it when
  `RollBackTurns` runs.
- The hold lives in `daemon.internal.workspace.verbs.RollBack()`, so every
  rollback of a running turn (the confirmed one too) stops drawing the marker.
  Rows of the held turns are buffered at the feed's one upsert door. A
  successful rollback discards them. A refused one replays them in order.

## Accepted costs

- Output from the stopped turn that lands after the press, while the rollback
  runs, is dropped unseen. The press decided on the feed as it stood, and the
  user asked to throw away the turn that output belongs to.
- A first prompt cannot be rolled back by the shim (`first_prompt`), so the
  first press on a fresh workspace's first prompt still stops and draws the
  marker, with the reason stated.
