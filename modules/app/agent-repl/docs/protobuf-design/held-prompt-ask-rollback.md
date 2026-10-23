# A joining prompt an ask overtook goes back to the held tray

## Problem

The owner, 2026-10-23: questions were sometimes drawn in the feed at the same
time as a prompt that had just been "accepted". The prompt is mirrored to the
feed the moment the daemon sends it to the shim (`promptqueue.mirrorAccepted`),
which keeps the composer responsive. The owner asked that the prompt roll back
to being a held prompt when a question, or anything like it, comes in before
the shim has actually handled the prompt.

The race is on the `join_running_turn` path. The classifier's
`after_tool_call` verdict sends a prompt to join the running turn. The shim
pushes it into the vendor's command queue at once, and the vendor folds it in
at the running turn's next tool boundary. When the model raises an ask
(`AskUserQuestion`, or a tool call that needs consent) after the push, that
boundary lies behind the user's answer. The prompt then stood in the feed as
accepted beside a card the user had not answered, and it ran only once the
card was answered. The same thing happened when the ask opened during the
classifier's call, before the join was sent at all.

## Core design principles

- **The shim, which sees both the ask and the waiting joins, decides.** Both
  live in the shim's single-threaded session, so the decision has no window.
  A join is either pushed before the ask registers, and is then taken back, or
  it arrives after, and is then refused.
- **The vendor arbitrates the take-back.** The shim asks the vendor to drop the
  queued message by its uuid (`cancel_async_message`). The vendor answers
  whether it dropped it. A prompt it already took keeps its usual fate (folded,
  or run as its own turn), so every joining prompt leaves by exactly one door.
- **A withdrawn prompt is held again in its place.** It keeps its original
  `QueuedAt` and gets a new turn id, because a turn id is started once and the
  vendor uuid is derived from it.

## Landed changes

### 1. `conversation.v1.SessionUpdate.join_withdrawn` (`conversation.v1.SessionJoinWithdrawn`)

- WHAT: a new session-stream event naming the withdrawn prompt's turn and the
  turn it was to join.
- WHY the session stream: a joining prompt has no stream of its own. The
  session stream's rule allows a unit fact only when the unit has no stream.
- The producer and consumer obligations are written on the message.

### 2. `shim.v1.StartTurnFailure.ask_open` (`shim.v1.StartTurnAskOpen`)

- WHAT: a refusal of a `join_running_turn` start while an ask blocks the
  vendor. The prompt is never pushed.
- The daemon maps it to `promptqueue.ErrAskOpen` and keeps the prompt held for
  the running turn's end, with the reason "a question or permission ask is
  waiting on you".

## Consequences

- The shim calls `Query.cancelAsyncMessage`, which the SDK provides at runtime
  but does not declare. It is held to the runtime the way `getSettings` is:
  `asQueryLike` refuses a query without it, and the sdk-canary suite fails if
  the runtime method or its declared request disappears.
- The daemon closes a withdrawn turn as `wsm.CloseWithdrawn`. It draws no
  ending, raises no fault and no banner, and its accepted row is taken down.
- A join the shim refuses for any reason now takes its accepted row down too.
  Before this change, a refused join left the row in the feed while the prompt
  stayed held.
- The withdrawn prompt waits for the running turn's end
  (`hold_for_turn_end`). The user can still force it through from the tray.
- Edge case: the turn the prompt joined can end, so the prompt stands as its
  own turn, before the vendor answers the cancel. The shim then releases that
  turn and records the case at ERROR, as it already did for a refused peer
  join. The daemon ends that turn, and the turn end delivers the re-held
  prompt.
- Asks raised by the keep-alive never withdraw anything, because the
  keep-alive's asks are never served.
