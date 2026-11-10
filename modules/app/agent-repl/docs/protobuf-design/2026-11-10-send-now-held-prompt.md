# "Send now" on a held prompt sends it now (2026-11-10)

Design record for `agentrepl.v1.UpdateHeldPromptReleaseRefused.cause`,
`frontend.v1.HeldPrompt.sent_now` and `frontend.v1.HeldPromptBadge.sent_now`.
Autonomous design mode.

## Trigger

Workspace `torch-human-perf`, daemon pid 38660: the agent started a turn on
its own (`daemon.sessionwatcher.turn_adopted`, origin
`PROMPT_ORIGIN_VENDOR_STARTED`), the user's prompt was held behind it, and
two presses of "Send now" each answered `release_refused` because the
running turn is never joined. The card said "the session would not take this
prompt now", one sentence for every cause.

Owner ruling: "Send now" means send now. It is supported on a turn the agent
started on its own and is not rejected.

## Assumptions (autonomous mode)

- A1: A release that cannot go at once but whose blocker clears on its own
  is a SUCCESS, not an error: the user's request was taken, and the card is
  where its waiting is drawn. Evidence tier: the ruling.
- A2: An interrupt of the running turn is the only way to "send now" into a
  turn that cannot be joined. It is the path an interjection and a released
  session act already take (`promptqueue.registerInterjection`,
  `promptqueue.sendInterrupt`), now shared as `releaseByInterrupt`.
- A3: A waiting press must survive a restart and a handover, so it is
  durable on the held prompt row (`wsm.SentNow`, layout 120), not in the
  queue's memory.

## The choice per cause

Every cause the old `release_refused` comment listed, and the joins that
could not go:

| Cause | Outcome | Why |
|---|---|---|
| running turn the agent started (never joined) | interrupt, deliver at its real end | the ruling |
| an ask open blocks the join | interrupt, deliver at its real end | "send now" on a turn waiting on the user; the prompt answers instead |
| a second press while its interrupt is under way | accepted, already going | the interrupted turn's end delivers it once |
| another prompt's interjection under way | accepted, waits; goes right after (joins its turn) | the interjection was decided first |
| uninterruptible verdict / running context cut | accepted, waits for the cut's end, goes first | interrupting a cut spends its cost for nothing |
| reconnect hold (session not up, vendor block) | accepted, waits for the session, goes first | nothing to send to yet |
| bring-up in progress | accepted, waits, goes first | same |
| drain for a bounce | accepted, waits for the new shim, goes first | the bounce delivers every held prompt |
| merge drives the session | accepted, waits for the merge's end, goes first | a prompt forced in would run inside the merge's turns |
| shim call that opens no turn in flight | accepted, waits for its settle | one call at a time per workspace |
| join deferred to a standing call / turn ended before the join | accepted, waits until the session can take it | the settle or the pop sends it |
| prompt being edited | REFUSED `being_edited` | sending would discard the edit under way |
| prompt queued behind an edit | REFUSED `behind_edit` | an edit keeps its prompt and all after it in order |
| cold gate standing | REFUSED `cold_gate` | sending spends the cold re-read only the user's gate answer decides |
| shim refused the join / turn record failed | REFUSED `session_refused` | a real failure; the prompt waits for the turn's end |

A waiting press that later meets a refusing cause (the user began editing
it) is withdrawn: the mark is cleared and the record says so.

## How a waiting press goes (structural, no timers)

- `promptqueue.nextDeliverable` takes a marked prompt first, behind only the
  head an interjection installed, so every pop (turn end, session up, bounce
  finish, merge end) delivers it first.
- `promptqueue.delivery.unlock` takes every waiting press again
  (`promptqueue.replaySentNow`) on the same decision a fresh press takes
  (`promptqueue.decideRelease`). Every blocker it waits on clears under the
  delivery lock, so the decision that cleared it is the one that sends it.

## Landed shapes

```protobuf
message UpdateHeldPromptReleaseRefused {
  oneof cause {
    UpdateHeldPromptReleaseRefusedBeingEdited being_edited = 1;
    UpdateHeldPromptReleaseRefusedBehindEdit behind_edit = 2;
    UpdateHeldPromptReleaseRefusedColdGate cold_gate = 3;
    UpdateHeldPromptReleaseRefusedSessionRefused session_refused = 4;
  }
}

// frontend.v1.HeldPrompt
HeldPromptSentNow sent_now = 28;
message HeldPromptSentNow { string waits_for = 1; }

// frontend.v1.HeldPromptBadge.stands_for
HeldPromptBadgeSentNow sent_now = 18;
```

- The refusal carries no free text: each arm is a cause, and each client
  words it (`webapp` `releaseRefusedSentence`, Emacs
  `agent-repl--input-release-refused-text`). A refusal with no cause is a
  malformed frame on both clients, and the daemon never answers one (a bare
  `promptqueue.ErrReleaseRefused` is not a typed refusal at the server).
- `waits_for` is daemon-worded display text, as every held badge's words are
  (`resolve/holds.heldStatus` is the one place they are composed). It ranks
  after the edit and before the hold, so the blocker the card waits on stays
  visible as a note.

## Consequences

- `release_refused`'s comment no longer lists the waits.
- Layout 120 adds `held_prompts.sent_now_at` and `sent_now_waits_for`
  (additive).
