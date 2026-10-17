# The cold gate's answer never sends held prompts on its own

The owner answered a cold gate with "clear context" (doom, 2026-10-13), and the
prompts held at the gate were sent into the cleared session as soon as it came
up. The gate exists so that nothing spends tokens without the user's say, and
"clear" is its big red button: it must spend nothing at all. Before this change,
every answer (pay, clear, compact) brought the session up, and the session-up
edge (`promptqueue.queue.releaseReconnectHolds`) delivered every prompt held
under `wsm.HoldColdGate`, whatever the choice.

## Core design principles

### A cold gate's answer does exactly what its button says, and nothing more

- **In the owner's terms**: "for the compaction gate, we should immediately do
  compaction or clear, with no further ado whatsoever. If the user wants to pay
  and resume, even then nothing should happen (it's up to the user to decide
  when that happens)."
- **Consequences**: no gate answer delivers a held prompt unless the button
  names the delivery. Auto-draining a held prompt when the session comes up is
  the merge queue's behavior, not the gate's.
- **Reopens**: the held-prompt-cold-gate.md exit "delivery when the answer
  brings the session up" for `frontend.v1.HeldPromptColdGateHold`.
- **Does not claim**: anything about `HoldReconnect`, `HoldMerge`,
  `HoldBuildRefresh` or `HoldShutdown`. Those holds still release on their
  own edges. A prompt the user submits AFTER clicking an answer is the user's
  own new send and is delivered when the session comes up.

## Landed changes

### 1. Each answer's effect on held prompts (owner ruling, 2026-10-13)

- **clear**: every prompt held at the gate is DISCARDED the moment the daemon
  takes the answer, before the re-open starts.
- **pay** and **compact**: the held prompts are KEPT, under a new hold
  (`frontend.v1.HeldPrompt.cold_gate_answered`, `wsm.HoldColdGateAnswered`,
  stored 5), and are never sent automatically. The user sends each one
  (release) or cancels it.
- **compact and resume** (new answer arm
  `agentrepl.v1.AnswerColdGateRequest.compact_and_resume`, the same payload as
  `compact`): compacts, then sends the held prompts once the compacted session
  is up, through the existing session-up release.
- "Held at the gate" means a prompt under either `HoldColdGate` or
  `HoldColdGateAnswered`. A prompt kept by an earlier pay or compact is still
  unsent when a later gate stands, so that gate's clear discards it and its
  compact-and-resume sends it.
- **Why the restamp happens at the answer, not at the session-up edge**: the
  answer is taken under the delivery lock BEFORE `ResumeColdDetached` starts.
  So the session-up release cannot run before the prompts are restamped
  (keep) or tombstoned (clear). The release only ever delivers `HoldColdGate`
  entries, so the race is ruled out by structure, not by timing.
- **A failed re-open**: the gate stands again (unchanged). Kept prompts stay on
  `cold_gate_answered`, and the re-stood gate counts them, so its own
  compact-and-resume sends them. Discarded prompts stay discarded.
- **Consequence accepted**: discarding on clear is immediate and permanent.
  If the clear's re-open then fails, the prompts are still gone.

### 2. The gate offers "compact and resume" only when prompts are held at it

- `frontend.v1.FeedColdGateStanding.compact_and_resume` (optional element
  message `FeedColdGateCompactAndResume`, carrying the held count) is set only
  when at least one prompt is held at the gate and the gate offers compaction.
  The daemon resolves the button's presence. The webapp does not derive it
  from the tray.
- The fleet republishes the standing gate row whenever the count changes.
  The prompt queue reports the count on every tray republish
  (`promptqueue.queue.pushTray`), which is the one place the held set is
  published.
- The button wears the same color as the compact opener
  (`hibernation-compact`), and opens the same model/scope submenu. Its send
  answers `compact_and_resume`.
- A `compact_and_resume` answer to a gate that served no compact menu is
  refused `unserved_remediation`, as `compact` is. An answer that arrives
  after the held prompts were cancelled is not refused: it compacts and sends
  nothing.

### 3. Release (send now) of a kept prompt

- A `cold_gate_answered` prompt is released by the ordinary release path once
  the session serves. While a gate stands again, the release is refused, as it
  is for `cold_gate`.
- A kept prompt stands outside the queue line. Prompts submitted later are not
  classified against it, folded into it, or held behind it
  (`promptqueue.queue.itemAhead`, `submitBehindHeld`).

### 4. An answered gate shows its workspace (owner ruling, 2026-10-17)

- **In the owner's terms**: a hidden workspace "should be unhidden immediately
  after any gate pass, and the selected option should be immediately executed
  and obeyed."
- Every answer (pay, clear, compact, compact and resume) on a workspace in
  the hidden section moves it out (`workspace.verbs.recordHidden`), after the
  gate is spent and before the held prompts are settled and the re-open
  starts. So the session the answer brings up is one the daemon runs and
  keeps warm (`wsm.Workspace.Runs`, `wsm.Workspace.KeepaliveHeld`).
- **Why**: a hidden workspace's paid re-open was stood down by the cache
  keeper the moment it came up (its cache had lapsed long before), so the
  gate stood again on the next selection, answer after answer.
- A show that cannot be recorded refuses the answer, and the gate stands
  again, unspent (`workspace.verbs.reraiseRefusedColdGate`).

