# Prompt selection, deselect-on-scroll, and rollback

The owner (2026-10-01) asked for four connected controls: a selected bubble
ends once it scrolls out of view, prompts can be selected like responses
(`C-S-p` / `C-S-n`), `C-c C-RET` rolls the conversation back to just before
the selected prompt (or the latest one, which is "cancel"), and `C-c M-RET`
does the same and also restores files. The full agreed plan is
`docs/PLAN-prompt-rollback.md`. The owner delegated every proto decision to
the lead for this change ("don't ask for protobuf approval"), so each entry
below is the lead's decision, recorded with its reasoning.

## Core design principles

- **The daemon is the only holder of the selection.** Everything that acts on
  a selection (a sent prompt replying to a response, a rollback to a prompt)
  reads the daemon's own selection; no client carries a copy back.
  - Consequence: `agentrepl.v1.SubmitPromptRequest.reference_response_feedid`
    is retired; the daemon applies the selected response itself.
  - Consequence: Emacs learns the selection from a push
    (`agentrepl.v1.HostWorkspaceSelection` on the host workspace watch) and
    uses it only to decide what escape does.
  - Why: the owner sent a prompt that went out as a reply to a response they
    had selected and scrolled away from. Emacs held its own copy of the
    selection, so nothing the webapp saw could end it. With one holder, the
    webapp ending a selection ends it everywhere.
  - Not claimed: the webapp's report that a row left the viewport and a
    keypress in Emacs travel on different connections, so a send made in the
    instant between a scroll and that report still carries the reply. The
    window is the report's transit time, far below a human's next keypress;
    no ordering between the two connections exists to make it impossible.

## Landed changes

### 1. `frontend.v1.FeedSelection` is a oneof of none / response / prompt

- What: the `selected` / `active` / `center` fields are replaced by a oneof.
  `FeedSelectionNone` carries how the viewport reacts (`return_to_tail` when
  the user dismissed the selection, `stay` when it ended because the row left
  the viewport). `FeedSelectionResponse` and `FeedSelectionPrompt` each carry
  the selected row.
- Why: a prompt can now be selected, and must never be selected alongside a
  response; and an out-of-view end must not jump the reader to the tail the
  way a dismissal does.
- Consequences: the webapp centers the selected row whenever the selection
  moves to a new row (the old `center` always equalled `selected`). The
  old bool beside an optional was the state-enum defect the oneof removes.

### 2. `agentrepl.v1.SelectFeedRow` replaces `SelectResponse`

- What: one rpc whose request is a oneof move: step through responses, step
  through prompts, clear, or report a row that left the view
  (`SelectFeedRowLeftView`, naming the row). Success says whether a row is
  selected, a step found nothing selectable, or nothing is selected.
- Why: the selection is one state with one owner, so its moves are one rpc.
  The left-view report names the row so a stale report (the user already
  stepped to another row) is ignored by construction rather than racing.
- Consequences: Emacs (`lisp/input.el`, `lisp/wire-verbs.el`, `lisp/rpc.el`),
  the webapp (`webapp/src/feed/background-click.ts`), the daemon
  (`internal/server/select_response.go`, `requestlog_server.go`,
  `validate.go`) move to the new rpc.

### 3. `SubmitPromptRequest.reference_response_feedid` retired (tag 6 reserved)

- What and why: per the first core principle. The daemon prepends the
  selected response when it accepts a prompt, then clears the selection.
- Consequences: `internal/server/prompt.go` reads its own selection; the
  refusal for an unresolvable reference stays (a selection naming a row the
  feed no longer has is refused, never dropped).

### 4. `agentrepl.v1.HostWorkspaceSelection` pushed on the host workspace watch

- What: which kind of row is selected (none / response / prompt), never the
  row itself, because Emacs never names it back.
- Why: Emacs's escape handling ("press escape again to clear the selection")
  needs to know whether a selection stands, and must stay true when the
  webapp ends one.

### 5. `shim.v1.RollBackSession` — the shim performs the vendor-side rollback

- What: one shim rpc naming the first dropped turn (`to_before`), every
  dropped turn, and whether files are restored. It interrupts the open turn,
  (restoring) stops the dropped turns' detached work and restores files, then
  restarts the query resumed at the chain entry before the prompt. Typed
  failures: no session, prompt not recorded, first prompt, unseen prompt,
  vendor refused, files not restorable.
- Why one shim operation: the shim alone owns the vendor query, and only the
  shim knows which turn spawned which detached item (`live.spawnedBy`,
  `engine/turn.ts:1162`); the daemon's `sessionwatcher.LiveWorkSet` carries no
  spawning turn.
- The vendor uuid is DERIVED from the turn id (stated on
  `shim.v1.StartTurnRequest.turn`): a uuid-shaped turn id (a prompt the
  sidecar adopted from the vendor's own record) is used as is; a daemon-minted
  16-hex turn id maps to a version-5 uuid. Why: the turn-to-uuid link lived
  only in the shim's in-memory send ledger (`engine/sends.ts`), so a restarted
  shim could not find the prompt. Deriving it makes losing the link
  impossible rather than unlikely, and stores nothing. A StartTurn repeating a
  turn id starts nothing (the endpoint's existing rule), so one uuid is never
  sent twice by one shim.
- The fork point is the prompt record's `parentUuid` in the vendor
  transcript: the SDK requires the KEPT turn's last chain entry
  (`sdk.d.ts` `resumeDropsTurn` doc), not the last assistant uuid the
  keep-alive anchor tracks (`engine/keepalive.ts`).
- `resumeDropsTurn` validates exactly one dropped turn. A rollback dropping
  one turn passes it; one dropping several cannot (the later prompts are
  other turns, which the guard refuses), so the shim applies the guard's
  intent itself: every prompt record after the fork point must be a prompt of
  a turn in `dropped_turns`, else `unseen_prompt`.
- A refusal is never retried (SDK guidance): the shim resumes the session
  plainly and answers `vendor_refused`.
- Files are checked (`rewindFiles` dry run) before the conversation is cut,
  and restored on the old query before it closes; `enableFileCheckpointing`
  is turned on for every session.
- Accepted cost: the first prompt of a vendor conversation cannot be rolled
  back (no chain entry precedes it to resume at); the answer says `/clear`
  starts over. Cancelling the first prompt of a fresh conversation is
  therefore refused.

### 6. `agentrepl.v1.PlanRollback` / `RollBack` with a typed `RollbackToken`

- What: PlanRollback (files kept or restored) answers a plan — token, target
  (selected or latest, with an excerpt and how many prompts drop), files
  (kept, or restored with how many running detached items stop), an
  interrupt marker when a turn runs, and how many held prompts drop — or
  `nothing_to_roll_back`. RollBack takes the token and answers the rolled-back
  prompt's `conversation.v1.UserSaid` (and, restoring, the file count), or a
  typed refusal.
- Why a plan and a token: every rollback is confirmed in the minibuffer, and
  the confirmation must state the side effects in red; the token makes the
  confirmed plan the only thing RollBack can perform. The daemon refuses a
  token whose effects no longer match (`plan_stale`), so a rollback never does
  more than the user read.
- Why the target is resolved by the daemon: the daemon holds the selection
  (core principle), so Emacs never names a prompt.
- Consequences: the daemon performs RollBack under the prompt queue's
  ownership of the turn sequence (`internal/promptqueue`, the drain lock at
  `queue.go:56`), so no prompt is delivered part way; the rolled-back turns
  are recorded durably (wsm) and the feed refuses to draw any row of a
  rolled-back turn at its one upsert door, which covers live frames, history
  replay after a restart, and the vendor transcript's abandoned branch the
  sidecar keeps ingesting. The store keeps the dropped entries (it has no
  delete, `store/v1/service.proto`); the feed's gate is what hides them.
