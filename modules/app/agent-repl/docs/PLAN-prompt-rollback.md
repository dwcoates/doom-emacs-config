# PLAN: prompt rollback, prompt navigation, deselect on scroll, and three smaller items

Agreed with the owner on 2026-10-01. Work in the worktree
`~/.config/doom-worktrees/footer-activity-updates` on branch
`merge-queue-rework`, which is level with master. Land by rebasing onto master,
then cherry-picking. No CPU load testing; run each suite once, one at a time.
Use at most one or two subagents, each with a large piece, and keep any
subagent dispatch to the opus-medium tier.

Proto changes are the lead's to design. Load `/create-or-update-protobufs`
for its conventions only once the owner says to start implementing, and skip
its steps that ask the owner. Record each proto decision in a design record
under `docs/protobuf-design/`.

## 1. Drop the vendor's rate-limit line from the footer

- The salient `rate_limit` arm (`frontend.v1.FooterStatusActivityRateLimit`,
  "weekly nearly spent 85% · resets in 2h") is removed from every status arm's
  salient oneof. Its tags are reserved.
- The enduring usage line already shows the 5-hour and weekly figures, and
  the owner ruled it covers this information.
- The vendor's rate-limit events still feed the enduring usage figures; only
  the separate line goes.
- Update `docs/protobuf-design/footer-activity-tiers.md` (the "Rate limiting
  is salient" ruling is superseded), `AGENTS.md`, the e2e and webapp tests,
  and the changelog.

## 2. A selected bubble is deselected when it scrolls out of view

- Applies to both response selection (`C-p` / `C-n`) and the new prompt
  selection (section 3).
- A selection ends when the selected bubble is entirely outside the feed's
  viewport, whatever moved it: the user's scroll, or new output pushing it
  away.
- Why: the owner selected a bubble with `C-p`, scrolled away, and the next
  prompt still went out as a reply to it.
- Find where the selection lives first (Emacs or the webapp) and which side
  composes the "Replying to" attachment. The visibility check must end the
  selection on the side that composes the attachment, so a send can never
  carry a selection the user can't see.

## 3. Prompt navigation

- `C-S-p` / `C-S-n` in the input window select the previous and next prompt
  bubble, mirroring `C-p` / `C-n` for responses.
- The selection border is the same as for responses.
- Selectable prompts: main-agent prompts in the current vendor session only,
  not before the last compaction or `/clear`, and not a subagent's prompts.
  Nothing else can be selected, so nothing else can be rolled back.
- Check: `C-S-p` is bound inside a leader map in `keybindings.el` (paste:
  create PR). That is under a prefix and does not clash with the input
  window's bindings, but confirm it.

## 4. Rollback (one mechanism; cancel is a shortcut for it)

### Keys

| Key | With a prompt selected | With nothing selected |
| --- | --- | --- |
| `C-c C-RET` | roll the conversation back to just before the selected prompt | roll back the latest prompt ("cancel") |
| `C-c M-RET` | the same, and restore files to their state then | the same for the latest prompt, with files |

- `RET` keeps sending the input window, and `C-c C-k` stays the plain
  interrupt. Neither changes.
- Cancel is exactly a rollback to just before the latest prompt; there is no
  other distinction, and no "has it started answering" condition.

### Confirmation (minibuffer)

- Every rollback asks for confirmation.
- `C-c C-RET` says it does not restore files, and that `C-c M-RET` does.
- `C-c M-RET` says it also restores files and cancels everything started
  since the prompt, and that `C-c C-RET` leaves files alone.
- Side effects that apply are listed in red:
  - a running turn will be interrupted;
  - queued prompts added since the rollback point will be dropped;
  - for `C-c M-RET`, background agents, shells and monitors started since
    then will be cancelled.

### What a rollback does

1. Interrupt the running turn, if any.
2. Drop the held-queue prompts enqueued after the rollback point's prompt.
3. `C-c M-RET` only: cancel every detached item (subagent, shell, monitor)
   started since that prompt, then restore files with the SDK's
   `Query.rewindFiles(userMessageId)`.
4. Rewind the vendor conversation: the shim restarts its session with
   `resume` plus `resumeSessionAt` (the kept turn's LAST chain entry) and
   `resumeDropsTurn` (the dropped prompt's UUID), so the SDK refuses a cut
   that would drop anything we have not observed. Map that refusal
   ("Resume rejected by --resume-drops-turn:") to a typed failure: never
   retry, and leave the conversation as it was.
5. Remove the dropped turns from the feed and from the stored history, so
   neither a reload nor a restart draws them again.
6. Emacs saves the input window's contents the way `agent-repl-discard-input`
   does, then fills it with the rolled-back prompt's text and attachments.

### Decided points and gotchas

- The daemon performs the whole rollback as one operation and answers it
  with a typed outcome. Emacs refills the input window only on success.
- The request names the prompt by a typed echo token the client received,
  and the daemon refuses with a typed reason if that prompt is no longer
  rollback-able (compacted away, cleared, already rolled back, not the main
  agent's).
- A rollback with the shim mid-restart, or racing a new prompt, must be
  structurally ordered (one owner of the session's turn sequence), never
  retried.
- File checkpointing: turn on `enableFileCheckpointing` for every session.
  It backs up a file before the agent's edit tools change it, so its cost is
  modest. Its limits go in the `C-c M-RET` confirmation and the user guide:
  - it restores only changes made through the agent's file-editing tools,
    not changes made by shell commands (`sed -i`, builds, `git checkout`);
  - it does not undo git commits, so files and history can disagree after a
    restore;
  - it covers only prompts sent after checkpointing was on.

## 5. User guide

- Add `docs/USER-GUIDE.md`, starting with the controls in sections 2-4.
- Add a rule to `modules/app/agent-repl/AGENTS.md`: any new or changed user
  control lands with a matching `USER-GUIDE.md` update. Existing features are
  not backfilled now.

## 6. The merge bubble survives a daemon restart

- Cause: the feed keeps daemon-made rows, such as the merge bubble's head and
  tabs, only in memory, so a restart loses them.
- Design, generic for every durable daemon-made row:
  - a wsm table (layout 16) holds each such row's published snapshot, its
    feed address and its order key, written under the feed resolver's lock
    on every `UpsertSynthesized`;
  - the feed restores them, in their original places, when a workspace's
    feed state is first built in a daemon process;
  - `ResetWorkspace` (a new vendor conversation) deletes them;
  - errors go through the canonical logger at ERROR.
- Order keys come from conversation places, so a restored row keeps its
  position relative to replayed history. Restore through the existing
  `placement.inherit` rank.

## Order of work

1. Section 6 (independent, already designed).
2. Section 1 (small).
3. Section 2.
4. Sections 3 and 4 together, with the proto changes first.
5. Section 5 alongside 2-4.
