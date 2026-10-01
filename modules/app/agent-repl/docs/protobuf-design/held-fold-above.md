# Held prompts: "fold above"

The owner asked (2026-09-30) for a "fold above" button on the held prompt
entries in the webapp's hold tray: it "tacks on the prompt to which it's applied
to the one just ahead of it in the queue, and removes it from the held prompts /
prompt queue (in that way, you've folded it into the one above to be handled at
the same time as it)." The lead ruled on the details (below), with the owner's
clearance to decide.

## Core design principles

1. The daemon decides whether the button is offered, and which entry it folds
   into.
   - The webapp holds no business logic (module AGENTS.md), so the tray entry
     carries the button as a daemon-resolved element, and the card derives
     nothing from its neighbours.
   - It does not claim that the webapp may hide the button on its own reading
     of the tray; it draws exactly what is pushed.
2. A prompt is never folded into an entry the user did not see above it.
   - Both entries travel as typed echo tokens, and the daemon refuses, folding
     nothing, the moment the entry ahead is not the one the client named.

## Lead's rulings (recorded as given)

1. The button is on a held PROMPT only when the entry directly ahead of it is
   also a held prompt that can still be changed.
   - Never on the first entry.
   - Never when the entry ahead is a session act (a model or permission-mode
     change, `/compact`, `/clear`).
   - Never into the running turn: that is the classifier's `after_tool_call`
     join, a separate mechanism.
2. The folded content is appended to the entry ahead, separated by a blank
   line, keeping every block in order.
   - The entry ahead keeps its place and identity, and the folded entry leaves
     the queue, in ONE store transaction.
3. The fold reuses the classifier's coalescing machinery, and the merged
   entry's verdict is treated exactly as an edit's commit treats it.
4. Typed refusals, folding nothing: the entry ahead moved, either entry gone or
   being edited, the entry ahead not a prompt.
5. The proto shape is the implementer's to choose (this record).
6. The button follows the existing held-entry buttons exactly, labelled
   "fold above".
7. Emacs gets the verb only if it already offers the sibling tray actions.

## Landed changes

### 1. A new endpoint, `agentrepl.v1.FoldHeldPrompt`

- What: `service.proto` (DAEMON-HOLD TRAY section) gains `FoldHeldPrompt`, in
  `endpoint_fold_held_prompt.proto`.
  - The request carries `workspace`, `turn` (the folded prompt) and `above`
    (the entry the client saw directly ahead of it).
  - The response is the standing `oneof result { success; error; }`.
  - The error arms are the four workspace-addressing ones every rpc shares, then
    `no_such_hold`, `not_held`, `not_a_prompt`, `above_moved`
    (carrying `optional current_above`), `above_not_a_prompt` and
    `being_edited` (carrying `editing_turn`).
- Why a new endpoint rather than a fourth `UpdateHeldPrompt` action arm.
  - `UpdateHeldPrompt`'s three arms are payload-free answers about ONE entry,
    and its error arms are about that entry.
  - A fold names TWO entries, and four of its refusals (`above_moved`,
    `above_not_a_prompt`, `not_a_prompt`, `being_edited`) mean nothing for a
    release, a drop or an accept.
    - Putting them in `UpdateHeldPromptError` would make "a drop refused because
      the entry above moved" representable: the adjacent-exclusivity defect.
  - `EditHeldPrompt` is the precedent: a held-prompt verb with its own
    vocabulary got its own endpoint.
- Why `not_held` and not `already_delivered`.
  - The client does the same thing however the folded prompt left (delivered,
    dropped, or itself folded): its card is stale and the next push takes it
    down. One arm states that.
- Why `being_edited` names the entry.
  - It reuses the daemon's existing `BeingEditedError` refusal, which already
    fills `editing_turn` for `EditHeldPrompt`, so the two rpcs spell the same
    condition the same way.
- `above` equal to `turn` is Connect InvalidArgument, never an arm: it is a
  malformed request, not a state of the queue.

### 2. `frontend.v1.HeldPrompt.fold_above` and `HeldPromptFoldAbove`

- What: an optional element on the tray entry, present exactly when the fold is
  offered, carrying `above`, the typed echo token of the entry directly ahead.
- Why an element and not a boolean.
  - figma→idl: the button is a drawn element and gets its own message.
  - The token the click must send back lives inside the element that sends it,
    so a client can never pair the button with the wrong entry.
- `HeldPrompt.coalesced`'s comment now says a user's fold sets it too: the
  merged entry wears the same "coalesced" badge, because the fact it states
  ("later prompts were folded into this one") is the same fact.

## Consequences and implementation notes

- ONE eligibility definition (`daemon/internal/holdfold`) serves both the tray
  (offer the button) and the verb (refuse the fold), so the button is offered
  exactly when the verb would accept it at that instant.
  - The session-act predicate (`heldIsAct` in `promptqueue/ahead.go`) moved
    there unchanged, so "what is a session act" has one reading for the
    classifier's queue walk, the tray and the fold.
- The store transaction is `wsm.Store.CoalesceHeldPrompts`.
  - It writes the merged content (marked coalesced) and retires the folded
    entry with tombstone kind `coalesced`, or neither.
  - The classifier's coalesce uses it too. Before, that path made two separate
    writes, and a failure between them left the merged text standing beside a
    folded entry still in the queue.
  - `Coalescence.DiscardVerdict` is the one difference between the two callers.
    - A user's fold discards the merged entry's verdict and acceptance, as
      `ReplaceHeldPromptSaid` does for an edit.
    - The classifier's coalesce keeps the queued entry's standing verdict, as it
      did before (behavior preserved; see "Open question").
- Verdict handling mirrors `CommitEdit` exactly.
  - The fold runs under the delivery lock, then the verdict lock.
  - It bumps the content epoch of BOTH turns, so a verdict still in flight about
    either one's old words is discarded when it lands.
  - It drops a queue jump either turn had earned (`clearHeadIf`), republishes
    the tray, and reclassifies the merged entry through `reclassify`.
- The blank-line separation joins the entry's last text block and the folded
  prompt's first text block into one text block, `"<ahead>\n\n<folded>"`.
  - When either side of the seam is not text (an image), the blocks are
    appended as they are, in order.
  - The classifier's coalesce keeps appending blocks without joining, as
    before.
- The merged entry keeps the entry ahead's identity: turn, origin, target
  (bubble-addressed row), delivery (ordinary or deferred) and queue position.
  The folded prompt's own target and delivery are not carried over.
- No footer line is drawn for a user's fold. The classifier's coalesce draws a
  "coalesced" submission line because the user did not ask for it; a fold is the
  user's own click, and the tray push is the answer.
- Emacs is untouched. It does not mirror the hold tray (no release, drop or
  edit-begin there; `held-edit.el` only hosts an edit the webapp began), so per
  ruling 7 it gets no fold verb. Its Go test fake daemon
  (`lisp/testsupport/fakedaemon`) implements the new rpc only because it mirrors
  the generated handler interface, one method per rpc.

## Open question for the owner

- The classifier's coalesce keeps the queued entry's verdict on the merged
  text, while a user's fold discards it. The two now share one transaction,
  and the flag is the only difference. Whether the classifier's coalesce should
  also re-judge the merged entry is a behavior change left for the owner.
