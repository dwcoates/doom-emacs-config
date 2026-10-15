# One step through the turn boundaries

The composer had two feed-selection walks: `C-p` / `C-n` stepped through the
final responses, and `C-S-p` / `C-S-n` stepped through the prompts a rollback
can reach. The owner asked (2026-10-15) for one pair of keys, `M-s-j` (down,
newer) and `M-s-k` (up, older), that steps to the next or previous row that is
either a user prompt or a final response. The dedicated final-response keys are
removed.

## Core design principles

### The daemon is the only holder of the selection

Carried over from `bubble-selection.md`. The daemon computes where a step
lands, from rows in the order it draws them. Emacs sends only the direction.

## Landed changes

### `agentrepl.v1.SelectFeedRowRequest.move` steps through turn boundaries

- **What changed.**
  - Added the `turn_boundary` arm (field 7, `SelectFeedRowStep`).
  - Removed the `response` (2) and `prompt` (3) arms, and reserved their numbers and names.
- **Why.**
  - The owner wants to move through the feed's prompts and final responses as one sequence.
  - No client sends the kind-specific steps any more, so keeping them would leave two walks with no caller.
- **Which prompts.**
  - The walk covers the prompts a rollback can reach, not every user prompt.
  - That keeps a stepped-to prompt selected as `frontend.v1.FeedSelection.prompt`, which arms the rollback keys exactly as `C-S-p` did.
  - A user prompt before the newest compaction or clear is still selectable by a click, as `bubble`.
- **Where the order comes from.**
  - `daemon.internal.resolve.feed.resolver.TurnBoundaries()` walks the root feed's drawn row order.
  - It does not concatenate the two lists. `finalAnswers` is in conclusion order, which is not feed order once older pages load.
- **Consequences.**
  - Each landed row is selected as its own kind (`response` or `prompt`), so reply-to and rollback behave as before.
  - A step from a selected `bubble` (a click on a row the walk does not hold) starts at the newest boundary in either direction, as a step from "another kind" did before.
  - The Elisp flash prefix for a step is now `selection`, and the empty walk says `selection: no prompt or final response to select`.

## Composer keys on a selection (no contract change)

The owner asked (2026-10-15) for the selected row to answer `RET` and the
search keys. These are Emacs-only. They read the selection's kind off the
host watch, which already carries it.

- **`RET` on a selected prompt rolls back to it.**
  - It keeps files, as `C-c C-RET` does. `C-c M-RET` stays the way to restore files.
  - This replaces the 2026-10-01 ruling for prompts a rollback can reach, which said `RET` quotes a selected prompt as a reply.
  - A prompt selected as `bubble` (one a rollback cannot reach) is still replied to.
- **`RET` on any other selected bubble sends a reply.** The daemon quotes the bubble, as before.
- **`C-s` searches the selected bubble.**
  - It opens the same read-only popup `/` uses, and runs whatever `C-s` is bound to in that buffer.
  - A selected bubble is already expanded in the webapp, because selected and expanded are one state.
