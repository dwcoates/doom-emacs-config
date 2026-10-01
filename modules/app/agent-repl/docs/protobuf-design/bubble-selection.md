# One selection for every landed bubble, on the SelectFeedRow model

This rebuilds the feature first designed in
`docs/protobuf-design/response-search.md` (branch `response-search`). Read that
record for the owner's rulings and the reasoning behind them. While that
branch was in progress, `master` landed its own redesign of the same contract:
`agentrepl.v1.SelectFeedRow` replaced `agentrepl.v1.SelectResponse`, and
`frontend.v1.FeedSelection` became a `none` / `response` / `prompt` oneof,
where a selected prompt arms the rollback keys. The branch could not be
rebased onto that, so the feature is rebuilt here on `master`'s model.

## Core design principles

### Every agent-repl view of information opens through one shared opener

Carried over unchanged, and already landed on `master` (`lisp/popup.el`).
Every vertical-split popup is one Doom popup: on the right, 40% of the frame
width, and closed by `q`, which also kills its buffer. The response view opens
through it.

### The daemon is the only holder of the selection

`master` established this, and the rebuild keeps it. Everything a client draws
or acts on comes from the daemon's pushes. A sent prompt and a rollback read
the daemon's own selection.

## Owner rulings carried over (2026-10-01)

- **Selectable bubbles:** the landed bubbles the root feed draws.
  - Final responses, interim responses and thinking responses.
  - User prompts and agent-to-agent prompts.
  - Never held prompts, a response still streaming, or anything in a
    subagent's sub-feed.
- **Look:** every selected bubble kind gets the blue selection border.
- **Selected and expanded are one state,** so at most one bubble is expanded.
- **Click:** clicking a bubble selects it, with the same mechanics as `C-p` and
  `C-n`.
  - That includes the composer's selection mode.
  - Clicking the selected bubble clears the selection.
- **`C-p`/`C-n` are unchanged.** They still step through final responses.
- **Reply:** sending while any bubble is selected prepends that bubble's text.
- **Search:** `/` and `?` in the composer's command mode, with a bubble
  selected, open its MARKDOWN in a read-only `markdown-mode` buffer through
  the shared popup and run real Evil search there.
  - The webapp mirrors nothing of the search.
- **The protobuf design is the implementer's.** The owner directed: "just make
  the changes as we've discussed, proceed e2e without my input".
- **Nothing reaches `master` until everything is done** (2026-10-01).

## Decisions the implementer made where the two models meet

- **The extra kinds get their own selection arm, without rollback.**
  - `master` lets only final responses (reply) and rollback prompts be
    selected.
  - A prompt arm arms the rollback keys, and only the main agent's prompts of
    the current conversation can be rolled back to.
  - So a clicked bubble is classified by the daemon:
    - a final response becomes the `response` arm;
    - a prompt a rollback can reach becomes the `prompt` arm;
    - every other selectable bubble becomes the new `bubble` arm, which the
      next prompt replies to and the rollback keys do not act on.
  - This keeps `master`'s rule that only a rollback-reachable prompt arms the
    rollback keys, while letting every bubble kind be selected.
- **A send prepends the selected bubble's text, whatever the arm.**
  - That includes a selected prompt.
  - The owner ruled that every selected kind is a reply target.
- **A selection still ends when its row leaves the viewport,** as `master`
  does.
  - Because selected means expanded, that also collapses the bubble.
- **Emacs gets the selected bubble's text on the host push**
  (`agentrepl.v1.HostWorkspaceSelection`).
  - Emacs needs it for `/` and `?`.
  - The ack carries nothing new, since `master` already writes Emacs's
    selection from the push alone.
- **A selection change is applied and published under one lock.**
  - On `master`, a change was applied under `mu` and published after
    releasing it, so two changes in quick succession could reach the webapp
    and Emacs in the opposite order from the daemon's state.
  - The rebuild makes that reordering impossible.

## Landed changes

### 1. The selectable affordance, the bubble arm, the click, the text for Emacs

- **What changed:**
  - `frontend.v1.FeedRow.selectable` (new `frontend.v1.FeedRowSelectable`,
    tag 22) marks landed root-feed prompts and response bubbles.
  - `frontend.v1.FeedSelection.selection` gains the `bubble` arm
    (`frontend.v1.FeedSelectionBubble`, tag 7).
  - `agentrepl.v1.SelectFeedRowRequest.move` gains `bubble`
    (`agentrepl.v1.SelectFeedRowBubble`, tag 6), a click.
  - `agentrepl.v1.SelectFeedRowError.cause` gains `not_selectable`
    (`agentrepl.v1.SelectFeedRowNotSelectable`, tag 5).
  - `agentrepl.v1.HostWorkspaceSelection` gains the `bubble` arm (tag 4).
    - Each selected arm now carries the bubble's text
      (`agentrepl.v1.HostWorkspaceSelectionMarkdown`).
- **Why:** this is the owner's widened selection, expressed as additions to
  `master`'s model (see "Decisions the implementer made").
- **Consequences:**
  - Every new field and arm is additive.
  - No existing tag moved, and no client of the old shapes breaks on the wire.
  - A client that does not know the `bubble` arm sees an unset selection
    oneof. The Emacs and webapp decoders are strict, so both are updated in
    the same change.

### 2. Implementation consequences (2026-10-01)

- **Daemon:**
  - `selectableMarkdown` (`daemon/internal/resolve/feed/selection.go`) is the
    one rule for which rows are selectable and what each says.
    - Stamping rows and reading them (`SelectableText`) both go through it.
    - `SelectableText` also says whether the row is a prompt.
  - `setSelection` (`daemon/internal/server/select_feed_row.go`) is the one
    way a selection changes. It stores and publishes under `selectionMu`.
    - The topic carries the selected row's text, which the host push hands
      to Emacs.
  - A click is classified as `response`, `prompt` or `bubble`, or refused
    with `not_selectable`.
  - A sent prompt quotes whatever is selected.
    - A prompt is quoted under its own preamble: "Replying to an earlier
      prompt in this conversation".
    - It is not the agent's response, so the response preamble would be
      wrong for it.
  - Rollback still reads only the `prompt` arm.
- **Webapp:**
  - Root-feed prompt and response rows are governed by the selection
    (`webapp/src/feed/bubble-selection.ts`).
  - A click selects or clears through the shared `selectFeedRow` sender.
  - `applySelection` handles the `bubble` arm, expands the selected box and
    collapses the others.
  - Neither a click nor the auto-collapse toggles a governed box.
  - Every selected bubble kind's border is blue.
    - The open-box eggshell applies only to an open bubble that is not
      selected.
- **Emacs:**
  - The host push decodes to the kind plus the text.
  - The composer follows it through `agent-repl--input-selection-changed`.
  - `/` and `?` open the selected bubble in a read-only `markdown-mode`
    popup and run `evil-ex-search-forward` or `evil-ex-search-backward`.
- **Owner-ruled behavior this changes on `master`:**
  - A selected prompt was sent unchanged. It is now quoted.
  - Only final responses and prompts had a blue border. Every bubble kind now
    has it.
  - An expanded selected bubble was eggshell. It is now blue.
