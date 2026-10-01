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
