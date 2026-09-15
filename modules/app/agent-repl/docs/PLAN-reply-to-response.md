# Plan — Reply-to-a-past-response (response selection mode)

Owner-requested feature, 2026-09-15. RPC/proto changes authorized. Not yet
dispatched — this file is the spec so it survives compaction.

## Goal

Let the user reply to a PREVIOUS final response from the agent without
re-typing context: select an earlier final-response bubble, and the next prompt
is sent with that response quoted + a note, so the agent knows the new message
refers to it.

## User-facing behavior (owner's spec, verbatim intent)

- In the Emacs input window's **command mode** (evil normal state of the input
  buffer): `C-p` selects the PREVIOUS final-response bubble, `C-n` the NEXT.
  - Both START at the MOST RECENT final response.
  - Wrap around: past the oldest wraps to newest and vice-versa (as applicable).
- Only **final response bubbles** are selectable — the ones that currently wear
  the GREEN border (the final-answer rows). Selecting one changes its border
  **color to BLUE** (not green) while selected.
- The webapp SCROLLS so the selected bubble is CENTERED in the viewport —
  unless it is near the start/end of the feed such that there isn't room to
  center, in which case stop at the feed edge.
- The selected response becomes the **target** of the next prompt: the next
  prompt is prefixed with a COPY of that response plus a small note that the
  following statement is a reference to this previous response. The DAEMON does
  the prefixing; Emacs only sends the FEEDID of the selected response bubble in
  the submit request.
- **Escape** (only when in command mode):
  - 1st press → minibuffer WARNING that hitting escape again will unselect (no
    y/n confirmation, just a warning).
  - 2nd consecutive escape (from command mode) → exit response-selection mode,
    clear the selection, return the user to the BOTTOM of the feed (whatever the
    bottom is now).

## Architecture (confirmed with owner)

- **Daemon owns the selection state** (per workspace), because both Emacs
  (drives it) and the webapp (renders it) must agree — the daemon is the shared
  source of truth (as with footer/feed state).
- Emacs `C-p`/`C-n` send a "select prev/next final-response" nav RPC. The daemon
  knows the ordered final-response rows, computes the new selected feedid
  (start = most recent, wrap both ends), and:
  - PUSHES the selection to the webapp (recolor that bubble green→BLUE,
    center-scroll it, clamp at feed edges), and
  - returns/acks it to Emacs (for escape/state tracking).
- **Escape**: command-mode only. 1st → minibuffer warning; 2nd consecutive →
  daemon clears selection, webapp returns to feed bottom.
- **Submit**: `SubmitPrompt` gains an optional `reference_response_feedid`. When
  set, the DAEMON prefixes the outgoing prompt (to the shim) with a copy of the
  referenced response's markdown + a note. Emacs only sends the feedid.

### Injected prefix wording (proposed; adjust if owner wants)

```
⟢ Replying to an earlier response of yours:

<copied response markdown>

⟢ My message:

<the user's prompt>
```

## New-responses-arrive-during-selection (owner's concern)

While a selection is pending, the CURRENT turn may still be producing new
responses. The webapp must NOT auto-scroll to the bottom (that would yank the
user away from the centered selected bubble).

Plan: the daemon marks that a selection is pending (it owns the selection
state), and the webapp SUPPRESSES tail-follow / auto-scroll-to-bottom while a
selection is active. Concretely:
- The selection push carries "selection active" state; the webapp, while a
  selection is active, does NOT follow the tail (`TailFollow`) and does NOT
  auto-scroll on new rows — new rows still render/append, but the viewport stays
  on the centered selected bubble.
- When the selection is cleared (double-escape), the webapp returns to the
  bottom (re-enables tail-follow and scrolls to bottom).
- New final-response bubbles arriving during selection are still added to the
  selectable set (so `C-n`/`C-p` can reach them), but do not move the viewport.

## RPC / proto changes (additive)

1. A nav RPC to move/clear the selection, e.g. `SelectResponse` with:
   - workspace ref
   - direction: `prev` | `next` | `clear` (or explicit feedid + a clear flag)
   - Daemon computes the new selected feedid from the ordered final-response
     rows and pushes it.
2. `SubmitPrompt` (endpoint_submit_prompt.proto): add optional
   `reference_response_feedid`.
3. The webapp needs to receive the selection state — likely a new arm on an
   existing feed/workspace push (e.g. a `FeedSelection`/selection field), OR a
   dedicated push, carrying: selected feedid (or none) + "selection active" +
   "center this feedid". Decide the cleanest carrier (probably ride the feed
   watch, or a small dedicated selection push).

## System touch points

- **proto**: SelectResponse RPC; SubmitPrompt.reference_response_feedid;
  selection push/arm for the webapp.
- **daemon**: selection state per workspace (ordered final-response rows, current
  selection); compute prev/next/wrap; push to webapp + ack Emacs; on submit with
  reference_response_feedid, prefix the prompt with the copied response + note;
  mark selection-active so the webapp suppresses autoscroll.
- **webapp**: render selected bubble border BLUE (vs green); center-scroll to it
  (clamp at edges); suppress tail-follow/autoscroll while selection active;
  return to bottom on clear.
- **elisp**: input-window command-mode `C-p`/`C-n` → SelectResponse prev/next;
  escape-once warning + escape-twice clear (command mode only); include the
  selected feedid in the submit request.

## Open/naming decisions

- Selection carrier to the webapp (feed-watch arm vs dedicated push).
- Exact prefix wording (proposed above).
- Whether `C-p`/`C-n` when nothing is selected starts at most-recent (yes per
  spec) and whether they are no-ops when there are zero final responses.

## Build approach

Cross-system: proto foundation first (SelectResponse + SubmitPrompt field +
selection push), then daemon + webapp + elisp in parallel, integration + tests.
Every change gets tests (Go table/AAA no time.Sleep; webapp vitest; elisp batch
ERT). Live-verify the selection highlight + center-scroll + autoscroll
suppression in the webview after deploy.

---

# Plan — Subagent restore-from-store on resume (consolidated)

Two related, owner-reported bugs, SAME root family (resume path doesn't
reconstruct subagent state from the store). Awaiting owner "proceed" before
dispatch. Investigated by the lead + the subagent-bubble agent.

## Bug A — expanded subagent bubble shows only the parent's prompt (#5)

Receipt-backed diagnosis (subagent-bubble agent):
- A subagent's own conversation reaches the feed ONLY via a `WatchAgent` opened
  for the created agent — opened only from a LIVE spawn frame
  (`route.go` routeActivityLocked→watchSpawnedSubagentLocked) or from an item
  still in `LiveWork` on adoption.
- On RESUME, the opening `HistoryPage` is per-agent and NOT walked as live frames
  (`routeOpeningPageLocked`); the MAIN agent's page carries the parent's spawn
  Start/Success (which mint the sub-feed + draw the commission) but NOT the
  created agent's own book.
- So a subagent that finished in a prior session is neither live nor re-watched;
  its conversation is never fetched. `place(s, created)` is never invoked for its
  real frames — no ordering/unplaceable bug. Expanded bubble shows only the
  commission (parent prompt) drawn by `drawCommission`.

## Bug B — detached subagent bubble restores as "failed"

Lead investigation of the store (`~/.cache/agent-repl/store/events.db`):
- `detached_work` records ALL have a stored terminal; sampled terminals are
  `success` (kind=bash 737, kind=detached 19, all has_terminal=1).
- So "failed" is NOT what's stored — the resume path isn't reading the stored
  terminal and defaults the bubble to failed.

## Consolidated fix (proposed; lead recommended, awaiting owner go)

One "restore subagents from the store on resume" change:
1. TERMINAL: on resume, draw the detached subagent bubble's terminal from the
   stored `detached_work.terminal` (the success already there), never a "failed"
   default.
2. CONVERSATION (Bug A): option (a) — recognize spawn frames on the opening page
   and open a `WatchAgent`/replay for each created agent so its own page replays
   into `Feed{Agent: created}`. Child frames ARE in the store (its book's
   page_line frames), so this is wiring, not new capture.
   - Alternatives considered: (b) persist finished-subagent ids + re-watch on
     resume; (c) shim serves nested child pages. Lead recommends (a).

Additive proto only if needed. Spans sessionwatcher + shim `WatchAgent`.

---

# PENDING STATE AT COMPACTION (2026-09-15)

## Un-merged branch (do NOT merge until after compaction, owner ruling)
- `fix/delete-pseudo-workspaces-after-load` @ `167bb4e07`, worktree
  `<scratchpad>/wt-pseudo`. Deletes the "main" pseudo-perspective after the first
  workspace loads (fixes M-<n> off-by-one). "none"/persp-nil is UNKILLABLE
  (persp-mode refuses), so only "main" is killed; off-by-one was by ONE, so
  killing the one killable phantom should resolve it. Hooked
  `agent-repl-roster-bringup-functions` (finished + opened>0). Batch ERT 274/274.
  NEEDS: merge + deploy + LIVE-VERIFY (this is a startup-bringup event; hot-load
  won't re-run it — verify by eval'ing the deletion fn live or after a restart,
  then check `persp-names` no longer has "main" and M-1 → first real workspace).

## Awaiting owner decisions
1. Subagent restore-from-store consolidated fix above — proceed? (lead rec:
   stored terminal + option (a)).
2. Reply-to-a-past-response feature (top of this file) — architecture + prefix
   confirmed by owner; ready to dispatch (cross-system fanout).

## Landed + deployed this session (recent, for context)
- Tab-bar-blue fix (elisp roster decoder accepts RosterRowWhen active/created).
- Tree wrap: to-cap, detached-cap, indented connectors, inline-code balancing,
  first-paint (reverted — caused disappearing content), settle-instant duration.
- Incremental reveal (per-position reconcile, no per-frame rebuild).
- Overscan buffer (~5 pages) for scroll jitter.
- Prompt live-purple border keyed on data-wave.
- Per-letter full-spectrum thinking gradient.
- Subagent bubble head: yellow token, true total (incl cache reads), real clock,
  green done badge. (#5 interior still pending — see Bug A.)
- Detached-shell → canonical tool-call bubble reframe; sidebar when=last-activity
  (created shows bare age); forward-compat push-arm skew quiet-skip.
- deploy-all.sh now hot-reloads elisp by default.
