# Roster one-shot outcome

A one-shot workspace is created, prompted, and expected to finish by asking
for its own merge. Today nothing on the tab bar says a workspace is a
one-shot, and nothing says when one ended its turn WITHOUT a merge in flight
(it gave up, was interrupted, or forgot the merge), or when the merge it
asked for failed. The owner wants the tab to carry a one-shot mark for the
workspace's whole life, and a failure mark in its place once the one-shot has
ended without a live merge.

## Context

- **Ultimate objective (owner, 2026-10-11):** a one-shot workspace's tab shows
  at a glance that it is a one-shot, and whether it has ended without a live
  merge.
- **What is being built:** a per-row one-shot fact on `frontend.v1.RosterRow`,
  resolved by the daemon, drawn by the Emacs tab bar as a glyph before the
  tab name.
- **Frontend component:** yes, the Emacs tab bar (figma→idl governs: the fact
  is a dedicated element message on the row).
- **Systems involved:** the proto (`frontend.v1`), the daemon (the sidebar
  resolver, fed by the merge orchestrator), the editor lisp (roster codec and
  tab-bar glyph). The webapp sidebar decodes the row but is not asked to draw
  the mark.
- **Non-additive changes:** none to the wire. The tab bar's glyph precedence
  in `agent-repl-status-tab-glyph` changes.

## Owner rulings (2026-10-11)

- The one-shot glyph is 🎯.
- The failed glyph is ✖️ (not ❌, which a dead session already wears).
- A permission or question gate's glyph (❗ / ❓) replaces the one-shot glyph
  while the gate is open, and the one-shot glyph returns when it closes.
- The failed glyph stays until the workspace starts WORKING again, at which
  point the one-shot glyph is restored.
- While the merge is queued the tab keeps the one-shot glyph.
- The failed state is entered when the turn resolves with no merge queue
  entry live, or when the merge queue entry fails.

## Landed changes
