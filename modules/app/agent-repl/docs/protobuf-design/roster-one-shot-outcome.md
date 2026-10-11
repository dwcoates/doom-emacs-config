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

## Owner rulings (2026-10-11, second round)

- The tab bar draws NO merge glyph for a merge in flight or landed: the
  purple tab color already says a merge is in flight, and a landed merge
  shows nothing. Merges are reflected by a glyph only when they FAIL.
- The failed glyph clears the moment the row reaches the `thinking` status,
  and the one-shot glyph returns. What `/clear` or `/compact` do is
  irrelevant to the rule: only reaching `thinking` clears it.
- The webapp sidebar draws ✖️ by the same rule, and never draws the one-shot
  glyph.

## Verification evidence

- One-shot is recorded on the CREATION JOB, not the workspace row
  (`daemon/internal/wsm/types.go:226`, column `creation_jobs.one_shot`). The
  creation job is deleted only by `Forget` and repository retirement
  (`daemon/internal/wsm/workspaces.go:648`, `daemon/internal/wsm/retire.go:78`),
  so it lives as long as the workspace. Evidence tier: existing code.
- Two creation paths set it: the `CreateWorkspaceRequest.one_shot` form arm
  (`daemon/internal/server/workspaces.go:124`, which `SPC` `agent-repl-create-oneshot`
  sends) and the command file's `one_shot` field
  (`daemon/internal/commandfile/create.go:31`). Evidence tier: existing code.
- The `create-or-update-workspace` skill's `--one-shot` does NOT set the
  command file's `one_shot` field: it appends its own wrap-up instruction to
  the prompt (`~/.claude/skills/create-or-update-workspace/run.sh:590`), so
  the daemon never learns those workspaces are one-shots. Evidence tier:
  existing code.
- A merge requested during a turn is recorded as `wsm.MergeRequested` and
  admitted only after the turn ends; the sidebar resolver's
  `footer.MergeFacts` has no requested state, so a status-arm rule would see
  `done` with no merge for at least one render. Evidence tier: existing code.

## Landed changes

### 1. `frontend.v1.RosterRow.one_shot` (2026-10-11)

- **What:** `RosterRow` gains `optional RosterRowOneShot one_shot = 59`.
  `RosterRowOneShot` is a `oneof outcome` of two empty arms, `armed` and
  `failed`. Absent means the workspace is not a one-shot.
- **Why:** the owner wants a one-shot's tab to say it is a one-shot, and to
  say when it ended without a live merge, and the editor draws only what the
  daemon resolves.
- **Agreement:** the owner agreed the shape in prose (one optional field,
  armed and failed arms) and asked for implementation without a separate
  sketch review.
- **Consequences:**
  - The daemon reads one-shot off the creation job, so `wsm.Workspace` gains
    a read-only `OneShot` filled from `creation_jobs` in the shared select
    list, and a read-only `MergePending` (a `merge_queue` row stands in any
    state) for seeding the outcome after a restart: the boot publishes the
    roster BEFORE it recovers merges (`daemon/internal/boot/sequence.go`,
    `BindViews` before `recoverMerges`), so the seed cannot read in-memory
    merge facts.
  - The merge orchestrator tells the roster when a request is RECORDED
    (`Sidebar.SetMergeRequested`), so a one-shot that asked for its merge is
    `armed` on the very push that ends its turn. No client sees the merge any
    earlier: the request fact feeds only this outcome.
  - `failed` is entered on a turn end with no merge requested, queued or
    running, and on a merge failing; it clears when a turn reaches
    `thinking`, when a merge is requested, or when one is queued or running.
  - The tab bar stops drawing the in-flight and landed merge glyphs; a failed
    merge on any workspace draws ✖️ (the same mark as a failed one-shot).
