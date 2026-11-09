# Extra-repository workspaces drawn as repository blocks

A workspace in repository X nested under a parent of repository Y (an
"extra-repository" child) was drawn under a per-row divider whose presence
depended on the previous sibling's repository, interleaved with the parent's
own children and indented differently from them, so the parent link was
unclear. The owner settled (2026-11-09): under each parent, all children of one
other repository form ONE contiguous block headed by ONE divider naming that
repository; block rows are indented exactly as the parent's own-repository
children; blocks sit after the own-repository children; blocks are orderable
as units among themselves; rows are orderable within their block and can never
leave it. Indentation means only parent/child depth (owner clarification relayed
by the main workspace): the divider adds no indent of its own.

Mode: autonomous (pre-approved by the owner).

## Assumptions (autonomous mode)

- The figma→idl surface is `frontend.v1` (the roster); the arrangement verb
  surface is `agentrepl.v1.ArrangeSidebar`.
- Systems: daemon (resolver + wsm store + ArrangeSidebar verb), webapp
  (sidebar drawing + drag), editor lisp (wire decode + tab-bar walk).
- Non-additive: `frontend.v1.RosterOpenRow.repository_divider` is retired.
- Rule 5 (top-level blocks) needs no contract: a section draws a family in its
  ROOT's repository's section (daemon `resolve/sidebar/arrangement.go`
  `sectionRepos`), so a section's top level only ever holds its own
  repository's workspaces. The only exception is the legacy case of an open
  descendant standing in for a not-open root of another repository
  (`taborder.go` openNodes), which the hide/close family rules keep from
  arising. Recorded as a todo row, not modeled.

## Landed changes

### 1. `frontend.v1.RosterOpenRow` carries repository blocks

- WHAT: `children` now holds only the row's own-repository children;
  `repository_blocks` (field 6, after master's `children_fold` took 5; the children fold covers block rows too) holds one `RosterOpenRowRepositoryBlock` per
  other repository (`repository`, `divider`, `rows`). `repository_divider`
  (field 4) is reserved. `RosterOpenRowRepositoryDivider` now heads a block.
- WHY: one divider per block becomes a fact of the shape rather than a
  per-row flag a renderer must interpret; the block is the drag unit, keyed by
  its repository ref.
- CONSEQUENCES: every walker of open rows (daemon tab numbering, webapp
  `tab-numbers.ts`, `entries.ts`, `keyed-rows.ts`, lisp `roster.el` walk) must
  visit `children` then each block's `rows`, in that order, which is also the
  tab order. The lisp decoder `agent-repl-wire-decode-roster-open-row` must
  accept `repositoryBlocks` and stop accepting `repositoryDivider`.

### 2. `agentrepl.v1.ArrangeSidebarRequest.move_repository_block`

- WHAT: `ArrangeMoveRepositoryBlock { parent, repository, oneof to { before,
  end } }` (arm 10, after master's `by_agent` took 9) and error arm `no_repository_block` (13, after master's `main_workspace_not_sortable` took 12).
- WHY: rule 4, blocks are orderable units; the order persists per parent.
- CONSEQUENCES: the daemon stores a per-parent block order (wsm table
  `repository_blocks`, keyed `(parent_id, repo_id)`); a block never arranged
  comes after arranged ones in the repositories' stored order. A row's
  membership in a block is derived from `(parent_id, repo_id)`, so no write can
  persist a row outside its block. Moving a row out of its block (a tab move
  or swap onto another repository's sibling) stays refused by the existing
  `other_repository` arms of MoveWorkspace/SwapWorkspaces.
