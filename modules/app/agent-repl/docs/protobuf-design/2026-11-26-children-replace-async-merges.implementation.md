# Implementation: children replace async merges

The frozen contract is commit `13739537eb`, and the bindings were regenerated
in the commit after it. The decisions are in
`2026-11-26-children-replace-async-merges.md`, and the open assumptions are
in its `.vetting.md`. This document is the working checklist. Each item is
ticked when its commit lands on the branch `main-white-foreground-selection`.

## Phase A: retire the async merge from the daemon

- [ ] A1. CLI and server
  - The `workspace async-merge` verb (`cmd/agent-repld/agentcli.go`,
    `forward.go`, `graph.go`, `createrefusal.go`, `refusalsentences.go`)
  - `internal/server/asyncmerge.go`, and the async arms in
    `internal/server/validate.go` and `workspaces.go`
- [ ] A2. The merge orchestrator
  - `asyncrequests.go`, `pick.go`, `Request.Detached`, `DetachedRun`, and
    the async name claims
  - The off-queue causes `pick_failed` and `adopted`, the status kind, the
    requeue version, and the discard of a merge workspace
  - `RevealMergeWorkspace`, `mergeHidden`, and `asyncMergeBubble`
  - The roster queue's mode
- [ ] A3. `internal/mergeworkspace` (the whole package) and
  `internal/mergenotice`'s async handback, keeping the one-shot failure
  handback
- [ ] A4. wsm
  - The async request records, merge workspace records, hidden-merge
    columns, turn mirrors, owed handbacks, and reserved merge names
  - The historical DDL constants are kept for the migrations that replay
    them, and a new layout step drops the dead tables
- [ ] A5. The feed resolver: `turnmirror.go`, `mergeworkspace.go`, the merge
  head's mode and switch, and the resolving tab
- [ ] A6. The sidebar resolver: the async-merging marker and the queue mode.
  The vocab: `render-colors.json#async_merge_mark` and its Go and TS readers.

## Phase B: new daemon behavior

- [ ] B1. Create
  - `source_branch` on the standard and one-shot forms, cut with
    `git worktree add -b <new> <dir> <tip of source>`
  - The `subagent` context origin, using `internal/subagentmaker` to find
    the spawning session. This is subject to the vetting item.
  - `close_after`, `cleanup_source_worktree`, `compact_requester_after`,
    `deploy_hooks` and `profile_gate` on the one-shot form, recorded on the
    workspace and honored at its landing
  - The new refusals: `unknown_source_branch`, `unknown_subagent` and
    `unknown_close_after`
- [ ] B2. A child's landing report to its parent is one line.
- [ ] B3. The feed: a `frontend.v1.FeedChild` row in the originator's root
  feed at create time, re-published on any change to the child's roster
  status, summary, standing, links, close or removal
- [ ] B4. The footer: `frontend.v1.FooterChipChildren` and
  `frontend.v1.FooterExpandedChildren` from the open children's roster rows
  and summaries
- [ ] B5. The topbar: `frontend.v1.TopbarTitleChildMenu` from the children
  that are not open, most recent close first

## Phase C: webapp

- [ ] C1. Remove the merge bubble's async mode, workspace switch and resolving
  tab, the footer merges panel, the sidebar async marker, and the queue mode
  suffix.
- [ ] C2. Draw `frontend.v1.FeedChild`
  - The border tone, the name in the canonical purple, and a click that
    selects the child
  - The folded short summary and the open long summary as markdown
- [ ] C3. The footer children chip and panel
- [ ] C4. The topbar summary's click menu, in the link menu's look

## Phase D: Emacs lisp

- [ ] D1. `lisp/status.el`, `lisp/wire-roster.el` and `lisp/wire-verbs.el`:
  the async-merging bracket mark and the async-merge verb

## Phase E: harness

- [ ] E1. The metaprompt
  - Every `workspace async-merge` rule becomes a one-shot child cut from the
    branch
  - Remove the hidden merge workspace
- [ ] E2. The `agent-repl-merge-queue` and `agent-repl-workspaces` skills,
  through `/create-or-update-skill`
- [ ] E3. `docs/USER-GUIDE.md`, the `AGENTS.md` files, and
  `daemon/ARCHITECTURE.md`

## Replacement test specs

Unit and integration tests, written beside the code each item changes:

- Create
  - From a source branch: the new branch is at the source's tip at create
    time, and the source branch is untouched.
  - An unknown source branch is refused `unknown_source_branch`, logged, and
    creates nothing.
  - A subagent fork finds the spawning session. An unknown subagent is
    refused `unknown_subagent`.
  - A `close_after` workspace closes when the one-shot lands, and not when
    it closes unlanded.
  - `cleanup_source_worktree` removes the source worktree on landing, and
    never a registered workspace's worktree.
  - `compact_requester_after` compacts the requester after the landing
    notice.
- Feed child row
  - Published on create, re-published on a status, summary or standing
    change.
  - The border is unset once the child is not open.
  - The name's select is unset once the child is removed.
- Footer children panel
  - Open children only. A reopened merged child is listed.
  - Waiting-on-you counts.
- Topbar child menu
  - Non-open children only, most recent first.
  - Unset with none. A removed child is absent.
- Landing report: one line.

E2E: one scenario where a workspace creates a one-shot child from its own
branch, and the child lands. The parent's feed shows the child bubble going
from an open border to merged with no border, its footer panel empties, and
its summary menu lists the child.
