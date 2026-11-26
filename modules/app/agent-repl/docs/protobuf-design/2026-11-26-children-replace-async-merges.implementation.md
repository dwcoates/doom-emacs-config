# Implementation: children replace async merges

The frozen contract is commit `13739537eb`, and the bindings were regenerated
in the commit after it. The decisions are in
`2026-11-26-children-replace-async-merges.md`, and the open assumptions are
in its `.vetting.md`. This document is the working checklist. Each item is
ticked when its commit lands on the branch `main-white-foreground-selection`.

## Phase A: retire the async merge from the daemon

- [x] A1. CLI and server
  - The `workspace async-merge` verb (`cmd/agent-repld/agentcli.go`,
    `forward.go`, `graph.go`, `createrefusal.go`, `refusalsentences.go`)
  - `internal/server/asyncmerge.go`, and the async arms in
    `internal/server/validate.go` and `workspaces.go`
- [x] A2. The merge orchestrator
  - `asyncrequests.go`, `pick.go`, `Request.Detached`, `DetachedRun`, and
    the async name claims
  - The off-queue causes `pick_failed` and `adopted`, the status kind, the
    requeue version, and the discard of a merge workspace
  - `RevealMergeWorkspace`, `mergeHidden`, and `asyncMergeBubble`
  - The roster queue's mode
- [x] A3. `internal/mergeworkspace` (the whole package) and
  `internal/mergenotice`'s async handback, keeping the one-shot failure
  handback
- [x] A4. wsm
  - The async request records, merge workspace records, hidden-merge
    columns, turn mirrors, owed handbacks, and reserved merge names
  - The historical DDL constants are kept for the migrations that replay
    them, and a new layout step drops the dead tables
- [x] A5. The feed resolver: `turnmirror.go`, `mergeworkspace.go`, the merge
  head's mode and switch, and the resolving tab
- [x] A6. The sidebar resolver: the async-merging marker and the queue mode.
  The vocab: `render-colors.json#async_merge_mark` and its Go and TS readers.

## Phase B: new daemon behavior

- [x] B1. Create (the subagent fork refuses loudly until the shim forks a side transcript)
  - `source_branch` on the standard and one-shot forms, cut with
    `git worktree add -b <new> <dir> <tip of source>`
  - The `subagent` context origin, using `internal/subagentmaker` to find
    the spawning session. This is subject to the vetting item.
  - `close_after`, `cleanup_source_worktree`, `compact_requester_after`,
    `deploy_hooks` and `profile_gate` on the one-shot form, recorded on the
    workspace and honored at its landing
  - The new refusals: `unknown_source_branch`, `unknown_subagent` and
    `unknown_close_after`
- [x] B2. A child's landing report to its parent is one line.
- [x] B3. The feed: a `frontend.v1.FeedChild` row in the originator's root
  feed at create time, re-published on any change to the child's roster
  status, summary, standing, links, close or removal
- [x] B4. The footer: `frontend.v1.FooterChipChildren` and
  `frontend.v1.FooterExpandedChildren` from the open children's roster rows
  and summaries
- [x] B5. The topbar: `frontend.v1.TopbarTitleChildMenu` from the children
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

## Progress notes

These notes are for whoever picks this up after a compaction. Read the git
log of the branch first.

- Done, compiling
  - A1, the CLI and server, except `internal/server/*` callers
  - `internal/wsm`
    - `wsm.LandingEffects` replaces `wsm.MergeOrigin`, as `Workspace.Effects`
      and `RegisterFacts.Effects`. Its columns are `landing_close_after`,
      `landing_cleanup_dir`, `landing_compact_requester` and
      `landing_profile_gate`.
    - Layout 221, `retiredasyncmerges.go`, drops the async tables and
      columns and the stored rows' retired fields.
    - `MergeSource` lost every `Detach*`, `Pick*`, `CompactAfter`,
      `CloseOnLanding`, `Cleanup*` and `Hooks` field.
    - The historical DDL constants stay in their old files for the
      migrations.
  - The feed resolver (turn mirror gone), the footer (the merges panel is
    replaced by `footer.SetOpenChildren` in `children.go`), and the sidebar
    (async marker and queue mode gone, and `QueueEntry` has no
    `Credited`/`Async`).
  - The prompt queue's turn mirror is gone.
- In progress: `internal/merge`
  - `asyncrequests.go` and `pick.go` are deleted, and `request.go` is
    cleaned.
  - Still to do: `api.go` (`DetachedRun`, `Names.ClaimMergeName`,
    `DetachedMerges`, `RevealMergeWorkspace`), `orchestrator.go` (asyncWaits,
    picks), `queue.go`, `terminal.go`, `rosterqueue.go`, `offqueue.go` and
    `tabs.go`.
- The decision on `internal/mergeworkspace`
  - The package is deleted.
  - The in-place handback, a failed one-shot telling itself, moves to a new
    small package.
  - A one-shot child's landing reaches its parent through the existing
    child "one update" (the `landed` workspace event, `wsm.ParentFollowedEvents`
    and the `workspace-child-notice.md` brief). That notice becomes one
    line (B2).
  - `compact_requester_after` is submitted right after that notice is
    delivered, in the workspace-notice path.
  - `close_after` and `cleanup_source_worktree` run in the merge terminal,
    read off the landing workspace's `wsm.LandingEffects`.

### Status at the second checkpoint

- Every daemon production package compiles (`go build ./...`), and the
  integration harness compiles.
- `internal/landingreports` replaces `internal/mergeworkspace`
  - `HandBack` covers an in-place merge that did not land.
  - `CoverLandings` and `DeployEnded` replace `CoverOwed` and `PayOwed`.
  - The prompt is `prompts/merge-handback.md`.
- `gitclient.WorktreeHolding` is the one branch-worktree finder, shared by
  merge and create.
- The create path now carries:
  - `CreateSpec.SourceBranch`, cut at the resolved tip
  - `CreateSpec.Effects`, with the cleanup directory resolved by
    `sourceWorktreeToCleanUp`
  - `CleanupSourceWorktree` and `ForkSubagent`
  - the server's `closeAfterOf` and `subagentSpawner`
- `agentrepl.v1.CreateWorkspaceError.name_reserved` (tag 18) is reinstated,
  re-documented as a name another create in flight claims.
- Still to do
  - Unit tests: `go vet ./...` lists the failing packages.
    - Tests of deleted subjects are deleted.
    - Their replacements are in "Replacement test specs" above, plus a
      relaunched daemon redrawing a landed child's `frontend.v1.FeedChild`
      in its parent's feed.
  - THE SUBAGENT FORK
    - `CreateSpec.ForkSubagent` is set but the fork step still forks the
      spawner's main conversation.
    - The shim needs `shim.v1.ForkSessionRequest` to fork a subagent's side
      transcript. That is a contract increment, re-entering design per the
      vetting item.
    - Until it lands, the create must refuse loudly rather than fork the
      wrong conversation.
  - `internal/workspace/retire.go` (`RetireLanded`), `wsm.Landing`
    (`landed_head`, `landed_at`), and worktreereap's landed-branch reading
    are async-only and should be retired, with their columns dropped in
    layout 221.
  - B2 to B5, and phases C, D and E.

### Status at the third checkpoint

- Every daemon package and its tests compile; the changed tests pass.
- Retired beyond the plan: the detached landing record (`wsm.Landing`,
  `RetireLanded`, landedbranch's recorded head; layout 221 drops
  `landed_head` and `landed_at`), the pick tab and span, the
  `merge_ended` stand-down, dead async refusal arms.
- B2 is in `internal/workspacesubs`: `workspace-child-landed.md` is the
  one-line parent notice, and `compactRequester` sends `/compact` after it.
- The one-shot effects are honored: close-after and cleanup in the merge
  terminal, profile_gate in `merge.Enqueue`.
- Open cleanups, all in this branch before it lands:
  - Rename `merge.DetachedOutcome` and its kinds; drop `InPlace`.
  - Retire `wsm.MergeSourceBranch` and `wsm.MergeSourceWorkspace`, which
    nothing produces now, with `merge.ArmUnknownBranch`.
  - The merge package's remaining async comments and dead code
    (`offqueue.go` name-off-queue refusal, `tabs.go` pick and reveal heads,
    `rosterqueue.go`, `transfer.go`).
- Next: B3 to B5, then phases C to E.

### Status at the fourth checkpoint

- B3 to B5 are `internal/children`: one tracker fed by the roster's registry
  (`sidebar.WithRegistrySink`), its arm moves with their colors
  (`sidebar.WithArmSink`) and the summary keeper (`summaryFan`), drawing the
  `frontend.v1.FeedChild` row (`feedid.KindChild`), the footer's open children
  and the topbar's menu of the rest. Wired late-bound in `graph.go`
  (`childrenForwarder`).
- Not drawn yet, both optional on the wire: the target's pull request
  (`FeedChildTarget.pull_request`) and the merged version
  (`FeedChildStandingMerged.version`).
- Next: phases C to E, then the open cleanups above.
