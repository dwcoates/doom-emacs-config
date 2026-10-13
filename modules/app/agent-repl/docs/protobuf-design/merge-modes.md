# Merge modes: one-shot and async

The merge request lets an agent pick from several overlapping degrees of
freedom (own branch in place, keep it open or not, own branch detached, whole
branch or a pick, another workspace, a branch, already merged upstream). The
owner wants the API to make one thing structurally clear to agents: unless they
mean to finish the workspace — merge, then close it on landing — they use an
ASYNC merge. A sync merge exists only as a ONE-SHOT.

## Context (settled 2026-10-13)

- Ultimate objective: the daemon's merge API offers exactly two modes, a sync
  one-shot that closes the requester on landing and an async merge that never
  closes the requester, so an agent cannot block its own feed without meaning
  to finish.
- What is built: an API and skill language change; merge mechanics stay as
  they are.
- Frontend component: the merge bubble's first agentic tab is renamed from
  "pick" to "resolving"; nothing else in the webapp changes.
- Systems: proto (agentrepl.v1 merge endpoint, frontend.v1 feed merge tab),
  daemon (request validation and routing, merge-workspace creation, the
  handback, landing-time close and cleanup), Emacs lisp (wire-verbs encoder of
  the request, SPC TAB M), the merge-queue skill and its run.sh, docs.
- Non-additive changes: the `source` oneof, own-branch in-place/detached runs,
  `keep_open`, whole-branch/pick content and the `workspace` source are
  removed; `FeedMergeTabPick` is renamed.

## Core design principles

- **Sync merges are exclusively one-shots; everything else is async.** In the
  owner's terms: the only point of a sync merge is to block the feed, and the
  only point of that is to conclude the workspace once the merge lands.
  - Consequences: the sync arm always closes the requester on a successful
    landing; there is no keep-open sync merge. A failed one-shot never closes
    the requester, which is told about the failure.
  - Reopens: `MergeWorkspaceSourceOwnBranchInPlace.keep_open`, the own-branch
    `detached` run, the `workspace` source.
  - Does not claim: anything about how the queue runs, rebases, gates or lands.
- **Nothing about the merge machinery changes beyond what the owner named.**
  The named changes are: an async request names its branch, and its merge
  workspace (still a fork of the requester's conversation) starts from that
  branch's tip; an optional instruction becomes the merge workspace's first
  turn (today's pick turn, widened to any free text and renamed "resolving");
  an async request may name another workspace to close once it lands (the
  async one-shot); `cleanup_worktree` removes worktrees once it lands; and the
  requester is told when ANY merge finishes, landed or failed, except a sync
  one-shot that landed (its workspace closes).
  - Does not claim: a new verification step before queueing; the queue's own
    rebase stays the backstop.

## Landed changes

(Entries are appended as each change lands.)

### Iteration sequence

- One endpoint changes (MergeWorkspace) and one feed tab is renamed. The owner
  agreed the semantics in conversation; the request family is sketched whole as
  one increment because its arms share messages, then the tab rename.

### Decisions settled in conversation before the sketch

- One-shot sync: own branch in place, feed blocked, bubble pinned, requester
  closed on landing. `pr-merged` is a one-shot option (it closes the workspace
  and merges nothing).
- Async: `branch` REQUIRED; `instruction` OPTIONAL and only meaningful with
  the branch it operates on (several async requests in quick succession are
  told apart by their branch). With `branch` alone the branch lands as it is,
  with no resolving turn. With an instruction, the merge workspace's first
  turn carries it out, free text of any kind (rebase onto master, drop the
  requester's own commits, combine branches, fail the merge when information
  is missing).
- Async one-shot closes OTHERS, never the requester: a workspace that asks for
  an async merge is still working. It names the workspace to close on landing
  (a child, for example). It replaces the `workspace` source.
- Branch theft is made impossible, not only discouraged: an async request
  whose branch is checked out in the requester's own worktree is refused. The
  skill also tells agents never to check out the branch in the requesting
  worktree.
- `cleanup_worktree` removes, once the merge lands, the merge workspace's own
  worktree and the worktree that has the branch checked out — identified by
  branch name from `git worktree list --porcelain` at request time (a branch is
  checked out in at most one worktree, so the match is exact; matching by HEAD
  commit was rejected because every worktree freshly cut from master shares
  master's tip). Never removed: the requester's worktree, the main worktree,
  any registered workspace's worktree (open or closed: a closed workspace keeps
  its worktree so it can be reopened and shown under Recently Merged), or a
  worktree with uncommitted changes (plain `git worktree remove`, never
  forced; a refusal is logged). Branches are never deleted.
- The `pick` tab (leftmost, before queue) becomes the `resolving` tab.

### Landed: MergeWorkspaceRequest is one_shot | async; the pick tab is the resolving tab

- WHAT: `agentrepl.v1.MergeWorkspaceRequest.source` is replaced by the `mode`
  oneof of `MergeWorkspaceOneShot` (landing: own_branch | merged_upstream) and
  `MergeWorkspaceAsync` (name, branch, optional instruction, optional
  one_shot workspace to close, cleanup_worktree, compact_after). Refusals:
  `unknown_source_workspace` (9) becomes `unknown_one_shot_workspace`, and
  `branch_checked_out_in_requester` (12) is new. `frontend.v1.FeedMergeTabPick`
  becomes `FeedMergeTabResolving`, keeping field number 12.
- WHY: a sync merge exists only to conclude the workspace; everything else is
  async, so the API states the one choice an agent makes.
- Consequences:
  - Every MergeWorkspace caller moves to the new request: the daemon's server
    validation and routing (daemon/internal/server/workspaces.go,
    validate.go, requestlog_server.go), Emacs (lisp/wire-verbs.el), the
    merge-queue skill's run.sh, and their tests.
  - The async merge's worktree starts at the named branch's tip (before: the
    requester's own tip, or the target's tip for a pick). This is the one
    change to where a merge starts.
  - The requester is now handed back a failed ONE-SHOT too (before: only async
    merges were handed back).
  - The async one-shot closes the named workspace on landing; cleanup removes
    the recorded worktrees on landing. Both are new landing-time effects.
  - `conversation.v1.PromptOrigin.PROMPT_ORIGIN_MERGE_PICK` KEEPS its name:
    held prompts are stored as protojson (daemon/internal/heldingress), whose
    decoder reads enum NAMES, so renaming the value would break reading held
    prompts written before the change. Its comment describes the resolving
    turn. Likewise the daemon's internal "pick" identifiers
    (merge/pick.go, wsm.StartMergePick) keep their names: the machinery is
    unchanged. Stored feed rows (durable_feed_rows) are binary protobuf, so the
    FeedMergeTab field rename is safe on disk.
