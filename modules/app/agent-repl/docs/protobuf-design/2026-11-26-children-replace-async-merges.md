# Children replace async merges

An async merge is in effect a one-shot child forked from its requester's
conversation, whose only task is to land a branch that already exists. It
brings its own machinery: a hidden merge workspace, a turn mirror that streams
that workspace's turns into a bubble in the requester's feed, request records
with name reservations and their own refusals, and a second kind of queue
member. The owner wants one thing instead, the child workspace (owner request,
2026-11-26).

- A child can be cut from an existing branch. Its own branch is a snapshot
  made at create time, so what lands is fixed when the child is made. Today an
  async merge reads the named branch when its merge starts, so commits the
  requester adds meanwhile ride along or race it.
- Children are never hidden.
- Every child is drawn as a bubble in its originator's feed. The bubble does
  not mirror the child's turns.
  - The header looks like today's merge bubble header, minus the "open merge
    workspace" button.
  - The child's name is drawn in the canonical purple, and a click on it
    selects the child.
  - The child's short summary sits under the header while the bubble is
    folded, and its long summary while it is open.
  - A thin border in the child's canonical status color shows its live status.
    There is no border once the child is not open (merged, closed or nuked).
- The expanded footer lists every OPEN child: its name, short summary and
  status.
- A click on the topbar's summary drops a menu of the NON-OPEN children (merged
  or closed unmerged), drawn like the name's link menu. A row's title is the
  child's short summary, its second line is the child's name, and a click
  selects the child.
- A child's landing report to its parent is brief: one line.

## Core design principles

### Every landing is a workspace finishing itself

- **In the owner's terms**: async merges are complication for no good reason.
  A child is the one thing that lands work.
- **Consequences for the contract**:
  - `agentrepl.v1.CreateWorkspaceRequest.async_merge` is retired with every
    message only it used.
  - A merge queue entry is always some workspace's own finish, so the queue
    status's entry kind has one arm left, and the "async" vocabulary goes
    everywhere else too.
  - Whatever an async merge offered that a child lacks moves onto the
    one-shot create form: a source branch, a workspace to close once it lands,
    removing the source branch's worktree, compacting the creator, deploy
    hooks, a version increment, and gate profiling.
- **Decisions it reopens**:
  - The feed merge bubble's async mode, `frontend.v1.FeedMergeModeAsync`, and
    its workspace switch.
  - The footer's ⇄ merges chip and panel, which listed async merges.
  - The roster's `async_merging` marker, `frontend.v1.RosterRowAsyncMerging`.
  - The topology ownership rule ("a workspace is never cut from one branch and
    landed in another"), which only the async merge was exempt from.
- **What it does not claim**: a workspace still finishes in place with a sync
  merge, and that merge's bubble, with its tabs, stays in its own feed. Only
  the DETACHED form goes.

### Children are never hidden

- **In the owner's terms**: the owner never liked hidden async merges, and part
  of the point of making them children is that they are just as visible.
- **Consequences for the contract**: no create form makes a workspace born
  hidden by the queue, and no roster or feed message describes a hidden
  workspace's reveal.
- **What it does not claim**: the existing user-driven backlog (born
  backlogged intake workspaces) is a different mechanism, and stays.

### A child's bubble is a door, never a mirror

- **In the owner's terms**: keep the bubbles, drop all the turn mirroring, and
  a click on the name switches to the child.
- **Consequences for the contract**: the child bubble is one row with no
  sub-feed. Its content is the child's summaries, which the daemon already
  holds.
- **What it does not claim**: the child's own feed still draws its own sync
  merge bubble with its tabs.

### Status colors are canonical and documented

- **In the owner's terms**: there is one purple, the canonical purple, and the
  status colors are canonical colors documented in
  `modules/app/agent-repl/AGENTS.md`.
- **Consequences for the contract**: every new element that colors a child by
  its status names a color from `render-colors.json`'s closed set, resolved by
  the daemon from the same ladder the roster's row is, so the bubble's border,
  the footer row and the sidebar row can never disagree.

## Assumptions (autonomous design, step 1)

- **Objective**: one kind of workspace, the child, does all detached landing,
  and every child is visible from its originator: in the feed, the footer, and
  the topbar summary's menu.
- **What is built**: create-form changes, a new feed row, a new footer panel,
  a new topbar menu, and the retirement of the async merge everywhere.
- **Frontend**: yes, so figma-to-idl governs. The drawings are recorded with
  each increment below.
- **Systems involved**: the daemon (create, the merge queue, the feed, footer,
  topbar and roster resolvers, the CLI), the webapp (feed, footer, topbar,
  sidebar), the Emacs lisp (tab bar marker, wire verbs), the protobufs and
  `render-colors.json`, and the harness (the metaprompt, the
  `agent-repl-merge-queue` and `agent-repl-workspaces` skills, the user guide).
- **Non-additive changes**: the async merge create form, its refusals, its
  queue status kind, the feed bubble's async mode and handback badge, the
  footer merges chip and panel, the roster's async marker, and the
  `workspace async-merge` verb are all removed.
- **The originator** of a child is the workspace that requested its create
  (`agentrepl.v1.CreateWorkspaceRequest.requester`'s workspace). For a child
  that is almost always its parent. A child the user made has no originator,
  so it has no bubble, but it is still listed in its parent's footer and
  topbar menu, which key on the parent.

## Iteration sequence

Walked in this order, top-down by containment, each stage landed before the
next:

1. Topology and the create forms (`workspace_topology.proto`,
   `endpoint_create_workspace.proto`): the source branch, the subagent fork,
   and the async merge's options moving onto the one-shot form.
2. The feed's child bubble (`frontend/v1/feed.proto`).
3. The footer's children chip and panel (`frontend/v1/footer.proto`).
4. The topbar summary's children menu (`frontend/v1/topbar.proto`).
5. The retirement sweep: the create form, the feed's async mode and handback
   badge, the footer's merges, the roster marker, the merge queue status kind,
   and the refusals that only the async merge raised.

## Landed changes

### 1. Topology and the create forms

- **What changed**
  - `agentrepl.v1.WorkspaceSourceBranch` is new: an existing branch a create
    is cut from, in place of its landing target's tip. Both
    `agentrepl.v1.CreateWorkspaceStandard` and
    `agentrepl.v1.CreateWorkspaceOneShot` carry it as `source_branch`.
  - `agentrepl.v1.WorkspaceContextOrigin` gains a `subagent` arm,
    `agentrepl.v1.WorkspaceContextOriginSubagent`, which forks a subagent's
    own transcript.
  - The one-shot form takes over what an async merge offered that a child
    lacked:
    - `close_after`, with its message `agentrepl.v1.CreateWorkspaceCloseAfter`
    - `cleanup_source_worktree`
    - `compact_requester_after`
    - `deploy_hooks`
    - `profile_gate`
  - The `async_merge` form is retired (tag 10 reserved), with all nine of its
    messages and `agentrepl.v1.CreateWorkspaceAsyncMergeAccepted`.
  - Eighteen refusal arms that only the async merge raised are retired, and
    their tags reserved.
    - Three new arms replace the ones a one-shot still needs:
      `unknown_source_branch`, `unknown_subagent` and `unknown_close_after`.
  - The ownership rule in `workspace_topology.proto` now names the source
    branch as the one way other work reaches a target.
- **Why**: the owner wants one thing, the child, to land work. A child cut
  from a snapshot of a branch makes what lands fixed at create time. That
  removes, by construction, the race an async merge has: it reads its branch
  when its merge starts.
- **Rejected alternatives**
  - Putting the source branch inside `WorkspaceLandingTarget` lost: that
    message's one arm names both the cut and the landing because of the
    ownership rule. A source branch is precisely the case where the two
    differ, so it is a field of its own.
  - Carrying the async merge's `version_increment` onto the one-shot form
    lost: a one-shot names its version increment when it finishes
    (`agentrepl.v1.FinishWorkspace`), the same as every workspace that
    finishes itself. A create-time copy would be a second place to state it.
    The async merge needed one only because its finish was programmatic.
  - Carrying `instruction` lost: the one-shot's prompt is its commission.
- **Consequences**
  - The CLI's `workspace async-merge` verb goes. The `-maker` and
    `-context-fork` pairing becomes `workspace create -relationship child
    -one-shot -source-branch B -context fork|subagent ...`.
  - `daemon/internal/workspace/create.go` cuts the new branch with
    `git worktree add -b <new> <dir> <source tip>`, never touching the source
    branch.
  - The landing report a one-shot child sends its parent is the existing
    "one update" (`agentrepl.v1.WorkspaceRelationshipChild`). The owner wants
    it to be one line, which is a brief-text change in the daemon, not a
    contract change.
  - The async merge's off-queue request records, name reservations and
    `requeue`/`discard` for async names lose their reason to exist. Stage 5
    sweeps them.

### 2. The feed's child bubble

- **What changed**
  - `frontend.v1.FeedRow` gains the `child` arm (tag 27), `frontend.v1.FeedChild`.
  - The bubble is one row with no sub-feed. Its parts:
    - a head: `frontend.v1.FeedChildHead`, holding the glyph, the name, the
      target, the standing and the runtime
    - an optional summary: `frontend.v1.FeedChildSummary`, holding the short
      summary and an optional long one
    - an optional border: `frontend.v1.FeedChildBorder`, carrying a color
      name from `render-colors.json`
- **Drawing**, recorded as the layout the messages were checked against:

  ```text
  ┌─ FeedChildBorder: status color while open ───────────────────────────┐
  │ FeedChildHead: [glyph] [name] → [target] [standing badge] [runtime]  │
  │ FeedChildSummary: short while folded │ long markdown while open      │
  └──────────────────────────────────────────────────────────────────────┘
  ```

- **Why**: the owner kept the merge bubble's header look, dropped the "open
  merge workspace" button, made the name itself the clickable door in the
  canonical purple, and put the short summary under the header while folded
  and the long summary while open. A thin border in the child's status color
  shows its live status, with none once the child is not open.
- **Decisions**
  - The standing is a oneof: open, merging (waiting or merging), merged
    (with its version), closed, or removed.
    - The version rides only the merged arm, the one standing it means
      anything in.
    - The child's finer status (working, idle, waiting on you) is the
      border's color, never a second badge.
  - The name's `select` is optional. It is unset once the workspace is
    removed, because then there is nothing to select.
  - THE FOLD IS WEBVIEW-LOCAL, folded at first. Both summaries ride every
    push, so folding needs no round-trip and no rpc.
    - Rejected: a daemon-held fold like `agentrepl.v1.FoldMergeBubble`'s.
      That fold exists because merge bubbles have owner rulings about their
      defaults across reloads. A child bubble has no such ruling, so a
      durable fold would be an rpc with nothing to decide.
  - The border's color is a color NAME from `render-colors.json`'s `colors`,
    resolved by the daemon from the same status ladder the roster's row
    projects, as `frontend.v1.FeedPageErrorHeadline.tone` already does. So
    the bubble, the footer row and the sidebar row can't disagree.
  - The long summary is markdown, bulleted per sentence by
    `textline.Bullets`, the same rendering as the topbar's hover.
  - The version and target are duplicated as `frontend.v1.FeedChild*`
    element messages, not shared with `frontend.v1.FeedMerge*`, per
    figma-to-idl's "one fact in two views is two dedicated wrappers".
- **Consequences**
  - The daemon's feed resolver must publish a child row into the originator's
    feed when the create is accepted. It must then re-publish the row on
    every change to the child's status, summary, standing, links, close or
    removal, which means the feed resolver now subscribes to other
    workspaces' facts.
  - A child the user created (no requesting workspace) has no bubble.

### 3. The footer's children chip and panel

- **What changed**
  - `frontend.v1.FooterLiveWorkChips.merges` (tag 7) and
    `frontend.v1.FooterExpanded.merges` (tag 8) are retired, with the merges
    chip and panel messages.
  - The ⇄ chip is now `frontend.v1.FooterChipChildren`, which counts the open
    children and carries a waiting-on-you count.
  - Its panel is `frontend.v1.FooterExpandedChildren`, one
    `frontend.v1.FooterChildRow` per OPEN child. Each row has:
    - the name, in the canonical purple
    - the status, as a word and a color name
    - the short summary, optional
    - a select target
- **Drawing**:

  ```text
  ┌─ FooterExpandedChildren ───────────────────────────────────────────┐
  │ ⇄ [FooterChildRowName] [FooterChildRowStatus] [short summary]      │
  │ ⇄ ...                                                              │
  └────────────────────────────────────────────────────────────────────┘
  ```

- **Why**: the owner wants the expanded footer to hold every OPEN child,
  showing what the summary menu shows: the short summary and the status. The
  topbar summary's menu holds every child that is NOT open. A child reopened
  after it merged is open, so it is in the footer.
- **Decisions**
  - A row selects the child rather than jumping to its bubble. The bubble is
    itself only a door, so the row opens the door directly.
  - The status is a word plus a color name, the same pair the sidebar row
    draws, resolved from the one ladder.
- **Consequences**: the footer resolver needs every child's roster status and
  summary, not just facts about its own workspace.
