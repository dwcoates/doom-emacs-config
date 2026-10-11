# Detached merges run in merge workspaces

Today a merge runs in the workspace that asked for it. Its conflict
resolution and test fixing are turns of that workspace's own session, and
every prompt the user sends is held until the merge ends. The owner wants a
merge to run on its own, so the agent can put its work in the merge queue
and the user keeps working in the same conversation meanwhile. The merge's
agentic work runs in a fork of the conversation, and when the merge ends the
fork's outcome comes back into the main conversation.

## Context

- Objective (owner, 2026-10-11): a merge request forks the conversation into
  a merge workspace that carries the merge, so the user continues in the main
  conversation while the merge runs, and the merge's outcome surfaces back in
  the main conversation when it ends.
- What is built: a detached run mode for a workspace's merge of its own
  branch. The request creates a MERGE WORKSPACE, a forked workspace (forked
  conversation, its own worktree on the branch's tip at request time) that the
  merge queue lands on master.
- Frontend component: yes. The merge bubble in the parent's feed, the sidebar
  row of the merge workspace, and a handback marker in the parent's feed.
- Systems involved: the daemon (merge orchestration, workspace creation and
  fork, roster, feed resolver, prompt queue, cleanup), the webapp (merge
  bubble head, sidebar row, handback badge), the Emacs editor lisp and the
  merge-queue skill (the merge request), and the shim only through the
  existing fork path.
- Non-additive changes: `agentrepl.v1.MergeWorkspaceSourceOwnBranch.keep_open`
  moves into the in-place run arm, so every caller of the own-branch merge
  changes (editor lisp, the merge-queue skill's `run.sh`, the webapp if it
  sends one, and tests).
- Owner ruling on process (2026-10-11): the owner gives no input on protobuf
  shape for this change. The orchestrator decides every shape under the style
  guidelines and records the reasoning here.

## Core design principles

### A merge workspace is a forked workspace, shown as a child, that lands on master

- In the owner's terms: "we can implement as forked workspaces under the
  hood ... regard them as child workspaces as well ... they'd be merging into
  MASTER, not into the workspace of which they're parents. I still think
  that's a nice UX."
- Consequences for the contract: the sidebar nests the merge workspace under
  its parent through the existing `frontend.v1.RosterRow.children`, so the
  roster needs no new nesting field. The daemon's workspace table already
  records both a parent workspace and a parent branch; a merge workspace
  records the requester as its parent and the requester's own target branch
  (master) as the branch it lands on. The row says it is a merge workspace
  through its own marker element, because the nesting alone would read as an
  ordinary child that merges into its parent.
- Decisions it reopens: the `MergeWorkspace` header comment's "No workspace
  is ever created for a merge", which holds only for the in-place run now.
- What it does not claim: a merge workspace is not a child for merge
  targeting. It never merges into its parent's checkout, and its parent is
  never closed or held by it.

### The merge bubble is unchanged except for the switch to its merge workspace

- In the owner's terms: the merge bubble stays pinned until it concludes and
  then settles, exactly as today; the only addition is a prominent button in
  its header that switches to the merge workspace, and that button is always
  there on a detached merge's bubble ("it's invariant").
- Consequences: `frontend.v1.FeedMergeHead` gains one element, present for the
  whole life of a detached merge's bubble. Its click is the existing
  `agentrepl.v1.SelectWorkspace`, which already revives a closed workspace,
  so a landed (closed) merge workspace is re-opened by the same call. No new
  verb.
- What it does not claim: pinning, tabs, folding, durations and the test gate
  are untouched.

### Landed merge workspaces are kept, and every merge workspace is cleaned up after 15 calendar days of inactivity

- In the owner's terms: clean them after 15 days of inactivity, calendar
  inactivity.
- Consequences: a landed merge closes its merge workspace but keeps its
  worktree and conversation so the switch keeps working. A merge workspace
  with no activity for 15 calendar days is removed by the daemon. The switch
  element then states the removal instead of offering a switch, because the
  bubble outlives the workspace it names.
- What it does not claim: the cleanup applies to merge workspaces only, not
  to ordinary forked or child workspaces.

## Iteration sequence

The owner ruled out shape input, so the orchestrator settled the order
itself: the request first (it decides whether a merge workspace exists at
all), then the conversation-level handback origin (the leaf the feed reads),
then the frontend elements from the outermost surface inward: the merge
bubble head, the handback feed row, the sidebar row. Each lands as one
increment.

## Landed changes

### 1. The own-branch merge chooses where its agentic work runs

- What changed: `agentrepl.v1.MergeWorkspaceSourceOwnBranch` holds a `run`
  oneof with two arms, `in_place` (today's behavior, carrying `keep_open`) and
  `detached` (a merge workspace carries the merge). Tag 1 (`keep_open`) is
  reserved on the source message. The file's header comment describes both
  runs.
- Why: `keep_open` means nothing for a detached merge, because a detached
  merge never closes the requester; under the adjacent-exclusivity rule the
  mode that selects it is a oneof, with `keep_open` confined to the arm where
  it means something.
- Why the other sources get no detached arm: merging another workspace's
  branch or a bare branch already runs in another worktree, and the owner's
  case is the agent merging its own work while the user keeps going. A
  detached run for those sources has no stated need.
- Why `MergeWorkspaceSuccess` stays empty: the merge workspace is created
  when the requesting turn ends (the fork must hold the whole turn that asked
  for the merge), so the enqueue answer cannot name it yet. It appears on the
  roster stream.
- Consequences: every sender of an own-branch merge changes. The editor
  lisp's merge verbs and the merge-queue skill's `run.sh` (`--enqueue-own`,
  `--keep-open`) choose an arm. The daemon refuses an own-branch request with
  no `run` arm set as invalid.

### 2. A merge handback is its own prompt origin

- What changed: `conversation.v1.PromptOrigin.PROMPT_ORIGIN_MERGE_HANDBACK`
  (31): the daemon's prompt into the parent workspace's session reporting
  how a detached merge ended.
- Why: the parent agent must learn what happened to the work it handed off,
  and a durable origin lets replay route the row to the handback badge
  rather than a prompt bubble, exactly as the resolver routes other
  daemon-generated prompts by origin.
- Consequences: the daemon submits it through the prompt queue's one
  delivery path, so it is held behind a running turn like any submission. The
  prompt's text (what landed, or what failed and where the merge workspace
  is) is daemon-composed and is not part of the contract.

### 3. The merge bubble head carries the switch to its merge workspace

- What changed: `frontend.v1.FeedMergeHead.workspace_switch` (5, optional),
  `frontend.v1.FeedMergeWorkspaceSwitch` with a label element and an
  `availability` oneof: `available` (carrying the target workspace ref) or
  `removed` (when the workspace was cleaned up).
- Why: the owner's invariant button. Presence is the placement: unset on an
  in-place merge, set for the whole life of a detached merge's bubble. The
  target ref lives only in the `available` arm because a removed workspace
  has nothing to switch to.
- Consequences: the webapp draws the button in the bubble head and calls
  `SelectWorkspace` with the ref, as the merge queue tab's entries already
  do (`webapp/src/feed/merge/queue.ts:160`). The daemon re-pushes the head
  when the cleanup removes the workspace.

### 4. The parent's feed marks where a merge handback reached it

- What changed: `frontend.v1.FeedRow.merge_handback` (23) and
  `frontend.v1.FeedMergeHandbackBadge` with its label element.
- Why: the handback prompt is the daemon's words, not the user's, so it is
  drawn as a marker in the flow (as `subagent_handback` is), and the merge
  itself is read in the bubble. A dedicated message rather than the subagent
  badge: one fact in two components is two wrappers.
- Consequences: the feed resolver routes a `PROMPT_ORIGIN_MERGE_HANDBACK`
  prompt row to this arm on the live plane and on replay.

### 5. A merge workspace's sidebar row says it is one

- What changed: `frontend.v1.RosterRow.merge_workspace` (58, optional) and
  `frontend.v1.RosterRowMergeWorkspace` with its label element.
- Why: the row is nested under its parent like a child, but it lands on
  master; the marker states that, so the nesting is not read as "merges into
  its parent".
- Consequences: the roster resolver sets it from the daemon's own record of
  the workspace being a merge workspace, which the workspace table gains (an
  internal column, not part of the contract).
