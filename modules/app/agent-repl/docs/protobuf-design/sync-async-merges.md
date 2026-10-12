# Sync and async merges

The owner asked (2026-10-11) for the two ways an agent lands its own branch to
be named and reshaped. A SYNC merge (the `in_place` run) involves one
workspace and one shim: the requesting workspace's own agent does the conflict
resolution and the test fixing, its prompts are held until the merge ends, and
the bubble's agentic tabs stream that agent's turns. An ASYNC merge (the
`detached` run) forks a merge workspace so the requester stays free. Until now
that merge workspace was an ordinary visible workspace whose bubble tabs in the
requester linked to it instead of streaming. From here it is HIDDEN: it exists
and runs, but it appears in the tab bar and the sidebar only once it needs the
user, and its agent's turns stream into the requester's bubble tabs.

The owner asked for the work to be taken end to end without design rounds on
the proto shapes, so the shapes below follow the nearest precedents in the
contract rather than per-increment agreement.

## Core design principles

### Sync and async are the documented names of the two runs

- The principle, in the owner's terms: "sync" and "async" are known synonyms
  for the `in_place` and `detached` runs, used in the documentation; the proto
  arms keep their names.
- Consequences: no arm is renamed; comments and docs say sync/async where they
  name the runs.
- It does not claim that any other merge source gains an async run beyond the
  commit-subset pick below.

### A hidden merge workspace is revealed only when it needs the user

- The principle: an async merge workspace is absent from the tab bar and from
  the sidebar's open-workspace rows until it needs something from the user. It
  is revealed when its merge fails, when its agent asks for permission or asks
  a question, when the user interrupts its agent from the bubble, or when the
  user selects it. Once revealed it stays visible.
- Consequences: the daemon stores whether a merge workspace has been revealed;
  the roster and the tab bar leave an unrevealed, unmerged merge workspace out
  entirely (not even the hidden section draws it); a landed merge workspace is
  an ordinary closed workspace, drawn in Recently Merged and in the closed
  list whatever its revealed flag.
- It does not claim that a revealed merge workspace looks different from any
  other workspace: its feed carries no merge bubble and no merge-specific
  element, and its footer shows the ordinary statuses.

### The bubble lives only in the requester's feed

- The principle: the merge bubble of an async merge is drawn in the
  requesting workspace's feed only. The merge workspace's own feed is just
  that workspace's normal feed: its turns are ordinary turns.
- Consequences: the daemon's bubble placements shrink to one; the merge
  workspace's turns draw on its own root feed as ordinary turns AND are
  mirrored, re-identified, into the requester's bubble under the matching
  agentic tab; the agentic tabs' switch elements are retired.
- It does not claim that rows are copied anywhere else: the mirror is one
  projection with one author (the feed resolver), re-derived on every resolve.

### Async bubbles are placed, never pinned

- The principle: a sync bubble is pinned while its merge is in flight, because
  the merge is the only thing happening in that workspace. An async bubble is
  placed where it was drawn and the conversation moves on past it, so several
  async merges never stack at the bottom of the feed.

### An async merge never runs its agent in the requester

- `C-c C-k` (a turn interrupt) in the requesting workspace never stops an
  async merge's agent: that agent runs in another workspace.

## Landed changes

### `agentrepl.v1.MergeWorkspaceSourceOwnBranchDetached` gains `name` and `content`

- WHAT: a required `name` (the merge workspace's name, supplied by the calling
  agent) and a required `content` oneof: `whole_branch` (the requester's
  branch tip, as before) or `pick` (a free-text instruction telling the merge
  agent which changes of the requester's branch to cherry-pick).
- WHY: the owner ruled that the calling agent names the merge workspace,
  because it knows best what is being merged, and that the name carries no
  "-merge" decoration. The pick lets an agent land tangential work without its
  unfinished main work, and the owner ruled the cherry-picking is the merge
  agent's job, so commits that do not apply on their own are resolved rather
  than refused, and the agent may take only some hunks.
- Consequences:
  - the name is checked when the request arrives, so a taken name comes back
    to the calling agent while its turn still runs and it can retry;
  - the name is RESERVED on the durable request row from that moment until
    the fork exists, and every name allocation reads the reservations, so a
    collision at fork time is impossible rather than unlikely;
  - a pick forks the merge workspace onto the TARGET branch's tip (not the
    requester's tip) and starts it with the pick brief plus the instruction;
    the merge joins the queue only once that first turn ends, so the queue
    never waits on an agent picking;
  - the daemon judges the pick after its turn: at least one commit ahead of
    the target and a clean worktree with no cherry-pick in progress; anything
    else fails the merge, which reveals the workspace.

### `agentrepl.v1.MergeWorkspaceError.name_taken`

- WHAT: a refusal arm naming the branch the name resolves to.
- WHY: the calling agent retries with another name (owner ruling).

### `agentrepl.v1.CreateWorkspaceError.name_reserved`

- WHAT: a refusal arm for a supplied name a pending async merge has reserved.
- WHY: a supplied name bypasses disambiguation, so without the refusal a
  create could take a reserved name and the fork would collide.

### `conversation.v1.PromptOrigin.PROMPT_ORIGIN_MERGE_PICK`

- WHAT: the origin of the pick brief the daemon submits as an async pick
  merge's first turn.
- WHY: the turn is the merge's own (it draws under the pick tab), and every
  other merge-submitted turn has its own origin.

### `frontend.v1.FeedMergeTabPick`

- WHAT: a new AGENTIC tab, the first tab of an async pick merge, live,
  settled, or waiting on the user like the conflicts tab.
- WHY: the owner ruled the pick is part of the bubble as its first tab, not
  something that runs before the bubble appears, so the hidden agent's work is
  visible and interruptible from the bubble.
- Consequences: an async pick merge's tabs run pick, queue, rebasing, tests,
  fixes, committing; the bubble's clock starts when the pick starts.

### The agentic tabs' `workspace_switch` fields and `FeedMergeTabWorkspaceSwitch*`

- WHAT: removed from `FeedMergeTabPrePrompt`, `FeedMergeTabConflicts`,
  `FeedMergeTabFixes` and `FeedMergeTabPostPrompt` (tags reserved), and the
  four switch messages deleted.
- WHY: the tabs stream the merge workspace's turns now, so there is nothing
  to link to in their place.

### `frontend.v1.FeedMergeHead.workspace_switch` changes meaning

- WHAT: present once the merge workspace is VISIBLE: revealed, or landed (a
  landed merge workspace is an ordinary closed workspace). Unset while it is
  hidden, and on every sync merge.
- WHY: a hidden workspace cannot be switched to; once it is visible the head's
  button is the quickest way to it.

### `agentrepl.v1.InterruptRequest.merge_tab`

- WHAT: a target naming an agentic merge tab by its row's FeedId. It stops the
  turn running for that tab: the merge workspace's turn for an async merge
  (which also reveals the merge workspace), the requester's own for a sync
  merge. Success is `interrupted_turn`, or `nothing_running`; a FeedId that
  names no agentic tab of a live merge is the new `not_merge_tab` refusal.
- WHY: the owner ruled the interrupt in the bubble tab is how the user stops
  the hidden agent, and that interrupting opens the workspace as a failing
  merge does.

## Daemon-internal consequences (no contract text)

- `wsm`: layout 36 adds `merge_queue.source_detach_name`,
  `merge_queue.source_pick`, `workspaces.merge_revealed` (existing merge
  workspaces migrate as revealed, since they were visible), and a turn mirror
  column beside the turn address.
- The feed resolver keeps a per-workspace standing MIRROR (beside the
  standing output address): a turn the prompt queue records while it stands is
  drawn in its own workspace as usual and also, re-identified, in the mirror's
  workspace and feed under the mirror's parent row, as durable rows so a
  restart draws them without replaying the merge workspace.
- The merge workspace's sidebar marker (`frontend.v1.RosterRowMergeWorkspace`)
  stays: a revealed merge workspace is drawn as merge workspaces were.
