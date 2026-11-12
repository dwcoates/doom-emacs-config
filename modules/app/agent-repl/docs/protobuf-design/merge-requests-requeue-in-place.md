# A merge's request outlives its place in line

A merge that left the agent-repl merge queue without landing lost everything
that described it: its queue row was the only record of its request (version
part, changelog line, hooks, cleanup flags, placement), and every exit deleted
it. An async merge's helper workspace was closed on an evict or a dequeue, and
left open but unreferenced on every other exit, still holding its name, so a
re-request needed a new name and a new workspace. On 2026-11-12 thirty merges
evicted at the cold gate had to be re-requested from values scraped out of
transcripts. The owner's design (2026-11-12): a merge's request lives until it
lands or is explicitly discarded, so any merge that left the queue without
landing can be put back in line as itself, never by creating a new workspace.

Design mode: autonomous (contract changes pre-approved by the owner).

## Core design principles

- **A request ends only by landing or by an explicit discard** (owner,
  2026-11-12).
  - Consequences: every exit from a queue short of a landing keeps the request,
    with its cause. No exit closes an async merge's merge workspace. Requeue
    and discard are the only two verbs over a request that left.
  - Reopens: the close-on-release of an evicted or dequeued async merge's
    merge workspace (`merge.orchestrator.exitClosure`, 2026-11-12 on master),
    which is retired.
  - Does not claim: that a request survives its workspace's close. Closing
    (killing, nuking) the merge's workspace is itself an explicit end, so the
    record goes with the workspace in the close's own transaction.

## Assumptions (autonomous mode)

- Objective: requeue a merge that left its queue as itself.
- Frontend: yes, the sidebar's Merge Queue section (figma→idl governs).
- Systems: the daemon (wsm store, merge orchestrator, server, the
  `agent-repld` command line), the webapp sidebar. The editor lisp draws the
  sidebar through the webapp and is untouched.
- Non-additive changes: the removal of close-on-exit from the queue-exit
  store writes (`wsm.RemoveMergeQueueEntry` and `wsm.DropAdmittedMerge` lose
  their `closes` parameter). No wire field is removed.

## Iteration sequence

Default sequence: protocol (ConnectRPC, the existing `agentrepl.v1.AgentRepl`
service) → endpoint inventory (`RequeueMerge`, `DiscardMerge`; the status
read and the async-merge create gain arms) → conventions (the canonical
`result { success; error; }` oneof, carried forward) → each endpoint's shapes →
the sidebar component.

## Landed changes

### Naming: "off the queue", not "out of line"

The owner's brief called the new state OUT OF LINE. That phrase already names
something else in this codebase: a merge repairing inside its queue, holding
or awaiting the fixing-tests or resolving-conflicts slot
(`agentrepl.v1.MergeQueueStatus.entries`, "every merge out of line"). Reusing
it would make "a merge out of line" mean two opposite things (still in the
queue, repairing; and gone from it). The new state is named OFF THE QUEUE
everywhere: `off_queue` fields, `MergeQueueStatusOffQueue`, the refusal
`still_queued`. The store table is `off_queue_merges`.

### RequeueMerge (`endpoint_requeue_merge.proto`)

- WHAT: a new rpc taking the merge's workspace ref, a required
  `agentrepl.v1.MergeQueuePlacement`, an optional version increment and the
  requester. Success answers the merge's place in line. Typed refusals:
  unknown workspace, ref mismatch, `unknown_merge`, `still_queued`,
  `branch_gone`, `already_landed`, `version_increment_required`,
  `version_increment_unexpected`, `version_already_recorded`,
  `placement_behind_not_in_line`, `workspace_closed`.
- WHY: the owner's ask, step 2. The request is named by WorkspaceRef, as
  `UpdateMergeQueueEvict` names its merge, so the sidebar echoes a ref and the
  command line resolves the name `merge-queue status` lists to it.
- `version` is accepted only for a request that recorded none (an adopted
  one). Alternative rejected: letting `version` override a recorded one,
  because a requeue would then silently change what a merge lands as.
- `already_landed` is answered from the workspace's closure: a workspace with
  no request standing whose closure is `wsm.CloseByMerge` landed it. No landed
  tombstone is kept.
- `workspace_closed` exists for the race of a close after the caller named the
  merge; a record never outlives its workspace's close.

### DiscardMerge (`endpoint_discard_merge.proto`)

- WHAT: a new rpc taking the merge's workspace ref and requester. Success says
  which kind was discarded; an async discard reports whether the worktree its
  request asked to clean up was removed.
- WHY: the owner's ask, step 3: the only path that closes an async merge's
  merge workspace.
- The cleanup removal follows the landing's own rule
  (`merge.run.cleanupLanded`): git's clean door, a dirty worktree kept, and NO
  branch deleted. The brief said "worktree/branch per its cleanup flags"; the
  cleanup flag names a worktree only, and deleting an unlanded branch would
  destroy work that never reached any target. Recorded as a deliberate
  reading of the brief.
- A merge still in line is refused (`still_queued`): discard is not a
  shortcut around a dequeue.

### Status (`merge_queue_status.proto`)

- WHAT: `MergeQueueStatus.off_queue`, each a `MergeQueueStatusOffQueue`
  (workspace ref, name, branch, kind, cause, `left_at_ms`). The cause is a
  oneof of empty arms, one per exit: evicted, dequeued, failed,
  restart_failed, daemon_shutdown, pick_failed, finish_failed,
  ahead_did_not_land, hidden_at_cold_gate, adopted.
- `hidden_at_cold_gate` is the cause the waiting `merge-queue-cold-gate`
  branch introduces; it is declared now so that branch's exit maps onto the
  wire once both have landed.
- `workspace_closed` has no arm: such a record is never kept.

### Async-merge name refusal (`endpoint_create_workspace.proto`)

- WHAT: `CreateWorkspaceError.async_merge_name_off_queue` (tag 62) naming the
  off-queue merge's workspace, name and cause.
- WHY: the owner's ask, step 4. Checked before the generic
  `async_merge_name_taken`, which the same name would also trip (the merge
  workspace exists under it), so the caller learns to requeue instead of
  renaming.

### Sidebar (`frontend.v1.RosterRepoMergeQueue.off_queue`)

Drawing, under the repository's Merge Queue group, below the queue:

```text
┌─ RosterRepoMergeQueue ───────────────────────────────┐
│  head / waiting / repair slots (unchanged)           │
│  off the queue                                       │
│  ┌─ RosterMergeQueueOffQueue ───────────────────────┐│
│  │ name (Subject)    cause (Cause)    [↺]  [✕]      ││
│  └──────────────────────────────────────────────────┘│
└──────────────────────────────────────────────────────┘
```

- `RosterMergeQueueOffQueue` holds the subject (as every row), the cause
  element (daemon-resolved words), and the requeue and discard controls, each
  carrying the merge's OWN workspace ref (an async merge's merge workspace,
  never the requester the subject credits) as the echo the rpc takes.
- A queue listed only for off-queue merges carries no head, as one listed
  only for GitHub entries.
- The sidebar's requeue is the user's, placed at the back; the user's request
  needs no version increment (`merge_version_increment.proto`: one the user
  asks for may carry it and lands untagged without it).

### Store: `off_queue_merges` (persistence, not wire)

- The persistence model is a table keyed by workspace (one request per
  workspace, as `merge_queue`), holding the queue row's request columns
  verbatim plus `cause`, `left_at` and `adopted`. The mapping owner is
  `wsm`: `leaveQueueInTx` moves a row out, `requeueInTx` moves it back, both
  by one shared column list, so the two shapes cannot drift.
- Every queue exit goes through `wsm.leaveQueueInTx`: a cause of
  `wsm.MergeLeftLanded` deletes the request, every other cause moves it off
  the queue. `markClosedInTx` deletes a closed workspace's record.
- The adoption migration inserts a record for every open merge workspace with
  no queue row and no record, cause `adopted`, no request values.

## Consequences

- `wsm.RemoveMergeQueueEntry` and `wsm.DropAdmittedMerge` no longer take a
  closure; their decorators (`wsedges`, `cachelapse`, `merge.rosterQueueDB`,
  `merge.queueEventsDB`) lose the close plumbing.
- `merge.orchestrator.exitClosure` and `closedOnExit` are deleted.
- An evicted async merge's merge workspace stays open and hidden until it is
  discarded; this is the owner's stated preference.
- A request whose async hand-off never forked its merge workspace
  (`detached_fork_failed`) never stood in a queue under a workspace and is not
  kept: there is no workspace to requeue it as. Its requester is told, as
  before, and asks again.

### RequeueMerge takes a model (parent workspace's request, 2026-11-12)

- WHAT: `RequeueMergeRequest.model` (optional `conversation.v1.AgentModel`)
  and the refusal `not_in_catalog`; `RequeueMergeSuccess.position` became
  optional, unset while the daemon drains (the request stays recorded and
  joins the line at the next boot).
- WHY: a requeued merge whose session parked cold would otherwise need a
  separate SetModel, which a cold conversation refuses
  (`SetModelError.cold_gate`); the model must take effect as the session
  resumes, so the conversation is read once, on the new model.
- HOW: every session start names the session's recorded model
  (`workspace.Fleet.startSessionRequest`), so the requeue records the model
  in the same store transaction that puts the merge back
  (`wsm.setSessionModelInTx`, now the one write `SetSessionModel` also uses).
- THE CATALOG IS THE LOGIN'S RECORDED ONE (`wsm.AccountModelCatalog`, checked
  by `workspace.Verbs.RequireCatalogModel`), not the topbar-served one
  `SetModel` checks: a hidden merge workspace serves no selector.
- Accepted cost: a session that is live (not cold) when requeued switches at
  its next start, not at once; the target case is the cold session.

### Webapp

- `drawRosterMergeQueueOffQueue` (`webapp/src/sidebar/merge-queue-view.ts`)
  draws each row after the queue's own lines: name, cause label, `↺`
  (RequeueMerge at the back, as the user) and `✕` (DiscardMerge, as the user).
  Refusal words live in `webapp/src/rpc/merge-queue-refusal.ts`.

### Correction: a closed workspace's exit ends its request (2026-11-12)

The first landing kept a request for every cause but a landing unless the
workspace already read closed. A running merge abandoned because its
workspace closed (`merge.CauseWorkspaceClosed`) is torn down before the close
is recorded, so it was kept under `workspace_closed`, a cause with no off-queue
words, and the roster publish panicked in `merge.OffQueueWords`. Root cause:
the fate of each exit cause was implied by "not landed" rather than declared.
Now `wsm.EndsRequest` names the causes that end a request (a landing,
`wsm.MergeLeftWorkspaceClosed`, `wsm.MergeLeftToReturn`), and
`merge.TestEveryExitCauseKeepsOrEndsItsRequest` reads every cause constant of
the package and requires exactly one fate for each: ends, kept under a declared
arm and words, or no queue exit.
