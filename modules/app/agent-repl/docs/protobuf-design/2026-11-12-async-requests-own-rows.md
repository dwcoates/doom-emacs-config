# Async merge requests: one record each, any number per turn (2026-11-12)

A turn could ask for one async merge only. Every async request rode its
requester's one `merge_queue` row (keyed by repository and workspace) until the
requesting turn ended. So a second request in the same turn was refused
(`agentrepl.v1.CreateWorkspaceError.async_merge_requester_queued`), and
`behind=` could not name a request made earlier in the same turn
(`placement_behind_not_in_line`), because that request had not joined the
line yet. The metaprompt's rule that a task's merge and its row fixes are
dispatched together could not be followed in one turn.

The owner has standing approval for contract changes and ruled the direction
(2026-11-12): each pending async request gets its own record rather than riding
the requester's row.

Designed autonomously (no co-design was asked for).

## Assumptions

- ASSUMPTION: the objective is the ruling as stated: a requester may ask for any
  number of async merges in one turn, they join the line in the order asked
  for, and `behind=` may name one asked for earlier in the same turn. Evidence
  tier: the request itself.
- ASSUMPTION: systems involved are the daemon (`merge`, `wsm`, `server`,
  `workspace`), the webapp's agent-only arm list and the editor's create-error
  arm list. Evidence: grep of `async_merge_requester_queued` and
  `asyncMergeRequesterQueued` across the module.

## Landed changes

### `CreateWorkspaceError.async_merge_requester_queued` is retired

- WHAT: tag 36 and the name `async_merge_requester_queued` are reserved on
  `agentrepl.v1.CreateWorkspaceError`, and the message
  `agentrepl.v1.CreateWorkspaceAsyncMergeRequesterQueued` is deleted.
- WHY: no async request is refused for another the requester made, or for a
  merge of its own branch, so the refusal describes nothing that can happen.
- CONSEQUENCES: the daemon's rename of the merge queue's `already_queued`
  refusal to this arm (`daemon/internal/server/asyncmerge.go`), its entry in
  the create's agent-facing arm list (`daemon/internal/workspace/createfailed.go`),
  the webapp's agent-only arm list and the editor's arm list all drop it.
- ALTERNATIVES: keeping the arm unused, rejected because the owner rules that
  no dormant contract stays.

### `MergeQueuePlacement.behind` resolves against the requester's pending async requests

- WHAT: the comment on `agentrepl.v1.MergeQueuePlacement.behind` now states that the
  named merge may be an async merge the same requester asked for and has not
  handed off yet, and that it is then placed directly behind it. No field
  changes.
- WHY: the requester's pending async requests join the line first, in the
  order asked for, so the named one stands in line when this one is placed.
- CONSEQUENCES: a pending request of another requester is still refused
  (`placement_behind_not_in_line`): its turn may not have ended, so nothing
  orders the two.

## Storage, not contract

The ruling named the merge workspace ID reserved at request time as the key.
`merge_queue.workspace_id` is a foreign key to `workspaces(id)` with cascade,
and the merge workspace's row does not exist until its fork. Keying a
`merge_queue` row by it would mean dropping that foreign key, which is what
removes a merge when its workspace is removed. Each pending request instead gets
a row of its own in `async_merge_requests` (layout 128), keyed by the request's
own identity (`MergeSource.Detach`). `requester_id` cascades from the requester.
`merge_queue` keeps one row per workspace, and every row there is a
workspace's own merge. Layout 128 moves every pending detached request off its
requester's row, and it is breaking: the build before it would never look for
them there.
