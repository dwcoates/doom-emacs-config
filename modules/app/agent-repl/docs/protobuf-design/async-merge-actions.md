# Async merge actions

An async merge (`agentrepl.v1.MergeWorkspaceAsync`) requested from a
repository's main checkout always failed at hand-off, because the merge
workspace's create derived its landing from the requester's creation job,
which a registered main checkout never has. The same create also copied the
requester's configured merge actions into the merge workspace, which the owner
ruled unintended. The fix decides the landing once, in the merge queue, and
lets the async request name its own merge actions.

## Landed changes

### `agentrepl.v1.MergeWorkspaceAsync.merge_actions` (2026-10-14)

- **What changed.** `MergeWorkspaceAsync` gains
  `optional agentrepl.v1.CreateWorkspaceMergeActions merge_actions = 7`,
  reusing the create's message rather than declaring a second spelling of the
  same two prompts.
- **Why.** Owner ruling: a merge workspace never inherits its requester's
  merge actions. Its actions are exactly what the async request carried, and
  none when it carried none. Without a field on the request, an async merge
  could run no merge action at all.
- **Semantics.** The same as on create: `before_ws_merge` runs before the
  landing and its failure fails the run; `postprocessing_prompt` runs after the
  landing and never fails it. They run in the MERGE WORKSPACE, under its merge
  lease, never in the requester.
- **Consequences.**
  - The actions ride the queue durably between enqueue and hand-off: a new
    `merge_queue` column pair (layout 51), so a restart in between keeps them.
  - `wsm.MergeOrigin` gains `TargetDir`, `ParentBranch` and `Actions`, decided
    by `merge.orchestrator.handOffDetached()` through the one landing function
    `merge.orchestrator.detachedTarget()`. They are transport-only: the merge
    workspace's own creation job is where target and actions live durably,
    and its registry row records its parent branch, so nothing reads them back
    off a stored origin.
  - `workspace.verbs.loadMergeOrigin()` is deleted; the create no longer reads
    the requester at all.
  - One-shot merges and a workspace's own merges are unchanged: they keep the
    actions configured at the workspace's creation.
  - The command-file ingress (`commandfile.Entry`) and the
    `agent-repld merge-queue` verb gain the two prompts too, so the
    `/merge-queue` skill can pass them.
