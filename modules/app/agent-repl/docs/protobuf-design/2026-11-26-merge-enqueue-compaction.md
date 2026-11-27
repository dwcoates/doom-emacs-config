# Merge enqueue compaction

A workspace whose own merge enters the merge queue is compacted for remediation and switched to the latest Sonnet first, so the keep-alives while it waits and every remediation turn (a failing test gate, a conflict) run on a small Sonnet context. When the merge ends without landing, the workspace goes straight back to the model it ran before, with no second compaction.

## Assumptions (autonomous design)

- Every merge is a `FinishWorkspace` of the workspace's own branch. An async merge is a landing child, which finishes itself the same way, so one hook at the queue's enqueue covers both. Verified from `endpoint_finish_workspace.proto` and `merge/request.go`.
- A deploy remediation's finish lands directly (`FinishRemediation`) and is excluded, since the deploy mediator keeps its own model and compaction rule.
- `sonnet` is the family alias the served catalog carries (`workspace.WorkDefaultLaunchModel`), so it names the latest Sonnet without pinning a version.

## Landed changes

### `conversation.v1.PromptOrigin.PROMPT_ORIGIN_MERGE_ENQUEUE_COMPACTION`

- WHAT: one new origin arm, value 56, spend cause compaction.
- WHY: the compaction is the daemon's own, and its spend and its drawing must read as the merge's enqueue compaction rather than as the landing child's requester compaction (`PROMPT_ORIGIN_MERGE_COMPACT_AFTER`), which compacts a different workspace at a different moment.
- CONSEQUENCES:
  - The model switch and its restore need no wire change: both go through the prompt queue's existing set-model session act, and the model to restore is a daemon-store fact, not a protocol one.
  - Every switch over the origins that lists `PROMPT_ORIGIN_MERGE_COMPACT_AFTER` as a context cut lists the new arm too (`promptorigin.go`, `promptqueue/deliver.go`, `resolve/feed/prompt.go`).
  - The enqueue now waits for the compaction's turn to end before the merge takes its place in line, so a merge's place is taken after the compaction rather than at the finish.
  - The model to restore lives in the daemon store as `workspaces.merge_restore_model` (layout 224), held before the compaction and kept when already held, so a request re-run after a restart never records Sonnet as the model to return to.
  - The compaction is `merge.Deps.CompactForQueue`, which answers only once the compaction's turn has ended; the switch and the restore are `merge.Deps.SetModel`, the workspace verbs' queued set-model act.
