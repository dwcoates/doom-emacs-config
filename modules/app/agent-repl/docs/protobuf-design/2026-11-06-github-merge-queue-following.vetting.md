# GitHub merge queue following: vetting register

Investigations owed against GitHub's live behavior. The design rests on
GitHub's GraphQL schema as introspected on 2026-11-06 (a type surface), which
says what a field is, not that GitHub sets it on every path.

## 1. Does a queue merge also record a `RemovedFromMergeQueueEvent`?

- ASSUMPTION: a pull request the queue merges records no
  `RemovedFromMergeQueueEvent`, or, if it does, the implementation's rule
  holds: removal events read in the same poll that finds the pull request
  merged are not told.
- AFFECTED: `agentrepl.v1.PullRequestActivity.merge_queue_removed`, which
  would be told beside `state_changed` merged for every queue landing. For
  the whole-queue subscription, `agentrepl.v1.GithubMergeQueueActivityMerged`
  against `agentrepl.v1.GithubMergeQueueActivityRemoved`.
- HOW TO VERIFY: in a repository with a merge queue, read a pull request the
  queue merged with
  `gh api graphql -f query='{repository(owner:"O",name:"R"){pullRequest(number:N){timelineItems(last:20,itemTypes:[ADDED_TO_MERGE_QUEUE_EVENT,REMOVED_FROM_MERGE_QUEUE_EVENT,MERGED_EVENT]){nodes{__typename ... on RemovedFromMergeQueueEvent{reason createdAt}}}}}}'`.
- STATUS: OPEN. The available `gh` login reaches no repository with a merge
  queue to read.

## 2. Is a merge group's check runs readable at `commits/{headCommit}/check-runs`?

- ASSUMPTION: `MergeQueueEntry.headCommit.oid` is the merge group commit,
  and REST `repos/{o}/{r}/commits/{oid}/check-runs` lists the runs the queue
  waits on.
- AFFECTED: `agentrepl.v1.PullRequestActivity.merge_group_check_finished`,
  which would never be told.
- HOW TO VERIFY: while an entry is AWAITING_CHECKS, read its `headCommit`
  and that commit's check runs, and compare them with the checks the queue's
  page shows.
- STATUS: OPEN.

## 3. Introspected schema facts (CONFIRMED 2026-11-06)

- `MergeQueueEntry` has `state`, `headCommit`, `enqueuer`, `position`, and
  `mergeQueue`. `MergeQueueEntryState` is QUEUED, AWAITING_CHECKS,
  MERGEABLE, UNMERGEABLE, LOCKED.
- `RemovedFromMergeQueueEvent` has `id`, `actor`, `reason`, `createdAt`.
  `AddedToMergeQueueEvent` has `id`, `actor`, `createdAt`.
- `PullRequestTimelineItemsItemType` holds ADDED_TO_MERGE_QUEUE_EVENT and
  REMOVED_FROM_MERGE_QUEUE_EVENT.
- `Repository.mergeQueue(branch: String)` takes an optional branch, and
  `Repository.defaultBranchRef` names the default.
- Method: `gh api graphql` introspection (`__type`).
