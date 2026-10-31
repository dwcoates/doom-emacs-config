# GitHub event subscriptions: vetting register

1. **Assumption:** a pull request's `GET pulls/{n}/comments`,
   `issues/{n}/comments` and `pulls/{n}/reviews` answer `304` to
   `If-None-Match` when nothing changed, and a comment edit changes the ETag.
   **Affected:** the poll cadence and rate budget; edit detection
   (`agentrepl.v1.PullRequestActivityEdited`). **Verify:** read each list with
   `gh api -i`, repeat it with `If-None-Match`, edit a comment on a scratch
   PR, and repeat again. **Status:** PARTLY SETTLED 2026-10-30. On
   `ChessCom/explanation-engine` PR 8047 all three lists answered `304` to a
   repeated conditional read, and so did the timeline, with the rate limit
   unchanged. The edit case is still OPEN, because checking it means writing
   a comment.
2. **Assumption:** a check run's `name` and `conclusion` from
   `GET commits/{head}/check-runs` are enough to tell a check briefly, and
   GitHub's conclusions map onto `agentrepl.v1.PullRequestCheckConclusion`'s
   arms (`stale` is left unnamed). **Affected:**
   `agentrepl.v1.PullRequestActivityCheckFinished`. **Verify:** read the check
   runs of a recent `ChessCom/explanation-engine` PR. **Status:** SETTLED
   2026-10-30, with one finding. PR 8047's head has 57 runs (30 success, 23
   skipped, 4 failure), all of whose conclusions map onto the arms. Names
   repeat (`one-canonical-pr` twice, from a re-run), so the poller tells the
   latest run per name. 57 one-line items per push is the cost of the owner's
   "title and result" ruling as written. The brief may group skipped runs into
   one count line without any schema change, because the count is the
   brief's rendering, not a field.
3. **Assumption:** a merge through GitHub's merge queue sets `merged_at` on
   the PR, so `agentrepl.v1.PullRequestStateMerged` ends the subscription.
   **Affected:** subscription end. **Verify:** read a queue-merged
   `ChessCom` PR. **Status:** SETTLED 2026-10-30. PR 8023
   reads `state=closed` with `merged_at` set (12:11:00Z). The daemon maps
   closed with `merged_at` to `PullRequestStateMerged` and closed without it
   to `PullRequestStateClosed`.
