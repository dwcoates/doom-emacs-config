# The merge head's target opens the pull request it deploys

A merge card's head reads "<branch> → <target>". For a merge whose repository
deploys through a GitHub pull request (a work repository whose finish stage
opens a PR and whose deploy adds it to the GitHub merge queue), the owner asked
that the target be clickable, opening that pull request in the account's
browser.

## Assumptions (autonomous design)

- Frontend component: yes. The branch line is drawn by
  `webapp/src/feed/merge/merge.ts`; figma→idl governs.
- Systems: proto, daemon (merge orchestrator, workspace link verbs, wsm layout
  step), webapp. No elisp consumer reads the head label.
- Non-additive: `frontend.v1.FeedMergeHead.label` (tag 2) is retired, and
  `frontend.v1.FeedMergeLabel` is renamed `frontend.v1.FeedFixGateLabel`.

## Iteration sequence

One increment: the merge head's branch line. Settled by the agent per the
skill's autonomous mode.

## Landed changes

### The branch line is source and target elements, the target carrying an optional pull request

- WHAT: `FeedMergeHead.label` (a `FeedMergeLabel` holding the composed line) is
  retired, reserved by tag and name. `FeedMergeHead.branch_line = 8`
  (`FeedMergeBranchLine`) holds `source` (`FeedMergeBranchLineSource`) and an
  optional `target` (`FeedMergeBranchLineTarget`), and the target an optional
  `pull_request` (`FeedMergeBranchLineTargetPullRequest { url }`). The arrow is
  the client's glyph.
- WHY: the owner wants the target alone to open the PR; with one string the
  client would have to split the line to find the target, which is client-side
  derivation figma→idl forbids.
- The `FeedMergeLabel` message was shared by the merge head and the fix gate
  head (`FeedFixGateHead.label`). One fact in two views gets two wrappers, so
  the fix gate keeps the same wire shape under its own name, `FeedFixGateLabel`
  (a rename only: no wire change).
- `target` is optional because the daemon's existing fallbacks
  (`merge.orchestrator.bubbleLabel`, `merge.orchestrator.abandonedLabel`) draw the
  merging workspace's id alone when what the merge lands could not be read.
  That case keeps drawing just the id, with no arrow.
- Consequences:
  - Durable merge heads recorded before this change carry tag 2. A wsm layout
    step carries the stored line onto `branch_line`, splitting at the first
    " → "; a line with no arrow becomes a source alone. Without the step,
    `resolve/feed/durable.go` would refuse every such row as carrying unknown
    fields.
  - The footer's merge row (`frontend.v1.FooterMergeRowLabel`) keeps one
    composed string; the footer resolver composes it from the head's branch line.

### Which pull request, and when it is drawn

- Evidence: the daemon holds no pull request's head branch locally
  (`prwatch.decodePR` reads `head.sha` only, and the told snapshot keeps no ref),
  so "the owned PR whose head branch is the merge's branch" cannot be read
  without new GitHub reads. The GitHub merge-queue share
  (`merge/githubdeploy.go`, `prwatch/deployments.go`) also associates a
  workspace with its owned PRs without consulting a branch.
- Rule: the candidates are the owned links naming a GitHub pull request that was
  not closed unmerged, read from the merging workspace and, for an async merge,
  the requesting workspace. Exactly one candidate is linked. With none, the
  target is plain. With several, the target is plain and the daemon records
  INFO naming them: the head does not guess.
- A finish hook stores the PR mid-merge, so every change to a workspace's links
  (`workspace.verbs.changeLinks`) restates the drawn heads of that workspace's
  merges (`merge.Orchestrator.WorkspaceLinksChanged`), the same path the topbar's
  links are republished on. No poll.
