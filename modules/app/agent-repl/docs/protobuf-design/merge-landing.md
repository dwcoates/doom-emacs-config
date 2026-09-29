# Merge landing: a PR that already merged upstream, and a local merge that lands only once gated

Two defects in how the daemon lands a workspace, both reached on 2026-09-28/29.

A workspace whose GitHub PR has already merged into `origin/<default>` has no
way to say so. The `/create-or-update-workspace` skill's `merge --pr-was-merged`
writes `"pr_was_merged": true` on the command file's merge entry, meaning
"advance the local default branch to `origin/<default>` and close the
workspace, instead of merging its commits", and `create-or-update-pr` and
`check-cicd` send it once they have confirmed the PR merged. The daemon never
modelled it: the command-file reader dropped the field and ran an ordinary
local merge of commits that were already upstream, and now that the reader
decodes strictly it quarantines the file instead. Either way the footer never
honestly reaches `merged` for that workspace.

A local merge lands on the target BEFORE its test gate runs: the daemon runs
`git merge --no-ff` in the target worktree, gates that merge commit, and
relies on resetting the target on failure. A daemon exit mid-merge (lease
c8a3a664006f46c1, 2026-09-28) left an ungated merge commit on master, because
boot recovery released the orphan lease without undoing the merge. The owner's
required flow is: rebase the workspace branch onto the target tip, resolving
conflicts along the way; run the gate on the rebased branch, repairing
failures on the branch; and only once every suite passes, merge the rebased
branch into the target as a non-fast-forward merge.

## Context (step 1)

- **The daemon does not verify a PR-merged assertion (owner ruling, 2026-09-29).**
  The signal exists to prompt the daemon to take the requisite action, which
  is to pull the default branch in the repository's main worktree, not to
  re-establish that the PR merged. The callers that send it
  (`create-or-update-pr`, `check-cicd`) already confirmed the merge on GitHub
  before sending, so a daemon-side containment check would duplicate their
  confirmation.
  - Consequence: the daemon trusts the caller. A caller that sends the signal
    for a PR that did not merge gets its workspace closed and marked merged
    with its commits never landed; the guard against that is the caller's
    confirmation, not the daemon.
  - Consequence: the pull itself can still fail (a local default branch that
    has diverged from upstream, a dirty main worktree, no network). That is an
    ordinary failure of the action the signal asked for, surfaced as the
    merge's failure, and is not a verification of the assertion.

- **The merge bubble stays in the feed, collapsed to one line (owner
  ruling, 2026-09-29).** Unexpanded it is a single line, the way a subagent row
  is; expanded it looks as the merge bubble does today, tabs included. The
  agent responses its tabs carry today (conflict resolution, test
  remediation) stay in their tabs AND are also drawn in the workspace's main
  feed as ordinary turns.
  - Consequence: each such response is drawn in two places. Under figma→idl
    that is two resolved copies of one fact, one per component, never one
    shared message the client fans out.

## Landed changes
