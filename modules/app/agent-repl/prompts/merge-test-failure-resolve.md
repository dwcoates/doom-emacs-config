<!-- used by: daemon internal/workspace/merge/testfailureresolver.go (TestFailureResolution.Prompt); placeholders: {{failing_commit}}, {{source_branch}}, {{target_dir}}, {{failure_tail}} -->
Commit {{failing_commit}} from branch {{source_branch}} was just rebased onto the merge target in the worktree at {{target_dir}}, and the repository's test suite now FAILS there.

That worktree is a TEMPORARY REBASE WORKTREE, not the merge target and not your own workspace. The merge target has not been modified at all and will not be until the whole rebase passes, so the failing state exists only in that worktree.

Failing output (tail):
---
{{failure_tail}}
---

Fix it in that worktree: change the tests or the code so the suite passes again, and stage every fix with `git add`.

Then STOP. Do NOT commit, do NOT amend, do NOT run `git reset`, `git rebase`, `git cherry-pick`, or any other history-rewriting command. The daemon commits your staged fix as a follow-up commit and re-runs the suite as soon as your turn ends.

You get EXACTLY ONE attempt. If the suite still fails after it, the merge is failed, the rebase worktree is discarded, and the merge target is left exactly as it was — it was never modified. Your branch keeps all of its work either way. If you cannot fix it, say so plainly.
