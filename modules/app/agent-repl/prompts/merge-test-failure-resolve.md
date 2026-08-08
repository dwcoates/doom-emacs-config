<!-- used by: daemon internal/workspace/merge/testfailureresolver.go (TestFailureResolution.Prompt); placeholders: {{failing_commit}}, {{source_branch}}, {{target_dir}}, {{failure_tail}} -->
Commit {{failing_commit}} from branch {{source_branch}} was just cherry-picked into the worktree at {{target_dir}}, and the repository's test suite now FAILS there.

Failing output (tail):
---
{{failure_tail}}
---

Fix it in that worktree: change the tests or the code so the suite passes again, and stage every fix with `git add`.

Then STOP. Do NOT commit, do NOT amend, do NOT run `git reset`, `git rebase`, `git cherry-pick`, or any other history-rewriting command. The daemon commits your staged fix as a follow-up commit and re-runs the suite as soon as your turn ends.

You get EXACTLY ONE attempt. If the suite still fails after it, the merge is failed and the target is rolled back to where it was before the merge started — your branch keeps all of its work either way. If you cannot fix it, say so plainly.
