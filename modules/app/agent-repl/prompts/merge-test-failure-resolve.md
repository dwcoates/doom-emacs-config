<!-- used by: daemon internal/workspace/merge/testfailureresolver.go (TestFailureResolution.Prompt); placeholders: {{source_branch}}, {{target_dir}}, {{failure_tail}}, {{escalation_file}}, {{escalation_marker}} -->
Every commit of branch {{source_branch}} was just rebased onto the merge target in the worktree at {{target_dir}}, and the repository's test suite FAILS on the resulting head. The suite runs once per merge, on that head, so the failure is a fact about the whole rebased line rather than about any one commit of it.

That worktree is a TEMPORARY REBASE WORKTREE, not the merge target and not your own workspace. The merge target has not been modified at all and will not be until the whole rebase passes, so the failing state exists only in that worktree.

Failing output (tail):
---
{{failure_tail}}
---

Fix it in that worktree: change the tests or the code so the suite passes again, and stage every fix with `git add`.

Then STOP. Do NOT commit, do NOT amend, do NOT run `git reset`, `git rebase`, `git cherry-pick`, or any other history-rewriting command. The daemon commits your staged fix as a follow-up commit and re-runs the suite as soon as your turn ends.

There is NO attempt limit. If the suite still fails, you are asked again with the new failing output, and you may keep working the problem across as many turns as it takes. Fix things properly rather than papering over a failure to fit inside one turn.

The one way this ends without a passing suite is YOUR OWN JUDGEMENT. If you conclude that a correct fix requires unforeseen non-trivial ARCHITECTURAL changes — a redesign rather than a repair — then stop fixing and write the file `{{escalation_file}}` in that worktree, whose FIRST line is exactly:

{{escalation_marker}}

and whose remaining lines explain, in your own words, what the architectural problem is and why no local fix is correct. The daemon reads that file, fails the merge with your explanation as the reason a human will read, discards the rebase worktree, and leaves the merge target exactly as it was — it was never modified. Your branch keeps all of its work either way. Do not write that file for a failure you simply have not finished working on.
