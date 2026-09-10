<!-- used by: daemon internal/workspace/merge/testfailureresolver.go (TestFailureResolution.Prompt); placeholders: {{source_branch}}, {{target_dir}}, {{failure_tail}}, {{escalation_file}}, {{escalation_marker}} -->
Branch {{source_branch}} was just merged into the merge target with `git merge --no-ff` in the worktree at {{target_dir}}, producing a single merge commit, and the repository's test suite FAILS on that commit. The suite runs once on the merge commit, so the failure is a fact about the merged result as a whole rather than about any one commit of the source branch.

That worktree IS the merge target, not a temporary scratch tree and not your own workspace. Changes you make here are real changes to the target — the merge commit already exists there, and you are fixing the failure on top of it.

Failing output (tail):
---
{{failure_tail}}
---

Fix it in that worktree: change the tests or the code so the suite passes again, and stage every fix with `git add`.

Then STOP. Do NOT commit, do NOT amend, do NOT run `git reset`, `git rebase`, `git cherry-pick`, or any other history-rewriting command. The daemon commits your staged fix as a follow-up commit and re-runs the suite as soon as your turn ends.

There is NO attempt limit. If the suite still fails, you are asked again with the new failing output, and you may keep working the problem across as many turns as it takes. Fix things properly rather than papering over a failure to fit inside one turn.

The one way this ends without a passing suite is YOUR OWN JUDGEMENT. If you conclude that a correct fix requires unforeseen non-trivial ARCHITECTURAL changes — a redesign rather than a repair — then stop fixing and write the file `{{escalation_file}}` in that worktree, whose FIRST line is exactly:

{{escalation_marker}}

and whose remaining lines explain, in your own words, what the architectural problem is and why no local fix is correct. The daemon reads that file, fails the merge with your explanation as the reason a human will read, and resets the merge target back to where it was before the merge — there is no rebase worktree to discard. The source branch keeps all of its work either way. Do not write that file for a failure you simply have not finished working on.
