<!-- used by: daemon internal/merge/phases.go (run.fixes); placeholders: {{source_branch}}, {{source_dir}}, {{target_branch}}, {{target_dir}}, {{queue_dir}}, {{archive_path}}, {{failure_tail}}, {{escalation_file}}, {{escalation_marker}} -->
The merge queue merged your branch {{source_branch}} onto {{target_branch}} with `git merge --no-ff`, in a scratch tree of its own at {{queue_dir}}, and the repository's test suite FAILS on that merge commit. The suite runs once on the merged result, so the failure is a fact about the merge as a whole rather than about any one commit of your branch. {{target_branch}} in {{target_dir}} has NOT moved: it moves only once the suite passes.

Failing output (tail; the whole run is archived at {{archive_path}}):
---
{{failure_tail}}
---

You may read the merged tree at {{queue_dir}} to investigate, but change nothing there: it is the queue's, and it is thrown away when your turn ends. Never touch {{target_dir}}.

Fix it on YOUR OWN branch, in your own worktree at {{source_dir}}: change the tests or the code so the suite passes, and COMMIT the fix to {{source_branch}}. When your turn ends, the queue merges your branch again onto {{target_branch}}'s current tip and re-runs the suite.

Do NOT change the merge machinery while this merge is running: nothing under `modules/app/agent-repl/daemon/internal/merge/` and not `modules/app/agent-repl/bin/test-all.sh`. The running daemon is what gates this merge, so a change there cannot fix it; the queue refuses a repair that makes one and parks the merge. A fix to the merge or its gate lands through a branch of its own.

There is NO attempt limit. If the suite still fails, you are asked again with the new failing output, and you may keep working the problem across as many turns as it takes. Fix things properly rather than papering over a failure to fit inside one turn.

The one way this ends without a passing suite is YOUR OWN JUDGEMENT. If you conclude that a correct fix requires unforeseen non-trivial ARCHITECTURAL changes — a redesign rather than a repair — then stop fixing and write the file `{{escalation_file}}` at the root of your worktree {{source_dir}} (do not commit it), whose FIRST line is exactly:

{{escalation_marker}}

and whose remaining lines explain, in your own words, what the architectural problem is and why no local fix is correct. The queue reads that file, removes it, and parks the merge with your explanation as the reason a human will read. {{target_branch}} is never touched, and your branch keeps all of its work. Do not write that file for a failure you simply have not finished working on.
