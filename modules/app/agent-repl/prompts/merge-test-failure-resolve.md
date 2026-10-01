<!-- used by: daemon internal/merge/process.go (run.fix); placeholders: {{source_branch}}, {{worktree_dir}}, {{target_branch}}, {{failing_suites}}, {{archive_path}}, {{failure_tail}}, {{attempt}}, {{max_attempts}}, {{escalation_file}}, {{escalation_marker}} -->
The merge queue rebased {{source_branch}} onto {{target_branch}}'s tip in the worktree {{worktree_dir}}, and the repository's test suite FAILS on the rebased branch. Failing suites: {{failing_suites}}. {{target_branch}} has NOT moved: it moves only once the suite passes.

Failing output (tail; the whole run is archived at {{archive_path}}):
---
{{failure_tail}}
---

This is fixing attempt {{attempt}} of {{max_attempts}}. Fix it in {{worktree_dir}}: change the tests or the code so the suite passes, and COMMIT the fix to {{source_branch}} there. When your turn ends the queue runs the suite again on the branch. If the last attempt still fails, the merge fails and the workspace is handed back to the user.

Do NOT change the merge machinery while this merge is running: nothing under `modules/app/agent-repl/daemon/internal/merge/` and not `modules/app/agent-repl/bin/test-all.sh`. The running daemon is what gates this merge, so a change there cannot fix it; a repair that makes one fails the merge. A fix to the merge or its gate lands through a branch of its own.

If you conclude that a correct fix requires unforeseen non-trivial ARCHITECTURAL changes — a redesign rather than a repair — stop fixing and write the file `{{escalation_file}}` at the root of {{worktree_dir}} (do not commit it), whose FIRST line is exactly:

{{escalation_marker}}

and whose remaining lines explain, in your own words, what the architectural problem is and why no local fix is correct. The queue reads that file, removes it, and fails the merge with your explanation as the reason a human will read. Do not write that file for a failure you simply have not finished working on.
