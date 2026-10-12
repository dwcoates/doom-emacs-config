<!-- used by: daemon internal/merge/process.go (run.fix); placeholders: {{source_branch}}, {{worktree_dir}}, {{target_branch}}, {{failing_suites}}, {{archive_path}}, {{failure_tail}}, {{attempt}}, {{max_attempts}}, {{escalation_file}}, {{escalation_marker}} -->
The merge queue rebased {{source_branch}} onto {{target_branch}}'s tip in the worktree {{worktree_dir}}, and the repository's test suite FAILS on the rebased branch. Failing suites: {{failing_suites}}. {{target_branch}} has NOT moved: it moves only once the suite passes.

Failing output (tail; the whole run is archived at {{archive_path}}):
---
{{failure_tail}}
---

This is fixing attempt {{attempt}} of {{max_attempts}}. Fix it in {{worktree_dir}}: change the tests or the code so the suite passes, and COMMIT the fix to {{source_branch}} there. When your turn ends the queue rebases the branch onto {{target_branch}}'s tip and runs the whole suite again. If the last attempt still fails, the merge fails and the workspace is handed back to the user.

RUN ONLY THE FAILING TESTS, AND KEEP FIXING UNTIL THEY PASS, BEFORE YOU END YOUR TURN. As the final stage of every fix:
1. Run exactly the tests that failed above -- each by name, with its suite's own targeted invocation (the module's AGENTS.md lists them: e.g. `go test -run '^TestName$' ./pkg/...` through `bin/background.sh`, `npm test -- -t "name"`, one ERT file through batch Emacs), never the whole suite and never `bin/test-all.sh`.
2. If any of them still fails, fix again and run them again. Repeat until every one of them passes.
3. Only then commit and end your turn. The queue's own full run (after its rebase) is what checks everything else; a turn that ends with a failing test it was asked to fix spends an attempt and a whole suite run on a known failure.

IF A FAILING TEST CANNOT BE RUN ON ITS OWN (its suite has no targeted invocation, or the test only fails inside the whole suite), do not stop, and do not weaken the test or the code to get around it:
- Run the smallest invocation that does reach it (its package, its file, its suite) and keep fixing until it passes there, then commit and end your turn as above.
- Make sure the gap gets fixed, without compromising coverage or production UI/UX: the tests should be adapted so this test can be run on its own. Use the `/create-or-update-workspace` skill: run `status` first, and if an open, non-one-shot workspace already investigates making this suite's tests runnable on their own, send it a `prompt` naming this test and why it could not be run alone. Otherwise `create` a new NON-one-shot workspace off {{target_branch}} whose task is exactly that. Then carry on with this fix; that workspace owns the gap, not you.

Do NOT change the merge machinery while this merge is running: nothing under `modules/app/agent-repl/daemon/internal/merge/` or `modules/app/agent-repl/testrun/`, and not `modules/app/agent-repl/bin/test-all.sh`. The running daemon is what gates this merge, so a change there cannot fix it; a repair that makes one fails the merge. A fix to the merge or its gate lands through a branch of its own.

If you conclude that a correct fix requires unforeseen non-trivial ARCHITECTURAL changes — a redesign rather than a repair — stop fixing and write the file `{{escalation_file}}` at the root of {{worktree_dir}} (do not commit it), whose FIRST line is exactly:

{{escalation_marker}}

and whose remaining lines explain, in your own words, what the architectural problem is and why no local fix is correct. The queue reads that file, removes it, and fails the merge with your explanation as the reason a human will read. Do not write that file for a failure you simply have not finished working on.
