<!-- used by: daemon internal/merge/process.go (run.fix); placeholders: {{source_branch}}, {{worktree_dir}}, {{target_branch}}, {{failing_suites}}, {{archive_path}}, {{failure_tail}}, {{attempt}}, {{max_attempts}}, {{failing_tests_file}}, {{failing_tests}} -->
The merge queue rebased {{source_branch}} onto {{target_branch}}'s tip in the worktree {{worktree_dir}}, and the repository's test suite FAILS on the rebased branch. Failing suites: {{failing_suites}}. {{target_branch}} has NOT moved: it moves only once the suite passes.

Failing output (tail; the whole run is archived at {{archive_path}}):
---
{{failure_tail}}
---

This is fixing attempt {{attempt}} of {{max_attempts}}. Fix it in {{worktree_dir}}: change the tests or the code so the suite passes, and COMMIT the fix to {{source_branch}} there. When your turn ends the queue rebases the branch onto {{target_branch}}'s tip and runs the selected suites again. If the last attempt still fails, the merge fails and the workspace is handed back to the user.

The failing test cases, read off that run (also written to {{failing_tests_file}}):
---
{{failing_tests}}
---

RUN ONLY THE FAILING TEST CASES, AND KEEP FIXING AND RERUNNING THEM UNTIL THEY PASS, BEFORE YOU END YOUR TURN. Your remediation is a loop over exactly those cases:
1. Run exactly the tests listed in {{failing_tests_file}}, the cases above: each group comes with the `agent-repld test run` command that runs only its failing cases through the daemon, and `run all` runs every one of them at once -- never the whole suite, and never a test command by hand (the daemon starts every test run; `bin/background.sh` refuses any other). Where it says a unit's tests could not be named (a build, vet or typecheck failure, a crash, a timeout), read that unit's output in the archive and run the smallest selection that reaches it (`agent-repld test run -go DIR`, `-vitest FILE`, `-ert FILE`, or `-suite NAME`). Do not commit that file; it is excluded from commits already.
2. Fix what fails, then rerun those same targeted cases, together with the tests this branch itself added or changed, named one by one. Repeat until every one of them passes.
3. Only then is the remediation resolved: commit and end your turn. The queue then runs the selected suites in full again (after its rebase), which is what checks everything else; a turn that ends with a failing case it was asked to fix spends an attempt and a full run of the selected suites on a known failure.

IF A FAILING TEST CANNOT BE RUN ON ITS OWN (its suite has no targeted invocation, or the test only fails inside the whole suite), do not stop, and do not weaken the test or the code to get around it:
- Run the smallest selection that does reach it (its package, its file, `-suite NAME`) through `agent-repld test run`, and keep fixing until it passes there, then commit and end your turn as above.
- Make sure the gap gets fixed, without compromising coverage or production UI/UX: the tests should be adapted so this test can be run on its own. Use the `/agent-repl-workspaces` skill: run `agent-repld workspace list` first, and if an open, non-one-shot workspace already investigates making this suite's tests runnable on their own, send it a prompt (`agent-repld workspace prompt`) naming this test and why it could not be run alone. Otherwise create a new NON-one-shot workspace cut from {{target_branch}} whose task is exactly that (`agent-repld workspace create`, a peer naming its repository, relationship and context as the skill describes). Then carry on with this fix; that workspace owns the gap, not you.

Do NOT change the merge machinery while this merge is running: nothing under `modules/app/agent-repl/daemon/internal/merge/` or `modules/app/agent-repl/testrun/`, and not `modules/app/agent-repl/bin/test-all.sh`. The running daemon is what gates this merge, so a change there cannot fix it; a repair that makes one fails the merge. A fix to the merge or its gate lands through a branch of its own.

NEVER END THE TURN WITHOUT A COMMITTED FIX, and never leave a fix broken on purpose to make the merge fail: a turn that commits nothing fails the merge, and a broken fix spends a whole run of the suite on a merge that cannot land. If you conclude that you cannot fix the failures correctly -- a correct fix needs an architectural change, a redesign rather than a repair, or a decision that is the user's -- give the merge up instead, then end your turn with a reply explaining the problem:

`agent-repld merge-queue give-up -reason "<why the failures cannot be fixed here, in a sentence or two>"`

The merge leaves the queue at once, no test run starts for it, and the workspace is yours again. Do not give up a failure you simply have not finished working on.
