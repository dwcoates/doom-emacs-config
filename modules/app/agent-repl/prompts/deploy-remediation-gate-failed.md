<!-- used by: daemon internal/merge/remediationfinish.go (resumeRemediation); placeholders: {{archive_path}}, {{failing_tests}}, {{attempt}}, {{max_attempts}} -->
Your finish's full test gate failed on your branch rebased onto the main branch, so nothing was landed or deployed. The whole run is archived at {{archive_path}}. The failing test cases, read off that run:

{{failing_tests}}

Run only the failing test cases, and keep fixing and rerunning them until they pass. Run each through `agent-repld test run`, naming only the failing cases and the tests your fix adds or changes -- never the whole suite, and never a test command by hand. Fix the code or, where a test states something your change legitimately changed, the test, with a reason. Then commit and finish again: `agent-repld workspace finished here -version-part patch -changelog-line "<what the fix changes>"`. Your finish does not join the merge queue: agent-repl runs the full gate again, then lands your branch directly on the main branch and deploys it.

This is fixing attempt {{attempt}} of {{max_attempts}}; past the last, the deploy is escalated to the user.

Never pause, resume, evict or move anything on the merge queue.
