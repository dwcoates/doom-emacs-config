<!-- used by: daemon internal/merge/remediationfinish.go (resumeRemediation); placeholders: {{main_branch}}, {{why}}, {{attempt}}, {{max_attempts}} -->
Your finish could not land: your branch could not be brought onto {{main_branch}}'s tip, so nothing was gated, landed or deployed. Why:

{{why}}

Rebase your branch onto {{main_branch}} yourself, resolve every conflict so both sides' intent survives, run the tests the conflicts touched and the tests your fix adds or changes through `agent-repld test run` (never the whole suite), commit, and finish again: `agent-repld workspace finished here -version-part patch -changelog-line "<what the fix changes>"`. Your finish does not join the merge queue: agent-repl runs the full gate again, then lands your branch directly on {{main_branch}} and deploys it.

This is fixing attempt {{attempt}} of {{max_attempts}}; past the last, the deploy is escalated to the user.

Never pause, resume, evict or move anything on the merge queue.
