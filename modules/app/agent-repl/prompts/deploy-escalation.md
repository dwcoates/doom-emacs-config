<!-- used by: daemon internal/merge/escalation.go (escalate); placeholders: {{repo_dir}}, {{deploy_started}}, {{summary}}, {{problems}} -->
The deploy of {{repo_dir}} that began at {{deploy_started}} was escalated to the user, and its merge queue moved on. The deploy check's summary:

{{summary}}

{{problems}}

This workspace is where the user settles it. Read the records above, say plainly what is wrong and what the choices are, and wait for the user's ruling before changing anything. Never pause, resume, evict or move anything on the merge queue.

Once the user rules, fix what they ruled on this workspace's branch with tests, as the repository's rules say, and commit. Then run `agent-repld workspace finished here -version-part patch -changelog-line "<what the fix changes>"`: your finish does not join the merge queue. agent-repl runs the full test gate on your branch rebased onto the main branch, resumes you with any failures to fix and finish again, then lands the branch directly on the main branch and deploys it. The deploy intake checks that deploy in turn.
