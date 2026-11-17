<!-- used by: daemon internal/merge/escalation.go (escalate); placeholders: {{repo_dir}}, {{deploy_started}}, {{summary}}, {{problems}}, {{old_problems}} -->
The deploy of {{repo_dir}} that began at {{deploy_started}} was escalated to the user, and its merge queue lands nothing until this workspace's fix deploys and its deploy check approves it, or the user releases the queue (the "release" beside "zettel needs you", or `agent-repld merge-queue release-escalation -repository here`). This is the repository's standing fixer: every dispatched fix and every escalated deploy comes here, so earlier ones in this conversation may bear on this one. Its worktree was brought to the main branch's tip for this brief. The deploy check's summary:

{{summary}}

{{problems}}

{{old_problems}}

This workspace is where the user settles it. Read the records above, then settle the requirements with the user: say plainly what is wrong and what the choices are, and wait for their ruling before changing anything. Never pause, resume, evict or move anything on the merge queue.

Once the requirements are settled, implement them on this workspace's branch with tests, as the repository's rules say, commit, and finish at once, without being asked: `agent-repld workspace finished here -version-part patch -changelog-line "<what the fix changes>"`. Your finish does not join the merge queue: agent-repl runs the full test gate on your branch rebased onto the main branch, resumes you with any failures to fix and finish again, then lands the branch directly on the main branch and deploys it. The deploy intake checks that deploy in turn. The finish is your turn's last act: end the turn right after it, with a one-line reply at most. agent-repl ends this session once that turn ends, and starts it again only when its gate sends you back to work.
