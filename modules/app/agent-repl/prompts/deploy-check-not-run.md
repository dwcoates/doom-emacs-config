<!-- used by: daemon internal/merge/escalation.go (escalate); placeholders: {{repo_dir}}, {{deploy_started}}, {{what}}, {{cause}} -->
agent-repl could not prepare the check of the deploy of `{{repo_dir}}` that began at {{deploy_started}}, so nothing has looked at that deploy yet. This says nothing about whether the deploy broke anything.

- What failed: {{what}}
- The cause: {{cause}}

The merge queue lands nothing past the deploy until you decide it. The user was notified that the check could not run, and can release the queue themselves (the "release" beside "zettel needs you").

Your job is the usual one, without the digest: find out whether this deploy broke anything, fix what it broke, and decide the deploy. The decision is yours alone: you need no approval from the user.

- Sample the warnings and errors since the deploy began: `bin/logs.sh --all --level warn --since {{deploy_started}} --tally --sample 1`
- If the deploy broke nothing, or everything you find was there before it, approve it at once, naming each older group: `agent-repld merge-queue mediation approve -summary "<one sentence>"`
- If the deploy broke something, fix it on this workspace's branch with tests, commit, and finish: `agent-repld workspace finished here -version-part patch -changelog-line "<what the fix changes>"`
  - The check is held as an escalation, so its fix lands through the finish and no dispatch is made.
- If the fix turns on a decision that is the user's, say plainly what is wrong and what the choices are, and wait for their ruling.

The failure above is a failure of agent-repl's own deploy check, so it is always yours to fix, whenever it was first logged: fix it with the rest and finish, never name it on the approval as older.
