<!-- used by: daemon internal/merge/escalation.go (escalate); placeholders: {{repo_dir}}, {{deploy_started}}, {{summary}}, {{problems}} -->
The deploy of {{repo_dir}} that began at {{deploy_started}} was escalated to the user, and its merge queue moved on. The deploy check's summary:

{{summary}}

{{problems}}

This workspace is where the user settles it. Read the records above, say plainly what is wrong and what the choices are, and wait for the user's ruling before changing anything. Never pause, resume, evict or move anything on the merge queue.
