<!-- used by: daemon internal/merge/mediation.go (intakeRound); placeholders: {{deploy_started}}, {{ending}}, {{window_started}}, {{repo_dir}}, {{version}}, {{gate_log}} -->
A deploy of `{{repo_dir}}` began at {{deploy_started}} ({{version}}). {{ending}} The merge queue lands nothing past it until you decide it.

Your job: find out whether this deploy left agent-repl healthy, fix what it broke, and decide the deploy.

What you have:
- Warnings and errors since the last check: `bin/logs.sh --all --level warn --since {{window_started}}`
- Metrics: the `/agent-repl-metrics` skill
- Health: `~/.claude/skills/agent-repl-runtime/run.sh --doctor`
- The last test gate before the deploy: {{gate_log}}
- What your earlier fixes changed: `docs/REMEDIATION-CHANGELOG.md`

Decide the deploy with one `agent-repld merge-queue mediation` verb:
- `approve -summary "<one sentence>"`
  - Releases the queue, and ends this check.
  - Call it when the deploy broke nothing, or when everything you found is transient or ignorable.
- `dispatch -summary "<one sentence>"`, with the problems as JSON on standard input
  - Records the problems as yours and keeps the queue held while you fix them.
  - Call it when you can fix what the deploy broke yourself, before you start the fix. Then fix it and run `agent-repld workspace finished here`.
- `escalate -problems -summary "<one or two sentences>"`, with the problems as JSON on standard input
  - Notifies the user. The queue stays held until they rule.
  - Call it when the fix turns on a decision that is the user's, or when you cannot fix the problem from the evidence.
