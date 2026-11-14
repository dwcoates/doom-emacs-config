<!-- used by: daemon internal/merge/mediation.go (mediationRound); placeholders: {{deploy_started}}, {{repo_dir}}, {{version}} -->
A deploy of agent-repl began at {{deploy_started}} ({{version}}), and the merge queue of {{repo_dir}} advances nothing until you decide it. Check the deploy:

1. Every system's warnings and errors since it began: `{{repo_dir}}/modules/app/agent-repl/bin/logs.sh --all --since {{deploy_started}} --level warn`.
2. `~/.claude/skills/agent-repl-runtime/run.sh --doctor`.
3. Latency against the versions before it, through the `agent-repl-metrics` skill: workspace switches, page boots, vendor first output, and the deploy's own phases.

Judge only what THIS deploy caused (owner ruling, 2026-11-14). A warning or error that also appears before {{deploy_started}}, or that comes from code this deploy did not change, never holds the queue: do not fix it in this turn. Name each such defect in your decision's summary, and tell merge-queue-stall-triage about it (`agent-repld workspace prompt merge-queue-stall-triage "<the defect and its records>"`), which dispatches its fix through the queue.

Then decide, in this turn:

- Nothing wrong: `agent-repld merge-queue mediation approve -summary "<one sentence>"`.
- Something wrong that you cannot fix confidently, or whose fix has a reasonable user-facing alternative the user should choose between: `agent-repld merge-queue mediation escalate -summary "<what is wrong, in one or two sentences>"`. The user is notified, and the queue moves on.
- Something wrong that this deploy caused and that you can fix: fix it yourself, as your session's rules say, and deploy it. That deploy is checked by you in turn; the queue stays held until a deploy passes or is escalated.

A turn that ends deciding nothing lets the queue through and tells the user you decided nothing.
