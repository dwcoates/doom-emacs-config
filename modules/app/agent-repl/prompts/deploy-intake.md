<!-- used by: daemon internal/merge/mediation.go (intakeRound); placeholders: {{deploy_started}}, {{repo_dir}}, {{version}}, {{chain}}, {{bound}}, {{facts}}, {{reuse}} -->
A deploy of {{repo_dir}} began at {{deploy_started}} ({{version}}), and its merge queue lands nothing past it until you decide it.

The chain so far: {{chain}} The chain may deploy {{bound}} remediations in a row; at the bound, escalate.

What the daemon found in every system's logs since the deploy began, grouped by operation and message, and sorted against the same span before it:

{{facts}}

The test gates' reuse, which the daemon judged against the gates before:

{{reuse}}

A NEW reuse fact is escalated, with the reuse line as its evidence.

Also read `~/.claude/skills/agent-repl-runtime/run.sh --doctor`: a fault it reports is a problem of this deploy only when its records are NEW above.

Decide this deploy in this turn, as your session's rules say: approve, escalate, or dispatch the problems this deploy caused. First send each SEEN BEFORE group, with its records, to `merge-queue-stall-triage`, and name it in your decision's summary.
