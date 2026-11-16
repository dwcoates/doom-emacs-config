<!-- used by: daemon internal/merge/mediation.go (intakeRound); placeholders: {{deploy_started}}, {{window_started}}, {{repo_dir}}, {{version}}, {{chain}}, {{bound}}, {{facts}}, {{reuse}} -->
A deploy of {{repo_dir}} began at {{deploy_started}} ({{version}}), and its merge queue lands nothing past it until you decide it.

The chain so far: {{chain}} The chain may deploy {{bound}} remediations in a row; at the bound, escalate.

What the daemon found in every system's logs since {{window_started}}, where the repository's last check read to, grouped by operation and message, and stood against the span before it and the problems the repository's checks keep:

{{facts}}

The last test gate's reuse, which the daemon judged by why its units ran:

{{reuse}}

A NEW reuse fact is escalated, with the reuse line as its evidence.

Also read `~/.claude/skills/agent-repl-runtime/run.sh --doctor`: a fault it reports is dispatched or escalated unless its records are OWNED above.

Decide this deploy in this turn, as your session's rules say: approve only when every group is OWNED, and otherwise dispatch or escalate every NEW, SEEN BEFORE and RECURRED group.
