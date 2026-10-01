# A completed shell states whether it succeeded

The owner (2026-10-01) ruled that a background shell that exits with an error
draws a red dot, and a lost run (subagent or shell) a blue one. The proto
decision is the lead's (delegated).

## Landed changes

### 1. `frontend.v1.FeedShellCompleted.judgment` (succeeded / failed)

- What: the completed arm carries the daemon's judgment: `succeeded` (exit
  0) or `failed` (a non-zero exit, or a signal). SET BUT UNASSIGNED when the
  run carried no termination to judge, drawn as an ordinary completion.
- Why: the dot is coloured by the vocabulary (`feed_shell_dot`), and the
  webapp holds no business logic, so it must not read an exit code to
  decide failure. The daemon composes it in one place
  (`daemon/internal/resolve/feed/subagent.go` `shellJudgment`).
- Supersedes: the earlier rule that "failure is the reader's judgment of the
  code, never an arm".
- Not modeled as a new outcome arm: the process still ended on its own, so
  `completed` stays the outcome and the judgment refines it.
