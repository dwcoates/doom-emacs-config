# Orchestration: store doctor probe

Issue orchestrator worktree: `agent-repl-fix-10-integration`.

Parallel worktrees:

| Agent | Worktree | Exclusive ownership |
| --- | --- | --- |
| Client agent | `agent-repl-fix-10-health-client` | owned correlated store-health client and Go tests |
| Doctor agent | `agent-repl-fix-10-doctor` | doctor integration, JSON result classification, shell harness |

Expected overlap is under 10 percent at the executable interface. Agree first on exit codes and JSON output. The client agent does not edit the doctor and the doctor agent does not edit store protocol code.

Merge client then doctor. The orchestrator runs focused Go and shell tests, the full local gate, and one live read-only doctor check. Cleanup follows the common rule.
