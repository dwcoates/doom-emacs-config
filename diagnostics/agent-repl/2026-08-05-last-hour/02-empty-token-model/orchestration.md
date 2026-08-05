# Orchestration: token model validation

Issue orchestrator worktree: `agent-repl-fix-02-integration`.

Parallel implementation worktrees:

| Agent | Worktree | Exclusive ownership |
| --- | --- | --- |
| Ingress agent | `agent-repl-fix-02-validation` | canonical token validation and producer call sites, validation tests |
| Reducer agent | `agent-repl-fix-02-aggregate` | frontend aggregation invariant, frame construction diagnostics, reducer tests |
| Data-audit agent | `agent-repl-fix-02-audit` | read-only audit and explicit migration command, migration tests and operator output |

Expected overlap is 15 percent, mostly shared token-utilization fixtures. The ingress agent owns fixture API changes. Other agents rebase once on that commit before finalizing tests.

Merge order: ingress, reducer, data audit. The orchestrator runs the audit in read-only mode first, reviews every proposed mutation path, then runs the full local gate. Any actual data migration must be a separately approved operational step.

Cleanup follows the common rule in the root index after all commits and audit output are preserved.
