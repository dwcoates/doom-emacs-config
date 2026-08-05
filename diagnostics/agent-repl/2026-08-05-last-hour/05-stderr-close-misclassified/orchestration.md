# Orchestration: stderr lifecycle

Issue orchestrator worktree: `agent-repl-fix-05-integration`.

Parallel worktrees:

| Agent | Worktree | Exclusive ownership |
| --- | --- | --- |
| Lifecycle agent | `agent-repl-fix-05-proc` | `daemon/internal/shim/proc.go` and lifecycle instrumentation |
| Test agent | `agent-repl-fix-05-tests` | process-pump fixtures and shutdown-order tests |

Expected overlap is below 10 percent. The lifecycle agent publishes a small pump contract before the test agent finalizes fixtures.

Merge source then tests. The orchestrator runs focused Go tests, the full local gate, and checks that no test starts a real external shim process. Cleanup follows the common rule.
