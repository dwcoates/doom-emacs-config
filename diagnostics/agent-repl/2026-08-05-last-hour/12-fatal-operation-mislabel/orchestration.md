# Orchestration: shim operation labels

Issue orchestrator worktree: `agent-repl-fix-12-integration`.

Parallel worktrees:

| Agent | Worktree | Exclusive ownership |
| --- | --- | --- |
| Shim agent | `agent-repl-fix-12-main` | logger bindings and main lifecycle call sites |
| Test agent | `agent-repl-fix-12-tests` | operation and level contract tests, query fixture |

Expected overlap is below 10 percent. Merge source then tests. The orchestrator runs shim TypeScript tests, the full local gate, and a fake-shim startup plus fatal fixture to verify operation separation. Cleanup follows the common rule.
