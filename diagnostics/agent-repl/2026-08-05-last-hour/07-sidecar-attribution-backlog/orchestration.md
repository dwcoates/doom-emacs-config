# Orchestration: sidecar attribution

Issue orchestrator worktree: `agent-repl-fix-07-integration`.

Parallel worktrees:

| Agent | Worktree | Exclusive ownership |
| --- | --- | --- |
| Ownership agent | `agent-repl-fix-07-owner` | owner index and authoritative resolution paths |
| Lifecycle agent | `agent-repl-fix-07-terminal` | held-state reason model, terminal classification, readiness counters |
| Test agent | `agent-repl-fix-07-tests` | restart, late-owner, and historical-spool fixture suite |

Expected overlap is about 20 percent because ownership and held lifecycle meet in sidecar state. The orchestrator requires the two production agents to agree on a small internal interface before coding. Each owns separate source files or extracted modules.

Merge ownership, lifecycle, then tests. Run sidecar Go tests, the full local gate, and a fixture replay that proves no guessed attribution. Cleanup follows the common rule.
