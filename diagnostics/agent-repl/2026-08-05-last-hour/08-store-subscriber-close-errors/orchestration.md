# Orchestration: store subscriber termination

Issue orchestrator worktree: `agent-repl-fix-08-integration`.

Parallel worktrees:

| Agent | Worktree | Exclusive ownership |
| --- | --- | --- |
| State-machine agent | `agent-repl-fix-08-server` | subscriber terminal state and server instrumentation |
| Test agent | `agent-repl-fix-08-tests` | deterministic socket fixtures and concurrency tests |
| Correlation agent | `agent-repl-fix-08-logging` | logging field shape and cross-runtime correlation assertions |

Expected overlap is 15 percent. The state-machine agent owns `server.go`; the logging agent owns logging helpers and assertions, not the connection loop.

Merge state machine, logging, then tests. The orchestrator runs race-enabled Go tests where supported, the full local gate, and checks that each fixture yields exactly one terminal record. Cleanup follows the common rule.
