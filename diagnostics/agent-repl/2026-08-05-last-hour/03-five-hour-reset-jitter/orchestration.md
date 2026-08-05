# Orchestration: reset-window identity

Issue orchestrator worktree: `agent-repl-fix-03-integration`.

Parallel worktrees:

| Agent | Worktree | Exclusive ownership |
| --- | --- | --- |
| Contract agent | `agent-repl-fix-03-contract` | provider contract research note and fixture corpus only |
| Shim agent | `agent-repl-fix-03-shim` | timestamp parsing, canonical comparison, instrumentation |
| Test agent | `agent-repl-fix-03-tests` | UDS usage-window tests and boundary fixture matrix |

The shim agent begins with an interface placeholder and does not choose granularity until the contract agent supplies evidence. Expected overlap is under 10 percent because the test agent owns test files and the shim agent owns production files.

Merge order: contract note, shim, tests. If research proves a new wire field is required, the orchestrator pauses this issue and moves it into the same schema queue as issues 01 and 04. Otherwise it runs the full local gate and a fixture replay.

Cleanup follows the common worktree rule after the integrated result is preserved.
