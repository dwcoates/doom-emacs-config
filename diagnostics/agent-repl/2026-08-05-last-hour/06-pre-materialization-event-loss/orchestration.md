# Orchestration: workspace materialization gate

Issue orchestrator worktree: `agent-repl-fix-06-integration`.

Parallel worktrees:

| Agent | Worktree | Exclusive ownership |
| --- | --- | --- |
| Daemon agent | `agent-repl-fix-06-daemon` | creation job state, state-publication gate, daemon tests |
| Emacs agent | `agent-repl-fix-06-emacs` | workspace-create client invariant, routing diagnostics, ERT tests |
| Race-test agent | `agent-repl-fix-06-races` | cross-component delayed-materialization harness and fixtures |

Expected overlap is 15 to 20 percent, concentrated in creation fixtures. The daemon agent owns job-state fixture API changes, and the Emacs agent owns workspace wrapper usage.

Merge daemon, Emacs, then race tests. The orchestrator audits all workspace-state access through `workspace.el`, checks that no new third-party call bypasses a wrapper, runs the full local gate, and repeats the delayed-materialization scenario with several orderings.

Cleanup follows the common rule after the integration commit is preserved.
