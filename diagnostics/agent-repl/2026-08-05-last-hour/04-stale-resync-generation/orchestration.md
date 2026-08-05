# Orchestration: generation-bound resync

Issue orchestrator worktree: `agent-repl-fix-04-integration`.

Parallel implementation worktrees:

| Agent | Worktree | Exclusive ownership |
| --- | --- | --- |
| Schema agent | `agent-repl-fix-04-proto` | `frontend.proto`, generated outputs, compatibility tests |
| Daemon agent | `agent-repl-fix-04-daemon` | command validation, controller transition ordering, daemon tests |
| Frontend agent | `agent-repl-fix-04-frontends` | webapp and Emacs resync construction, client tests and logs |

Expected overlap is about 15 percent. Generated outputs belong only to the schema agent. The frontend agent may consume temporary handwritten fixture shapes but must rebase on the generated schema before final tests.

Merge order: schema, daemon, frontends. The orchestrator owns any conflict with issue 01 in generated protobuf files, reruns generation once, audits that all resync senders populate both fields, and runs the full local gate plus a delayed-command race test.

Cleanup follows the common rule after the integrated commit is on the program branch.
