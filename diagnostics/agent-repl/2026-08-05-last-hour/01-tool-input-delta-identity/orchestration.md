# Orchestration: tool-input identity

Issue orchestrator worktree: `agent-repl-fix-01-integration`.

Parallel implementation worktrees:

| Agent | Worktree | Exclusive ownership |
| --- | --- | --- |
| Schema agent | `agent-repl-fix-01-proto` | `core.proto`, protobuf generation, generated Go and TypeScript outputs, schema compatibility tests |
| Shim agent | `agent-repl-fix-01-shim` | Claude delta tracking and conversion, shim unit tests, shim instrumentation |
| Webapp agent | `agent-repl-fix-01-webapp` | `streaming.ts`, state adaptation, store logging, webapp ordering tests |

The agents may run concurrently after agreeing on the field name and generated accessor. Expected edit overlap is about 10 percent, confined to generated types consumed by the shim and webapp.

Merge order: schema commit, shim commit, webapp commit. The orchestrator regenerates protobuf outputs once after all merges, resolves generated-file conflicts, audits every changed path for instrumentation, runs `modules/app/agent-repl/bin/test-all.sh`, and performs a randomized order stress run.

Cleanup: after the integrated commit and test evidence are preserved, remove the three implementation worktrees and branches, then remove the integration worktree only after its commit is merged into the program branch.
