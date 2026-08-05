# Orchestration: bounded diagnostics

Issue orchestrator worktree: `agent-repl-fix-11-integration`.

Parallel worktrees:

| Agent | Worktree | Exclusive ownership |
| --- | --- | --- |
| Input agent | `agent-repl-fix-11-input` | metaprompt predicate normalization, input logging, ERT tests |
| Autosave agent | `agent-repl-fix-11-autosave` | perspective summaries, autosave logging, ERT tests |
| Rate agent | `agent-repl-fix-11-rate` | hot-operation inventory, transition or aggregation helpers, repeat diagnostics |
| Audit agent | `agent-repl-fix-11-audit` | log-content and payload-size test harness only |

Expected overlap is 15 to 20 percent around shared logging helpers. The rate agent owns helper API changes. Input and autosave agents stay within their feature files until that helper is merged.

Merge rate helper, input, autosave, then audit tests. The orchestrator reviews every removed field against the instrumentation contract, runs the full local gate, captures a controlled one-minute log sample, and compares record count and maximum payload size with this baseline.

Cleanup follows the common rule after metrics and commits are preserved.
