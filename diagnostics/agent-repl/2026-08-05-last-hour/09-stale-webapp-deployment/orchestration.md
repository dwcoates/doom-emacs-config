# Orchestration: webapp deployment drift

Issue orchestrator worktree: `agent-repl-fix-09-integration`.

Parallel worktrees:

| Agent | Worktree | Exclusive ownership |
| --- | --- | --- |
| Deployment agent | `agent-repl-fix-09-deploy` | canonical build/deploy script and artifact revision assertion |
| Verification agent | `agent-repl-fix-09-verify` | readiness and browser-level feature verification, no deployment mutation |

Expected edit overlap is below 5 percent. The deployment agent commits any script or test changes. The verification agent records evidence against that commit after the orchestrator authorizes deployment.

Merge any deployment-gate changes first, run the full local gate, then perform the operational deployment and live verification. The orchestrator records the before and after revisions and cleans both worktrees only after the deployed artifact is proven. A normal Emacs module reload is not accepted as a webapp deployment.
