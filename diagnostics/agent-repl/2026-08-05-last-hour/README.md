# Agent REPL last-hour incident inventory

## Scope

- Evidence window: `2026-08-05T12:56:29-04:00` through `2026-08-05T13:56:29.999999-04:00`.
- Primary workspaces: `doom`, `agent-repl-bubble-breath`, `ceac-sandbox-gns-gate`, and `slack-ceac-tech-ptn`.
- Sources: canonical workspace JSONL for Emacs, daemon, shim, webapp, and sidecar, plus the global daemon, store, sidecar, and Emacs sinks.
- This directory contains diagnosis and implementation planning only. No runtime or product fix was applied.

## Identity ledger

| Workspace | Workspace ID | Agent REPL session |
| --- | --- | --- |
| `doom` | `433af0a5` | `s_63499d7dc607c154`, then `s_7d1e94a17392b33a` |
| `agent-repl-bubble-breath` | `f5208590` | `s_5a6403cc7dff12d7` |
| `ceac-sandbox-gns-gate` | `ffb37303` | `s_273f52f20dfb8128` |
| `slack-ceac-tech-ptn` | `87dc75bf` | `s_12ea5df02075d2f2` |

## Confirmed issue inventory

| ID | Issue | Severity | Confidence | Protobuf |
| --- | --- | --- | --- | --- |
| 01 | Tool input deltas lose correlation after the durable final arrives first | High | Certain | Required |
| 02 | Empty token model reaches aggregation and makes frontend frames undecodable | High | Certain | No |
| 03 | Fractional reset timestamp jitter suppresses valid five-hour deltas | Medium | Certain | Conditional only |
| 04 | A resync from a retired controller generation targets a detached shim | Medium | High | Recommended |
| 05 | Normal shim pipe closure is emitted as a daemon error | Low | Certain | No |
| 06 | Session events arrive before workspace materialization and are dropped or misrouted | High | Certain | No |
| 07 | Sidecar retains a large, growing spool attribution backlog | Medium | Certain | No for the core fix |
| 08 | Expected subscriber closure becomes duplicate store transport errors | Medium | Certain | No |
| 09 | The running webapp artifact is behind the breathing-bubble source | Medium | Certain | No |
| 10 | The doctor reports the healthy store socket as connection-refused | Medium | Certain | No |
| 11 | Logging persists the metaprompt and complete perspective structures at high volume | High | Certain | No |
| 12 | Normal shim lifecycle records are labeled with the `shim.main.fatal` operation | Low | Certain | No |

See [PROTOBUF-IMPACT.md](PROTOBUF-IMPACT.md) for the field-level review.

## Evidence volume

The fixed window contained 25,140 records for `doom`, 18,257 for `agent-repl-bubble-breath`, 15,616 for `ceac-sandbox-gns-gate`, and 22,767 for `slack-ceac-tech-ptn`. The most material repeated symptoms were:

- 908 dropped tool-input deltas across 100 distinct message/block pairs.
- Seven rejected token-utilization frames in `doom`.
- Five provably false five-hour window-change classifications.
- Eight global store transport errors, seven associated with the four focused workspaces.
- A sidecar backlog rising from 1,818 to 1,821 held spools, all 1,821 stale by the final warning.
- 43,400 Emacs records across the three long-lived focused workspaces, including five approximately 31 KB metaprompt records and repeated multi-KB perspective dumps.

## Deliberate exclusions

The following were investigated and are not listed as bugs:

- Websocket `connecting` and `awaiting_snapshot` invalidations were normal transitional state.
- Cache-hit warnings represented measured rates below the configured threshold. Duplicate warning emission is covered by issue 11, but the low rates themselves are not classified as a product defect here.
- Missing current entries in the bounded registry were explained by terminal-record compaction at the configured 128-record limit.
- `agent-repl-log-discovery.sh` printed locations when called without `--session` or `--pid`, exactly as its help and debug-skill reference specify.
- SQLite integrity was skipped because the 38 GB database exceeds the automatic 1 GiB doctor limit. That is an unperformed deep check, not evidence of corruption.

## Program orchestration

Each issue directory contains `diagnosis.md`, `remediation.md`, and `orchestration.md`. The issue orchestrator owns a dedicated integration worktree, merges only committed implementation branches, runs the complete `modules/app/agent-repl/bin/test-all.sh` gate, resolves conflicts, and removes all issue worktrees only after the integrated commit is preserved.

Maximum parallelism is obtained in three program waves:

1. Start issues 02, 03, 05, 06, 07, 08, 09, 10, 11, and 12 concurrently because their primary edit sets are disjoint.
2. Start issue 01 and issue 04 schema work concurrently, but serialize their generated protobuf commits through a small schema integration queue.
3. Rebase every issue integration branch on the accumulated program branch, run the full gate after each protobuf issue and after each four non-protobuf issues, then run one final full gate.

Expected edit collision is below 20 percent within every issue plan. Cross-issue collision is concentrated in generated protobuf files, shared TypeScript lockstep tests, and the unified timing-neutral test runner. Those files have explicit merge ownership in the two schema plans.
