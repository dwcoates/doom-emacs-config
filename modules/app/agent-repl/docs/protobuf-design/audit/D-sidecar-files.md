# Partition D — sidecar non-transcript files: field-level audit

Read-only audit. Inventory script: `inventory_sidecar_files.py` (beside this file); raw output `inventory.out`.
Tier: **observed** — every count below is from the real files under `~/.claude`, `~/.claude-chesscom`, `/tmp/claude-501` on 2026-08-22.

Sources cited:
- `agent-shim/claude/shim-sidecar/internal/discover/discover.go` (discover.go)
- `agent-shim/claude/shim-sidecar/internal/handler/{shell,journal,agent,handler}.go`
- `agent-shim/claude/shim-sidecar/internal/convert/{journal,detached}.go`
- `agent-shim/claude/shim-sidecar/main.go`, `owner.go`, `internal/stale/stale.go`
- `proto/src/conversation/v1/{detached_work,workflow,agent,agent_activity}.proto`
- `docs/protobuf-design/figma-to-idl-redesign.md` (the "record")

---

## 0. What the sidecar tails vs. what exists

| File kind | Real path shape | On disk (count) | discover.go | Handler |
|---|---|---|---|---|
| A. Workflow journal | `projects/<proj>/<sess>/subagents/workflows/wf_<id>/journal.jsonl` | 114 (57 per root, byte-identical between roots) | glob L86, classify L143-152 | `handler/journal.go` → `convert/journal.go` |
| B. Workflow agent transcript | `.../workflows/wf_<id>/agent-<aid>.jsonl` | 172 (86 unique ids) | **NOT discovered** — glob L85 is non-recursive; classifyConfig L132 requires `len(segs)==4`, these have 6 | none |
| C. Workflow agent meta | `.../workflows/wf_<id>/agent-<aid>.meta.json` | 172 | not globbed; `MetaPath` never set for these | none |
| D. Ordinary agent meta | `projects/<proj>/<sess>/subagents/agent-<aid>.meta.json` | 2125 (1174 + 541 per root, plus duplicates) | `MetaPath` derived L141 — **never read anywhere** (grep: only producer of `MetaPath`, zero consumers) | none |
| E. Workflow run record | `projects/<proj>/<sess>/workflows/wf_<id>.json` | 114 | **NOT discovered** (no glob, no classify arm) | none |
| F. Persisted workflow script | `projects/<proj>/<sess>/workflows/scripts/<name>-wf_<id>.js` | 91 unique files; referenced by all 114 run records | **NOT discovered** | none |
| G. Shell spool | `/tmp/claude-<uid>/<slug>/<runtime-sess>/tasks/b<id>.output` | 1570 | glob L91, classify L159-201 | `handler/shell.go` |
| H. Agent spool | `.../tasks/a<id>.output` | 65 — each byte-identical to a `subagents/agent-<id>.jsonl` under a DIFFERENT session dir | classify L187-188 → KindAgentTranscript | `handler/agent.go` |
| I. Workflow spool | `.../tasks/w<id>.output` | **0 on disk** | classify L192-193 | journal handler |
| J. Other session-dir children | `tool-results/*.txt` (678), `auto-mode-classifier-error.txt` (3), `MEMORY.md`, `feedback_*.md` | — | not discovered; not in scope of any frame family | none |

Other things present in a `wf_*/` dir: nothing else (exactly `journal.jsonl`, `agent-*.jsonl`, `agent-*.meta.json`). The `/tmp/.../<sess>/` dir holds only `tasks/` and `scratchpad/`; `tasks/` holds only `*.output`.

---

## A. `journal.jsonl` — inventory (344 records / 114 files)

Two shapes, exactly as the record §"EXHAUSTIVE JOURNAL SURVEY" (record L1046-1050) says:

| Key | Type | Count | Example |
|---|---|---|---|
| `type` | str ∈ {`started`:172, `result`:172} | 344 | `"started"` |
| `key` | str, always `v2:<sha256-hex>` | 344 | `"v2:5d71a63b…"` |
| `agentId` | str (17 hex) | 344 | `"a1e9158618036abe6"` |
| `result` | str (never object here; 172/172) | 172 | markdown report text |

Observed: 0 orphan starts in this population (record saw 35/524 — that was the older, larger set). No phase, no run-level record, no error record, no timestamps.

### Classification

| Field | Verdict | Detail |
|---|---|---|
| `type=started` | REPRESENTED (intent) / **MIS-REPRESENTED (actual)** | Record L1192-1196 and agent.proto L199-207: a `started` record is the announcement → `AgentWorkflowUpdate.agent_start` (`AgentSubagentStart`). **Actual** converter `convert/journal.go:33-34` renders it as `DetachedWorkProgressed.output = "started v2:…\n"` — a lossy string (conceded at journal.go:45-49). |
| `type=result` | REPRESENTED (intent) / MIS-REPRESENTED (actual) | Intent: the agent's `AgentSubagentSuccess.report.prose` via `AgentWorkflowUpdate.agent_frame`. Actual: `"result v2:…: <text>\n"` progress string. |
| `agentId` | REPRESENTED | `AgentSubagentStart.created_agent_id` (agent_activity.proto L1109) / `AgentFrame.agent_id`. Not read by the actual converter. |
| `key` | **DROPPED-WITH-REASON** (weak) | Step cache key (`v2:` sha). Nothing in workflow.proto names a step key; the record L1310-1312 says phase/step grouping "HAS NO PRODUCER" on the journal. Actual converter emits it inside the progress string. |
| `result` (text) | REPRESENTED | `AgentSubagentReport.prose` (L1256). Actual: string-rendered. |

Journal `key` sha is the only per-step identity; `AgentActivityId` for the spawn unit has no journal producer (see NO-PRODUCER).

---

## B/C. Workflow-agent transcript + `agent-<id>.meta.json` (172 files)

Meta keys (workflow agents):

| Key | Type | Count | Values |
|---|---|---|---|
| `agentType` | str | 172 | `workflow-subagent` only |
| `spawnDepth` | int | 172 | always `1` |
| `model` | str | 172 (record said "on 158" — now 172/172) | `opus`:166, `sonnet`:6 |
| `spawnedWithWorktree` | bool | 158 | `true` |
| `worktreePath` | str | 158 | `…/.claude/worktrees/wf_<id>-<n>` |

Classification (record L1052-1060, L1076-1078 says the meta "must be read alongside" each transcript — it is not; `MetaPath` is never consumed):

| Field | Verdict | Target |
|---|---|---|
| `agentType` | REPRESENTED, no producer wired | `AgentSubagentPrompt.subagent_type` (optional) |
| `model` | REPRESENTED, no producer wired | `AgentSubagentPrompt.requested_model` (`AgentModel`). Note it is the SHORT alias (`opus`), while `wf_*.json.workflowProgress[].model` carries the full id (`claude-opus-5[1m]`). |
| `spawnedWithWorktree` | REPRESENTED, no producer wired | `AgentSubagentPrompt.isolation = worktree` (L1172) |
| `worktreePath` | REPRESENTED, no producer wired | `AgentSubagentSuccess.worktree.path` (L1304-1308) |
| `spawnDepth` | **UNSUPPORTED** | No depth field; the flat model (agent.proto L159-160) states lineage is never maintained. Reasoned drop in the record's flat-model section, but not stated at a field. |
| — branch | NO PRODUCER | `AgentSubagentWorktree.branch`: workflow meta has no `worktreeBranch` (ordinary meta does). |
| prompt text | REPRESENTED, no producer wired | `AgentSubagentPrompt.text` = first `user` record of `agent-<id>.jsonl` (record L1055-1057) — but that transcript is **never tailed** (row B). |

---

## D. Ordinary `subagents/agent-<id>.meta.json` (2125 files)

| Key | Type | Count | Example / values |
|---|---|---|---|
| `agentType` | str | 2125 | general-purpose 1019, opus-medium 668, Explore 205, opus-low 114, claude 51, fork 34, sockets-listener 16, analysis-runner 10, claude-code-guide 5, Plan 2, statusline-setup 1 |
| `description` | str | 2125 | `"Create Go hello world"` |
| `spawnDepth` | int | 2125 | 1:1567, 2:499, 3:59 |
| `toolUseId` | str | 2125 | `toolu_01PNj…` |
| `model` | str | 768 | opus 534, haiku 100, sonnet 76, fable 58 |
| `parentAgentId` | str | 558 | 17-hex agent id |
| `spawnedWithWorktree` | bool | 405 | true |
| `worktreePath` | str | 449 | path |
| `worktreeBranch` | str | 449 | `worktree-agent-<id>` |
| `inheritedWorktreePath` | str | 91 | path |
| `isFork` | bool | 34 | true |
| `stoppedByUser` | bool | 25 | true |
| `cwd` | str | 17 | path |
| `worktreeCleanlyRemoved` | bool | 6 | true |

| Field | Verdict | Target / reason |
|---|---|---|
| `agentType` | REPRESENTED (wired elsewhere) | `AgentSubagentPrompt.subagent_type` — transcript's tool_use input also carries it (partition for transcripts). Meta is a second producer; unread. |
| `description` | REPRESENTED | `AgentSubagentPrompt.description`; also from transcript tool_use input and `toolUseResult.description` (`convert/detached.go:90`). |
| `toolUseId` | REPRESENTED | `AgentActivityId` of the spawn unit / `DetachedWorkStarted.origin_tool_call_id` (detached.go:73). Meta duplicates what the transcript states. |
| `model` | REPRESENTED | `AgentSubagentPrompt.requested_model`. Unread here. |
| `spawnedWithWorktree` | REPRESENTED | `isolation.worktree`. Unread. |
| `worktreePath` / `worktreeBranch` | REPRESENTED | `AgentSubagentSuccess.worktree{path,branch}` (L1304-1310). **This meta file is the only non-transcript producer of `branch`.** Unread. |
| `parentAgentId` | **UNSUPPORTED** by design | Flat model: "frames carry no ancestry" (agent_activity.proto L1105-1107; agent.proto L159-160). Nothing in the record cites this key by name. |
| `spawnDepth` | UNSUPPORTED | same flat-model reason. |
| `inheritedWorktreePath` | UNSUPPORTED | No field; `isolation.none` vs `worktree` cannot say "inherited parent's worktree". Not in record. |
| `isFork` | UNSUPPORTED | `subagent_type` could carry `"fork"` but this is a separate bool; no field. Not in record. |
| `stoppedByUser` | REPRESENTED (other producer) | `DetachedWorkEnded.cancelled` — wired from `toolUseResult{command,taskType,taskId}` (detached.go:133-150), not from meta. Meta is a durable second witness (25 cases) that would survive a lost transcript line. |
| `cwd` | UNSUPPORTED | No field. |
| `worktreeCleanlyRemoved` | UNSUPPORTED | No field; `AgentSubagentWorktree` has no lifecycle. Not in record. |

---

## E. `workflows/wf_<id>.json` — run record (114 files, **undiscovered**)

Written ONCE at completion: for all 114, `timestamp == startTime+durationMs == file mtime` (Δ ≤ 0.2 s), and `status` is `completed` for all. So it is a post-completion artifact, and a running run has no file here.

Top-level keys:

| Key | Type | Count | Example |
|---|---|---|---|
| `runId` | str | 114 | `wf_0fd9fc3e-26f` (== directory name) |
| `taskId` | str | 114 | `wtxycyxao` (never equal to runId) |
| `timestamp` | ISO-8601 str | 114 | `2026-08-11T03:42:54.323Z` |
| `startTime` | int ms | 114 | `1786419131021` |
| `durationMs` | int | 114 | `643302` |
| `status` | str | 114 | `completed` only |
| `workflowName` | str | 114 | `stuck-streaming-input-members` |
| `summary` | str | 114 | `meta.description` of the script |
| `script` | str | 114 | full script text (== file at `scriptPath`, 114/114) |
| `scriptPath` | str | 114 | exists 114/114; 26 point into the OTHER config root (same project slug) |
| `defaultModel` | str | 114 | `claude-opus-5`:62, `claude-fable-5`:52 |
| `agentCount` | int | 114 | 1 |
| `totalTokens` | int | 114 | 119568 |
| `totalToolCalls` | int | 114 | 59 |
| `logs` | list | 114 | always `[]` |
| `phases` | list[object] | 114 | `[{title, detail?}]` (`detail` on 10) |
| `result` | str:86 / list[str]:16 / list[{agent,report}]:10 / list[{branch,report,scope}]:2 | 114 | script's return value, shape is script-defined |
| `workflowProgress` | list[object] | 114 | see below |

`workflowProgress[]` entries (286):

| `type` | Keys | Count |
|---|---|---|
| `workflow_phase` | `index, title, type` | 114 |
| `workflow_agent` | `agentId, attempt, durationMs, index, isolation?, label, lastProgressAt, lastToolName, lastToolSummary, model, phaseIndex?, phaseTitle?, promptPreview, queuedAt, resultPreview, startedAt, state, tokens, toolCalls, type` | 172 |

Value sets: `state` = `done` only; `isolation` = `worktree` (158) or absent; `model` = `claude-opus-5`:82, `claude-opus-5[1m]`:84, `claude-sonnet-5`:6.

### Classification

| Field | Verdict | Target / reason |
|---|---|---|
| `status=completed` | **REPRESENTED, NO PRODUCER WIRED** | `AgentWorkflow.success.completed` (agent.proto L217-233). **Contradicts the record** L1070-1074 ("NOTHING… EVER SAYS THE RUN FINISHED… only possible source is the run leaving the live-background set"): this file says it, structurally, per run. |
| `summary` | REPRESENTED | `AgentWorkflowCompleted.summary.text` (workflow.proto L95-98). Note it is the script's static `meta.description`, not a closing account. |
| `workflowName` | REPRESENTED | `AgentWorkflowStart.name` (L19). Also from `toolUseResult.workflowName` (detached.go:98). |
| `scriptPath` | REPRESENTED | `AgentWorkflowScript.path` (L63). Record L1199-1203. |
| `script` | DROPPED-WITH-REASON | workflow.proto L60-62: "deliberately NOT carried on the wire". |
| `runId` | REPRESENTED | `AgentWorkflowPlacementLocal.run_id` (L75); record L1103-1118. |
| `taskId` | REPRESENTED | `DetachedWorkId.value` (detached_work.proto L122-127); record L1112-1116. |
| `startTime` | REPRESENTED | `AgentWorkflowStart.started_at.at_ms` (L54). **Only non-transcript producer of this instant** — the journal has no timestamps. |
| `durationMs` / `timestamp` | DROPPED-WITH-REASON | agent_activity.proto L486-492: no elapsed on the wire, derive from start. (Per-agent `durationMs` does map — below.) |
| `defaultModel` | UNSUPPORTED | No run-level model field on `AgentWorkflowStart`; only per-agent `requested_model`. Not in record. |
| `agentCount`, `totalTokens`, `totalToolCalls` | UNSUPPORTED | No run-level totals on `AgentWorkflowCompleted`. Subagent-level totals exist (`AgentSubagentTotals`) but the run aggregate has no field. Not in record. |
| `logs` | UNSUPPORTED (always empty) | No field; nothing to lose today. |
| `phases[]`, `workflowProgress[].phaseIndex/phaseTitle/type=workflow_phase` | **UNSUPPORTED — record's "no producer" claim is false** | Record L1310-1313: "PHASE GROUPING HAS NO PRODUCER… neither reaches any readable artifact". It reaches THIS artifact (`phases`, `phaseIndex`, `phaseTitle`, 54 agents tagged). Still no proto field, so UNSUPPORTED, but the stated reason is wrong. |
| `result` | UNSUPPORTED (shape is script-defined) | Nearest: `AgentWorkflowCompleted.summary`. Untyped polymorphic; could be carried verbatim as text. Not in record. |
| `workflowProgress[].agentId` | REPRESENTED | `AgentSubagentStart.created_agent_id`. |
| `workflowProgress[].label` | **REPRESENTED — record's "no producer" claim is false** | `AgentSubagentPrompt.description` (L1127-1132 + record L1062-1066 say "HAS NO PRODUCER on this path"). `label` IS the `agent()` call's label, recovered here for 172/172 agents. |
| `workflowProgress[].promptPreview` | DROPPED-WITH-REASON (truncated) | full text belongs to `AgentSubagentPrompt.text` from the transcript; preview is a strict prefix. |
| `workflowProgress[].model` | REPRESENTED | `AgentSubagentPrompt.requested_model` / `AgentSubagentSuccess.models_used`. Full model id (better than meta's alias). |
| `workflowProgress[].isolation=worktree` | REPRESENTED | `isolation.worktree`. |
| `workflowProgress[].state=done` | REPRESENTED | `AgentSubagent.success` (terminal). Only value observed. |
| `workflowProgress[].durationMs` | REPRESENTED | `AgentSubagentTotals.duration_ms` (L1274). |
| `workflowProgress[].tokens` | REPRESENTED (partially) | `AgentSubagentProgress.total_tokens` (L1207) / `AgentSubagentTotals.usage` needs a `TokenUsage` breakdown — only a single figure exists here. |
| `workflowProgress[].toolCalls` | REPRESENTED | `AgentSubagentTotals.tool_use_count` / `AgentSubagentProgress.tool_use_count`. |
| `workflowProgress[].lastToolName` | REPRESENTED | `AgentSubagentUpdate.activity.tool_name` (L1220-1229). |
| `workflowProgress[].lastToolSummary` | UNSUPPORTED | `AgentSubagentActivityLabel` is "A BARE NAME AND NOT A TYPED CALL" (L1224-1228) — deliberately no args/summary. Reasoned in proto, not in record. |
| `workflowProgress[].lastProgressAt` | UNSUPPORTED | No "last activity at" instant on `AgentSubagentUpdate`. |
| `workflowProgress[].startedAt` | REPRESENTED | `AgentSubagentStart.started_at`. |
| `workflowProgress[].queuedAt` | UNSUPPORTED | No queued/pending state for a subagent; `AgentSubagent` has start→update→terminal only. |
| `workflowProgress[].attempt` | UNSUPPORTED | No retry count anywhere. |
| `workflowProgress[].index` | UNSUPPORTED | Ordering; flat model. |
| `workflowProgress[].resultPreview` | DROPPED (prefix of `report.prose`). |
| `workflowProgress[].type` | n/a discriminator. |

---

## F. `workflows/scripts/<name>-wf_<id>.js` (91 files)

Line format: JS module; every file opens with `export const meta = { name, description, phases: [...] }` (91/91) followed by `await agent(...)` calls. Not a tailed record stream.

| Field | Verdict |
|---|---|
| path | REPRESENTED: `AgentWorkflowScript.path` |
| `meta.name` | REPRESENTED: `AgentWorkflowStart.name` (also in wf json / toolUseResult) |
| `meta.description` | REPRESENTED via wf json `summary` |
| `meta.phases` | UNSUPPORTED (see E) |
| body | DROPPED-WITH-REASON (workflow.proto L60-62) |

---

## G. Shell spool `tasks/b<id>.output` (1570 files)

Line format: raw bytes (all 1570 valid UTF-8; sizes 0 – 603,300 B, median 33 B; 0 over the 4 MiB poll bound). Exactly one structured line, at EOF:

| Terminator (last line) | Count |
|---|---|
| `\n[exited with code 0]\n` | 1525 |
| `\n[exited with code 1]\n` | 1 |
| `\n[killed]\n` | 4 |
| no terminator (still running / abandoned) | 40 |
| **`EXIT=<n>\n` as final line** | **0** |

`EXIT=` appears in 4 files total; 2 at line start (`EXIT=0` mid-file at line 5 of 7 in one, `EXIT=3` three times mid-file in another, both script output), 2 mid-line only. Marker-ending files span 2026-08-19 → 2026-08-22, i.e. the whole current population.

### Classification

| Field | Verdict | Detail |
|---|---|---|
| raw bytes | REPRESENTED | `AgentBashUpdate.new_output` (agent_activity.proto L941-963). Actual: `DetachedWorkProgressed.output` (`handler/shell.go:71`). |
| byte offset | REPRESENTED | `AgentBashUpdate.from_offset` — `Attribution.Offset` (handler.go:40-50) is available; not placed on the actual payload. |
| `[exited with code N]` | **UNSUPPORTED by the current parser; REPRESENTED by the proto** | Proto: `AgentBash.success.completed` (code 0) — and the proto has NO exit-code field by design (L965-971, record L2070-2072). `handler/shell.go:15,122-157` parses only `EXIT=<digits>`, which matches **0 of 1570** spools. Consequence: `DetachedExited` (`convert/detached.go:173`) never fires today; every background shell ends only via the staleness LOST sweep (`stale.go:286-297`), the exact "wrong verdict" shell.go:29-33 says it exists to prevent. The `EXIT=` statistics in shell.go:90-121 (234 spools, 21 markers) describe a population no longer on disk. |
| `[killed]` | **UNSUPPORTED** | Nearest proto arm: `AgentBashSuccess.interrupted` (L989-993) / `DetachedWorkEnded.cancelled`. No parser. Not in record. |
| non-zero code | REPRESENTED (lossy) | `AgentBash.success.completed` per L965-971; actual `DetachedFailed.summary="exited with status N"` (detached.go:181-183), which the proto says is the wrong arm ("A NONZERO EXIT IS STILL THIS [success] ARM"). |
| stdout vs stderr | **UNSUPPORTED** | Spool interleaves both; `AgentBashOutputText.stdout/stderr` (L1011-1022) cannot be split from a spool. Proto L1018-1021 says no producer states an interleaving — the spool is exactly such a producer and loses the split. |
| `from_offset` gap detection on restart | NON-STATIC (see §K). |

---

## H. Agent spool `tasks/a<id>.output` (65 files)

Byte-identical copy of `subagents/agent-<id>.jsonl` (verified `cmp` on a sample), living under the RUNTIME session dir (discover.go L9-21). Same JSONL shape as transcripts (transcript partition). Notes for this partition:
- Discovered as `KindAgentTranscript` with `MetaPath` **empty** (classifySpool L181-185 sets no MetaPath), so even if meta were read, spool-sourced agents would have none.
- Overlap with workflow-agent ids: **0 of 86** — workflow agents get no spool, so row B's transcripts are reachable by no path at all.
- Double-ingestion hazard: the same bytes arrive via the config-root path (SessionID from path) and the spool path (SessionID resolved by owner index, `owner.go:98`). De-dup relies on the store's write identity; out of scope here but worth a sibling partition's confirmation.

## I. Workflow spool `tasks/w<id>.output` — 0 files. `classifySpool` L192-193 and `main.go:615-617` route it to the journal handler; no evidence the harness ever writes one (every run's journal lives in the config root). Dead arm.

---

## J. Consolidated lists

### J.1 UNSUPPORTED (field exists on disk, no proto field)

1. `agent-*.meta.json`: `spawnDepth`, `parentAgentId`, `inheritedWorktreePath`, `isFork`, `cwd`, `worktreeCleanlyRemoved`.
2. `wf_*.json`: `defaultModel`, `agentCount`, `totalTokens`, `totalToolCalls`, `logs`, `phases[]`, `result` (polymorphic), `workflowProgress[].{phaseIndex,phaseTitle,lastToolSummary,lastProgressAt,queuedAt,attempt,index}`.
3. `b*.output`: `[killed]` terminator; stdout/stderr separation (lost, not representable from a spool).
4. Script `meta.phases`.

### J.2 NO-PRODUCER (proto field on these families, nothing in any tailed-or-should-be-tailed file states it)

1. `AgentWorkflow.failure.{script_rejected, run_ended}` — no file ever records a failed run (`status` is only `completed`; a rejected script leaves no `wf_*.json`). Only possible source: the launch `toolUseResult` (transcript partition) or live-set departure. Confirms record L1070-1074 for failure.
2. `AgentWorkflow.success.interrupted` — no file; a `TaskStop` on a workflow is only in the transcript `toolUseResult`.
3. `AgentWorkflow.success.completed` — **HAS a producer** (`wf_*.json.status=completed`), contrary to the record. Unwired.
4. `AgentWorkflowStart.resumed_from` — no key in `wf_*.json`, journal, or script (no `resumeFromRunId` observed anywhere on disk).
5. `AgentWorkflowStart.placement.remote.session_url` — no remote run on disk; `AgentWorkflowNotice` — no file carries a notice.
6. `AgentSubagentWorktree.branch` for WORKFLOW agents — workflow meta lacks `worktreeBranch` (ordinary meta has it).
7. `AgentSubagentTotals.usage` (full `TokenUsage`) from non-transcript files — only a single `tokens` integer exists (`wf_*.json`); breakdown needs the agent transcript's `usage` objects.
8. `AgentSubagentTotals.tool_stats` — no file breaks tool calls down by kind.
9. `AgentSubagentFailure` — `workflowProgress[].state` is only `done`; meta has no failure flag.
10. `AgentSubagentUpdate.note` — no file carries a subagent's free-text note.
11. `AgentSubagentPrompt.requested_name` — not in any meta file.
12. `AgentBashStart.{command, started_at}` on the spool's own stream — the spool carries neither; both must come from the transcript's tool_use (cross-file, see K).
13. `AgentBashOutputPartial.bytes_omitted`, `AgentBashOutputImage` — no spool producer.
14. `DetachedWorkDetached.cause.{by_user, timed_out.timeout_ms}` — no file distinguishes; the spool exists for all three causes identically.
15. `AgentActivityStartedAt` for each journal step — journal has no timestamps (only `wf_*.json.workflowProgress[].startedAt`, written post-completion).

### J.3 NON-STATIC resolutions (anything not a constant keyed lookup on the current line)

1. **Run-finished is a different file** — `AgentWorkflow.success` needs `<sess>/workflows/wf_<id>.json`, reached from the journal only by joining `runId` across directories (`subagents/workflows/wf_X/` ↔ `workflows/wf_X.json`). Static per key, but a cross-file read.
2. **Meta join** — every `AgentSubagentStart` field except `created_agent_id` needs `agent-<id>.meta.json` beside the transcript (record L1076-1078). A sibling-file read keyed by the filename; for a spool-sourced agent (`a*.output`) there is no sibling at all (classifySpool sets no MetaPath), so the resolution depends on WHICH path discovered the file.
3. **Prompt text** — `AgentSubagentPrompt.text` for a workflow agent is "the first user message of its own transcript" (record L1055-1057): an order-dependent read (first line), and of a file that is not discovered (row B).
4. **Terminator detection is batch-relative** — `trailingExitCode` (shell.go:122-135) requires the marker to be the last line of the current poll batch and rejects a batch starting mid-line with `batchOffset != 0`; correctness depends on poll boundaries, not on the line alone (documented at shell.go:116-121 as an accepted hole).
5. **`from_offset` continuity** — `AgentBashUpdate.from_offset` "MUST equal the number of bytes the consumer has already accumulated" (L951-957); after a sidecar restart this needs the prior cursor, i.e. previous-state.
6. **Spool owner** — a spool's session is NOT in its path (discover.go L9-21); `owner.go:98 resolveOwnerResult` resolves it from the transcript that launched the task id — a cross-file, order-dependent dependency (launch must be ingested first; `MayArrive` L51 exists for the race).
7. **Journal `started` → announcement** needs the agent's type/model/isolation from meta (2) AND its label from `wf_*.json` (which does not exist until the run completes) — so a live announcement can carry `description` only after the fact.
8. **Journal duplicates across roots** — both config roots hold byte-identical `wf_*` trees for the same session id (diff -rq clean, 57 each); `Scan` (L82-90) unions both roots, so each record converts twice to the same `DetachedWorkMessageID`; uniqueness is left to the store's write identity (`MessageEntry(at, "detached_progress", m)` at detached.go:163 keys on offset+path, so they are NOT deduplicated — two distinct paths).
9. `scriptPath` points into the OTHER config root for 26/114 runs — any reader of `AgentWorkflowScript.path` that validates existence must search both roots.
10. `trailingExitCode` itself is static per batch but its `EXIT=` premise is stale (G) — resolution today is always "no marker", falling to the time-based `stale.go` sweep (`ShellSilence`), which is NON-STATIC by construction.

### J.4 Record claims contradicted by the files

| Record claim | Where | Observed |
|---|---|---|
| "NOTHING IN A JOURNAL EVER SAYS THE RUN FINISHED… only possible source is the live-background set" | L1070-1074 | True of the journal; false of the run: `workflows/wf_<id>.json.status=completed` + `durationMs` + `summary`, 114/114. |
| "`description` HAS NO PRODUCER on this path" | L1062-1066, proto L1127-1132 | `workflowProgress[].label` 172/172 (post-completion). |
| "PHASE GROUPING HAS NO PRODUCER… neither reaches any readable artifact" | L1310-1313 | `phases[]`, `phaseIndex`, `phaseTitle` in `wf_<id>.json`. |
| "per-agent meta holds only `{agentType, spawnDepth, model}`" | L1313 | plus `spawnedWithWorktree`, `worktreePath` (158/172); ordinary meta has 14 keys. |
| Spool "terminated by its `EXIT=<code>` line" | L2063, shell.go:13-15 | Terminated by `[exited with code N]` / `[killed]`; `EXIT=` terminates 0/1570. |
| "The sidecar tails exactly four kinds of file" | L1932-1935 | Three more kinds it should (B, E, F) and one dead arm (I). |
