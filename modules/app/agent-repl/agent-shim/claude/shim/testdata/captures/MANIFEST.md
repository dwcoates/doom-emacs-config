# Capture goldens MANIFEST

Real, unedited recordings of supervised runs against the ACTUAL agent binary
(the one sanctioned exception to the no-vendor-calls rule, taken by
`scripts/capture`). Nothing here is transcribed or hand-written: a converter
that only agrees with our own idea of the vendor's shapes fails against these
rather than in production.

Same contract as `testdata/corpus/MANIFEST.md`: every fixture is a real
artifact, every row says where it came from and what it is the golden for, and
a scenario without a row here fails `test/convert/goldens/fixtures.test.ts`.

- Capture run: 2026-09-01 / 2026-09-02 (per-scenario dates below).
- Source: `~/.config/doom-overhaul/captures/<scenario>/`, committed selectively.
- WHAT IS COMMITTED per scenario:
  - `stream.jsonl` — every recorded line, `dir: "sdk"` (what the SDK handed the
    shim, and the only thing the fold consumes) interleaved with
    `dir: "control"` (what the shim asked of it: the query options, the turns
    submitted, the `canUseTool` round trips).
  - `meta.json` — the run's own record: prompt, expectations, vendor session id,
    capture time, failures (all empty; a failed run is never a golden).
  - `files/projects/<slug>/**` — the vendor's transcript for the run, plus each
    subagent's `agent-<17-hex>.jsonl` and `.meta.json`.
  - `files/spool*/**/*.output` for the `bash-*` scenarios ONLY, because the
    spill suite reads one. Every other scenario's spools are DROPPED: the
    `turn-stop-max-turns` spool alone is 241 MB.
- NOT committed, ever: `_failed/` and `_inflight/` runs.
- No anonymization walker ran over these: the capture harness runs in a
  throwaway temp cwd with no credentials, and every path in them is that temp
  directory. Structural fields — uuids, session ids, tool-use ids, agent ids,
  upsert keys — are preserved verbatim, which is the whole point.

## Evidence gaps (arms NO capture exercises)

The run's model chose `Bash` and `Skill` where the contract has a typed arm, so
these `AgentActivity` arms have no golden at all and must not be asserted from
an invented fixture: `glob`, `grep`, `artifact`, `scheduleWakeup`, `worktree`,
`contextInjected`, `sendMessage` on a subagent, and every `AgentQuestion` /
`AgentPermission` frame (those are the engine gate's, not the fold's). Recorded
negatives from the same run: no `context_tip` or budget-warning attachment (the
nearest carrier observed is a `total_tokens_reminder` attachment), no failed
subagent, and no `compact_boundary` / `isCompactSummary` record anywhere —
`/compact` answered `Not enough messages to compact.`

Three TURN-STOP FAILURE terminals are DECLARED-ONLY — the mock keeps them
because `sdk.d.ts` declares them, but no capture reaches them, so nothing here
grounds the pairing and none may be asserted from a golden:

- `AgentFailure.execution_error` — `turn-stop-error-during-execution` ended
  `success.interrupted` (the run aborted its streaming rather than raising).
- `AgentFailure.stop_hook_prevented` — `turn-stop-hook-stop` ended
  `success.completed`; the Stop hook did not prevent continuation.
- `AgentFailure.structured_output_retry_exhausted` —
  `turn-stop-max-structured-output-retries` ended `success.completed`; the
  retries never exhausted.

Their `!fail-execution`, `!fail-stop-hook` and `!fail-structured-output` rows
in the shim's AGENTS.md scenario table carry the same DECLARED-ONLY mark.

| Scenario | Captured | Golden for (unit kinds → terminal) | Notes | Size |
|---|---|---|---|---|
| `account-usage` | 2026-09-02 | `hook`, `thinking`, `response` → `success.completed` | single turn | 44 KB |
| `artifact-publish-and-list` | 2026-09-01 | `hook`, `thinking`, `read`, `bash`, `response` → `success.completed` | single turn | 188 KB |
| `bash-detached` | 2026-09-02 | `hook`, `thinking`, `bash`, `response` → `success.completed` | single turn | 52 KB |
| `bash-foreground-completed` | 2026-09-01 | `hook`, `thinking`, `bash`, `response` → `success.completed` | single turn | 56 KB |
| `bash-image-output` | 2026-09-01 | `hook`, `thinking`, `bash`, `read`, `response` → `success.completed` | single turn | 92 KB |
| `bash-interrupted-by-timeout` | 2026-09-01 | `hook`, `thinking`, `bash`, `response` → `success.completed` | single turn | 60 KB |
| `bash-nonzero-exit` | 2026-09-01 | `hook`, `thinking`, `bash`, `response` → `success.completed` | single turn | 52 KB |
| `bash-partial-output-with-spill` | 2026-09-01 | `hook`, `thinking`, `bash`, `read`, `response` → `success.completed` | single turn | 1.8 MB |
| `compaction-directed` | 2026-09-02 | `hook`, `thinking`, `response` → `success.completed` | 3 turn terminals | 84 KB |
| `context-budget-warning` | 2026-09-01 | `hook`, `thinking`, `response`, `bash`, `read` → `success.completed` | single turn | 120 KB |
| `context-injected-memory` | 2026-09-01 | `hook`, `thinking`, `response` → `success.completed` | single turn | 40 KB |
| `context-injected-skills` | 2026-09-01 | `hook`, `thinking`, `response` → `success.completed` | single turn | 56 KB |
| `context-usage` | 2026-09-02 | `hook`, `thinking`, `read`, `response` → `success.completed` | single turn | 96 KB |
| `cron-create-list-delete` | 2026-09-01 | `hook`, `thinking`, `response`, `cron` → `success.completed` | single turn | 108 KB |
| `ctrl-b-detach-of-foreground-subagent` | 2026-09-01 | `hook`, `thinking`, `subagent`, `bash`, `response` → `success.completed` | single turn | 92 KB |
| `ctrl-b-detach-of-foreground-work` | 2026-09-01 | `hook`, `thinking`, `bash`, `response`, `read` → `success.completed` | single turn | 108 KB |
| `diagnostics` | 2026-09-01 | `hook`, `thinking`, `response` → `success.completed` | single turn | 116 KB |
| `edit` | 2026-09-01 | `hook`, `thinking`, `read`, `edit`, `response` → `success.completed` | single turn | 80 KB |
| `fan-wide-cancel` | 2026-09-02 | `hook`, `thinking`, `bash`, `response` → `success.completed` | single turn | 96 KB |
| `fast-mode` | 2026-09-01 | `hook`, `thinking`, `response` → `success.completed` | single turn | 40 KB |
| `glob` | 2026-09-01 | `hook`, `thinking`, `bash`, `response` → `success.completed` | single turn | 64 KB |
| `grep-content-files-count` | 2026-09-01 | `hook`, `thinking`, `bash`, `response` → `success.completed` | single turn | 80 KB |
| `held-turn-gate` | 2026-09-02 | `hook`, `thinking`, `bash`, `response` → `success.completed` | single turn | 76 KB |
| `hook-blocked` | 2026-09-01 | `hook`, `thinking`, `bash`, `response` → `success.completed` | single turn | 60 KB |
| `hook-cancelled` | 2026-09-01 | `hook`, `thinking`, `bash`, `response` → `success.completed` | single turn | 56 KB |
| `hook-failed` | 2026-09-01 | `hook`, `thinking`, `bash`, `response` → `success.completed` | single turn | 56 KB |
| `hook-succeeded` | 2026-09-01 | `hook`, `thinking`, `bash`, `response` → `success.completed` | single turn | 52 KB |
| `ide-diagnostics-after-edit` | 2026-09-01 | `hook`, `thinking`, `response`, `read`, `edit`, `bash` → `success.completed` | single turn | 156 KB |
| `identity-rotation-clear` | 2026-09-02 | `hook`, `thinking`, `response` → `success.completed` | 3 turn terminals | 116 KB |
| `interrupt` | 2026-09-01 | `hook`, `thinking`, `response` → `success.interrupted` | single turn | 36 KB |
| `max-tokens` | 2026-09-01 | `hook`, `response` → `success.completed` | single turn | 40 KB |
| `mcp-server-healths` | 2026-09-02 | `hook`, `thinking`, `bash`, `response` → `success.completed` | single turn | 100 KB |
| `mcp-unmodeled-tool` | 2026-09-01 | `hook`, `thinking`, `response`, `unmodeled` → `success.completed` | single turn | 88 KB |
| `model-changed` | 2026-09-02 | `hook`, `thinking`, `response` → `success.completed` | single turn | 72 KB |
| `monitor-deadline` | 2026-09-01 | `hook`, `thinking`, `bash`, `monitor`, `response` → `success.completed` | single turn | 92 KB |
| `monitor-persistent` | 2026-09-01 | `hook`, `thinking`, `monitor`, `response` → `success.completed` | single turn | 84 KB |
| `permission-allow-once` | 2026-09-01 | `hook`, `thinking`, `bash`, `response` → `success.completed` | single turn | 60 KB |
| `permission-allow-standing` | 2026-09-01 | `hook`, `thinking`, `response`, `bash` → `success.completed` | single turn | 60 KB |
| `permission-denied-by-policy` | 2026-09-01 | `hook`, `thinking`, `bash`, `response` → `success.completed` | single turn | 60 KB |
| `permission-denied-by-user` | 2026-09-02 | `hook`, `thinking`, `bash`, `response` → `success.completed` | single turn | 52 KB |
| `permission-mode-changed` | 2026-09-02 | `hook`, `thinking`, `response` → `success.completed` | single turn | 32 KB |
| `permission-undecidable-parked` | 2026-09-02 | `hook`, `thinking`, `bash` → `success.interrupted` | single turn | 32 KB |
| `plan-mode-enter-exit` | 2026-09-01 | `hook`, `thinking`, `planMode`, `subagent`, `response`, `bash` → `success.completed` | single turn | 116 KB |
| `prose-streamed` | 2026-09-01 | `hook`, `thinking`, `response` → `success.completed` | single turn | 56 KB |
| `push-notification-not-sent` | 2026-09-01 | `hook`, `thinking`, `pushNotification`, `response` → `success.completed` | single turn | 92 KB |
| `push-notification-sent` | 2026-09-01 | `hook`, `thinking`, `pushNotification`, `response` → `success.completed` | single turn | 76 KB |
| `question-free-text` | 2026-09-01 | `hook`, `thinking`, `response` → `success.completed` | single turn | 64 KB |
| `question-multi-select` | 2026-09-01 | `hook`, `thinking`, `response` → `success.completed` | single turn | 60 KB |
| `question-multiple-in-one-batch` | 2026-09-01 | `hook`, `thinking`, `response` → `success.completed` | single turn | 68 KB |
| `question-single-select` | 2026-09-01 | `hook`, `thinking`, `response` → `success.completed` | single turn | 92 KB |
| `question-unanswered` | 2026-09-01 | `hook`, `thinking`, `response` → `success.completed` | single turn | 60 KB |
| `read-whole-head-range` | 2026-09-01 | `hook`, `thinking`, `response`, `read` → `success.completed` | single turn | 84 KB |
| `report-findings` | 2026-09-01 | `hook`, `thinking`, `read`, `reportFindings`, `response` → `success.completed` | single turn | 80 KB |
| `schedule-wakeup-schedule-and-stop` | 2026-09-01 | `hook`, `thinking`, `skillUse`, `bash`, `response` → `success.completed` | single turn | 148 KB |
| `send-message-queued-and-resumed` | 2026-09-01 | `hook`, `thinking`, `response`, `subagent`, `sendMessage`, `bash` → `success.completed` | single turn | 140 KB |
| `skill-invocation` | 2026-09-01 | `hook`, `thinking`, `skillUse`, `read`, `response` → `success.completed` | single turn | 76 KB |
| `subagent-detached` | 2026-09-02 | `hook`, `thinking`, `subagent`, `bash`, `response` → `success.completed` | single turn | 92 KB |
| `subagent-sync-nested-activity` | 2026-09-01 | `hook`, `thinking`, `subagent`, `bash`, `read`, `response` → `success.completed` | single turn | 104 KB |
| `task-acts-create-change-reject` | 2026-09-01 | `hook`, `thinking`, `taskAct`, `response` → `success.completed` | single turn | 144 KB |
| `turn-stop-error-during-execution` | 2026-09-01 | `hook`, `thinking`, `response` → `success.interrupted` | single turn | 32 KB |
| `turn-stop-hook-stop` | 2026-09-01 | `hook`, `thinking`, `response` → `success.completed` | residue `vendor_specific/system/notification` | 188 KB |
| `turn-stop-max-budget-usd` | 2026-09-01 | `hook`, `thinking`, `response` → `failure.budgetExhausted` | single turn | 60 KB |
| `turn-stop-max-structured-output-retries` | 2026-09-01 | `hook`, `thinking`, `response`, `unmodeled` → `success.completed` | single turn | 128 KB |
| `turn-stop-max-turns` | 2026-09-01 | `hook`, `thinking`, `bash` → `failure.maxTurns` | single turn | 88 KB |
| `vendor-answered-slash-commands` | 2026-09-01 | `hook`, `response` → `success.completed` | single turn | 44 KB |
| `web-fetch` | 2026-09-01 | `hook`, `thinking`, `webFetch`, `response` → `success.completed` | single turn | 72 KB |
| `web-search` | 2026-09-01 | `hook`, `thinking`, `webSearch`, `response` → `success.completed` | single turn | 76 KB |
| `worktree-enter-exit-kept-and-removed` | 2026-09-01 | `hook`, `thinking`, `response`, `skillUse`, `read`, `bash`, `write` → `success.completed` | residue `vendor_specific/system/vcs_state_changed` | 664 KB |
| `write-created-and-updated` | 2026-09-01 | `hook`, `thinking`, `read`, `write`, `response` → `success.completed` | single turn | 92 KB |

## `e2ecleanup/fakesdk-ext` additions (test tooling, not new captures)

The daemon/e2e event-fabrication inventory
(`docs/overhaul/reports/E2E-EVENT-INVENTORY.md`'s "Consolidated PROPOSED
fake-SDK additions") asked for several new scenarios and scenario options so
`daemon/e2e` can drive the real fake SDK instead of hand-fabricating
`protocolv1.Event{...}` records. None of these is a NEW capture directory —
each is graded against the grounding named below, or marked ungrounded.

- **`!slash-shape-a` / `!slash-shape-a-unnamed`** (`src/fake/scenarios/session.ts`).
  UNGROUNDED, INVENTED: no capture in this manifest exercises the CLI's own
  slash-command bookkeeping as a raw `user`-typed transcript record (the
  nearest real artifact, `vendor-answered-slash-commands`, is the VENDOR
  answering the slash command itself — a different record, already covered by
  `!slash`). The shape is instead built from `machinery_e2e_test.go`'s
  `machineryContent`/`unnamed` constants, which the daemon/e2e suite has
  exercised as real vendor transcript bytes since before this fake-SDK
  addition existed. Retires: `machinery_e2e_test.go`'s and
  `slashdurability_e2e_test.go`'s hand-fabricated Shape-A call sites.

- **`!compact [summary]`'s summary override** (`src/fake/scenarios/session.ts`).
  GROUNDED: `compaction-directed` (this manifest, above) is the golden for the
  whole `!compact` shape; `ContextCompacted.Summary`'s DERIVATION (the
  assistant prose immediately following the boundary) is unchanged, only the
  fixed conclusion string is now the prompt's own argument when one is given.
  Retires every `sidecarCompactEvent(..., summary)` call site across
  `clearcompact_e2e_test.go`, `phaseword_e2e_test.go`, `revive_e2e_test.go`,
  `revivalhold_e2e_test.go` and `slashdurability_e2e_test.go`.

- **`!context-budget-warning`** (`src/fake/scenarios/session.ts`). UNGROUNDED,
  INVENTED, ORCHESTRATOR RULING (pending the project lead's): no capture in
  this manifest — including the one literally NAMED `context-budget-warning`,
  which the "Evidence gaps" section above and `golden-conformance.test.ts`'s
  own `EXCLUDED` entry both record as holding no budget-warning record of any
  kind — carries this record. `!context-tip` and `!tokens-reminder` are
  UNCHANGED and still land 5's ruling (a generic `/goal` tip and the one
  observed `total_tokens_reminder`, neither the budget warning): this is a
  SEPARATE, separately-named producer, added only so the converter's
  ALREADY-BUILT `context_budget_warning` arm
  (`test/convert/attachments.test.ts`) has a fake-SDK path to drive it from,
  pending a real grounding capture.

- **`!skill [skill-name] [args]` parameterization** (`src/fake/scenarios/skills.ts`).
  GROUNDED: `skill-invocation` (this manifest, above) remains the golden for
  the SHAPE (tool_use → `{success, commandName, allowedTools}` ack → isMeta
  document) — unchanged. Only the fixed `"fake-skill"` name/args/document body
  are now derived from the prompt's own argument (first token = skill name,
  rest = args; the document body is templated on the name), so a caller can
  name e.g. `create-or-update-workspace` with args `merge`. Retires
  `mergewindow_e2e_test.go`'s `mergeSkillCallLine` fabrication.

- **`!bash-detach-poll`** (`src/fake/scenarios/shell.ts`). UNGROUNDED,
  INVENTED: `TaskOutput` is a declared vendor tool (every capture's
  `init.tools` lists it), but NO capture ever calls it — every recorded
  backgrounded run was checked by re-reading its spool path, never by an
  explicit retrieval call. The poll's `toolUseResult` shape
  (`retrievalStatus`/`task{taskId,taskType,status,description,output,
  exitCode,exitCodeSet}`) mirrors the daemon/e2e Go harness's own invented
  `bashTaskOutcome`, since no vendor recording exists to spell it from. Note:
  `TaskOutput` is not in `src/convert/tools/registry.ts`, so these tool_use/
  tool_result pairs fold to `AgentUnmodeled` in the current converter — this
  addition is test-tooling only and does not add a converter arm. Retires
  `detachedworksettle_e2e_test.go`'s and `detachedspooloffset_e2e_test.go`'s
  explicit-poll fabrication.
