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
subagent. The original `compaction-directed` run's `/compact` answered
`Not enough messages to compact.`; that golden was RE-CAPTURED 2026-09-03 (see
the row below) by lengthening the shared world's own prompt list until
`/compact` had enough transcript to actually compact, so this manifest's
`compaction-directed` row is now a real `compact_boundary` capture, not a
`Not enough messages` non-event.

NO CAPTURE CAN GROUND THE `diagnostics` ATTACHMENT, on this or any host. The
record only exists when an editor integration is connected to the scratch cwd,
which is why `ide-diagnostics-after-edit` — the capture named for it — holds no
attachment at all: the run shelled out to `tsc` instead. The attachment's shape
is grounded in `testdata/corpus/attachments/diagnostics.jsonl` (a real harvested
attachment) on both of the mock's diagnostics scenarios alike. `!ide-diagnostics`
composes it with that capture's real `Edit` result; `!ide-diagnostics-write`
composes it with the real `Write` `type: "create"` result recorded in
`write-created-and-updated`, so the fold's write arm has the same grade of
evidence its edit arm has. `!ide-diagnostics-write` therefore has NO capture
directory and no row in the table below, and must not be given one.

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

| Scenario | Captured | Golden for (unit kinds → terminal) | Scenarios: (registered `!<name>`(s) that reproduce it) | Notes | Size |
|---|---|---|---|---|---|
| `account-usage` | 2026-09-02 | `hook`, `thinking`, `response` → `success.completed` | `!usage-full` (same shape as `!usage-available`) | single turn | 44 KB |
| `artifact-publish-and-list` | 2026-09-01 | `hook`, `thinking`, `read`, `bash`, `response` → `success.completed` | `!artifact-publish` + `!artifact-list` | single turn | 188 KB |
| `auto-compaction` | 2026-09-04 (Haiku) | `hook`, `thinking`, `read`, `response` → `success.completed` | `!compact-auto` | GROUNDED. 16 turn terminals, one per paced read. The two 2026-09-03 attempts tried to force this with `.claude/settings.local.json`'s `autoCompactWindow` and got nothing (occupancy never left ~45k); this run instead PACES real occupancy — sixteen turns, each reading one 40000-byte file (~10k tokens) in full — so the window climbs ~10k a turn and the vendor compacts BETWEEN turns rather than the run blowing past the window inside one turn. Real `compact_boundary` with `compact_metadata{trigger:"auto", pre_tokens:165716, post_tokens:13675, cumulative_dropped_tokens:152041, duration_ms:18695, preserved_segment{head_uuid, anchor_uuid, tail_uuid}, preserved_messages{anchor_uuid, uuids, all_uuids}}` — the SAME field set as the manual `compaction-directed`, `trigger` being the only discriminator. THREE shapes this capture settles, each AGREEING with `compaction-directed` so neither is a one-run accident: (1) the compaction's END is a `system:status` carrying `status: null` AND `compact_result: "success"`; (2) `preserved_segment.head_uuid`/`tail_uuid` are the FIRST and LAST entries of `preserved_messages.uuids` (a real multi-message span) while `anchor_uuid` is a uuid of its own appearing in NEITHER list; (3) `logical_parent_uuid` equals the preserved TAIL, not the head. The fake collapsed all five uuids onto the transcript head and pointed `logical_parent_uuid` at the head; both are fixed (`src/fake/scenarios/session.ts`'s `preservedUuids`, applied to `!compact` and `!compact-auto` alike), and `!compact-auto`'s previously invented token/duration figures are now this capture's real ones. | 872 KB |
| `bash-detached` | 2026-09-02 | `hook`, `thinking`, `bash`, `response` → `success.completed` | `!bash-detach` | single turn | 52 KB |
| `bash-foreground-completed` | 2026-09-01 | `hook`, `thinking`, `bash`, `response` → `success.completed` | `!bash` | single turn | 56 KB |
| `bash-image-output` | 2026-09-01 | `hook`, `thinking`, `bash`, `read`, `response` → `success.completed` | `!bash-image` | single turn | 92 KB |
| `bash-interrupted-by-timeout` | 2026-09-01 | `hook`, `thinking`, `bash`, `response` → `success.completed` | `!bash-timeout` | single turn | 60 KB |
| `bash-nonzero-exit` | 2026-09-01 | `hook`, `thinking`, `bash`, `response` → `success.completed` | `!bash-fail` | single turn | 52 KB |
| `bash-partial-output-with-spill` | 2026-09-01 | `hook`, `thinking`, `bash`, `read`, `response` → `success.completed` | `!bash-spill` | single turn | 1.8 MB |
| `compaction-directed` | 2026-09-03 (re-captured; Haiku) | `hook`, `thinking`, `response` → `success.completed` | `!compact` | GROUNDED. 9 turn terminals (6 filler turns added so `/compact` has enough transcript to actually compact — the 2026-09-02 run's `/compact` had only 3 turns and answered `Not enough messages to compact.`); real `compact_boundary` with `compact_metadata{trigger:"manual", pre_tokens:48374, post_tokens:3759, cumulative_dropped_tokens:44615, duration_ms:45767, preserved_segment{head_uuid, anchor_uuid, tail_uuid}, preserved_messages{anchor_uuid, uuids, all_uuids}}`, plus a `logical_parent_uuid` naming the preserved head; no `preCompactDiscoveredTools` field anywhere on either plane (the fake used to invent one; fixed) and no `system:local_command_output` line anywhere in the run; the real post-boundary summary is a plain `user`-role message (not assistant prose) beginning "This session is being continued from a previous conversation that ran out of context. The summary below covers the earlier portion of the conversation.\n\nSummary:\n1. Primary Request and Intent: ..." | 196 KB |
| `context-budget-warning` | 2026-09-01 | `hook`, `thinking`, `response`, `bash`, `read` → `success.completed` | `!context-budget-warning` — NAME MATCHES, GROUNDING DOES NOT: the capture holds no budget-warning record of any kind (see Evidence gaps); UNGROUNDED | single turn | 120 KB |
| `context-injected-memory` | 2026-09-01 | `hook`, `thinking`, `response` → `success.completed` | `!memory` | single turn | 40 KB |
| `context-injected-skills` | 2026-09-01 | `hook`, `thinking`, `response` → `success.completed` | `!skills-injected` | single turn | 56 KB |
| `context-usage` | 2026-09-02 | `hook`, `thinking`, `read`, `response` → `success.completed` | `!context-usage-drift` | single turn | 96 KB |
| `cron-create-list-delete` | 2026-09-01 | `hook`, `thinking`, `response`, `cron` → `success.completed` | `!cron` | single turn | 108 KB |
| `ctrl-b-detach-of-foreground-subagent` | 2026-09-01 | `hook`, `thinking`, `subagent`, `bash`, `response` → `success.completed` | `!subagent` — REACHABLE-ONLY: no scenario backgrounds a subagent the way a real vendor-side detach would; RULED out of scope (PROTO-CHANGES.md Landing 8) | single turn | 92 KB |
| `ctrl-b-detach-of-foreground-work` | 2026-09-01 | `hook`, `thinking`, `bash`, `response`, `read` → `success.completed` | `!vendor-backgrounded` — REACHABLE-ONLY: parks a live foreground Bash call but nothing can make a real DetachForeground resolve it; RULED out of scope (PROTO-CHANGES.md Landing 8) | single turn | 108 KB |

These two capture directory names are historical: they predate the owner
ruling (2026-09-04, PROTO-CHANGES.md Landing 8) that dropped "ctrl-b"/"Ctrl-B"
naming from the mocked vendor's scenario inventory (the fake scenario is now
`vendor-backgrounded`). Renaming the recorded capture directories themselves
would be pure churn, so they keep their original names.
| `diagnostics` | 2026-09-01 | `hook`, `thinking`, `response` → `success.completed` | (none registered — reproduced by ANY plain prompt falling through to the default `""` prose scenario; the e2e test asserts by absence of a topbar warning) | single turn | 116 KB |
| `edit` | 2026-09-01 | `hook`, `thinking`, `read`, `edit`, `response` → `success.completed` | `!edit` | single turn | 80 KB |
| `fan-wide-cancel` | 2026-09-02 | `hook`, `thinking`, `bash`, `response` → `success.completed` | `!cancel-all` | single turn | 96 KB |
| `fast-mode` | 2026-09-01 | `hook`, `thinking`, `response` → `success.completed` | `!fast-on` | single turn | 40 KB |
| `glob` | 2026-09-01 | `hook`, `thinking`, `bash`, `response` → `success.completed` | `!glob` | single turn | 64 KB |
| `grep-content-files-count` | 2026-09-01 | `hook`, `thinking`, `bash`, `response` → `success.completed` | `!grep-content` + `!grep-files` + `!grep-count` | single turn | 80 KB |
| `held-turn-gate` | 2026-09-02 | `hook`, `thinking`, `bash`, `response` → `success.completed` | `!hold` | single turn | 76 KB |
| `hook-blocked` | 2026-09-01 | `hook`, `thinking`, `bash`, `response` → `success.completed` | `!hook-blocked` | single turn | 60 KB |
| `hook-cancelled` | 2026-09-01 | `hook`, `thinking`, `bash`, `response` → `success.completed` | `!hook-cancelled` | single turn | 56 KB |
| `hook-failed` | 2026-09-01 | `hook`, `thinking`, `bash`, `response` → `success.completed` | `!hook-failed` | single turn | 56 KB |
| `hook-succeeded` | 2026-09-01 | `hook`, `thinking`, `bash`, `response` → `success.completed` | `!hook-success` | single turn | 52 KB |
| `ide-diagnostics-after-edit` | 2026-09-01 | `hook`, `thinking`, `response`, `read`, `edit`, `bash` → `success.completed` | `!ide-diagnostics` | single turn | 156 KB |
| `identity-rotation-clear` | 2026-09-02 | `hook`, `thinking`, `response` → `success.completed` | `!rotate` | 3 turn terminals | 116 KB |
| `interrupt` | 2026-09-01 | `hook`, `thinking`, `response` → `success.interrupted` | `!interrupt` | single turn | 36 KB |
| `max-tokens` | 2026-09-01 | `hook`, `response` → `success.completed` | `!max-tokens` | single turn | 40 KB |
| `mcp-server-healths` | 2026-09-02 | `hook`, `thinking`, `bash`, `response` → `success.completed` | `!mcp-all` | single turn | 100 KB |
| `mcp-unmodeled-tool` | 2026-09-01 | `hook`, `thinking`, `response`, `unmodeled` → `success.completed` | `!unmodeled` | single turn | 88 KB |
| `model-changed` | 2026-09-02 | `hook`, `thinking`, `response` → `success.completed` | `!model-fallback` | single turn | 72 KB |
| `monitor-deadline` | 2026-09-01 | `hook`, `thinking`, `bash`, `monitor`, `response` → `success.completed` | `!monitor-deadline` | single turn | 92 KB |
| `monitor-persistent` | 2026-09-01 | `hook`, `thinking`, `monitor`, `response` → `success.completed` | `!monitor-persistent` | single turn | 84 KB |
| `permission-allow-once` | 2026-09-01 | `hook`, `thinking`, `bash`, `response` → `success.completed` | `!perm-allow-once` | single turn | 60 KB |
| `permission-allow-standing` | 2026-09-01 | `hook`, `thinking`, `response`, `bash` → `success.completed` | `!perm-allow-standing` | single turn | 60 KB |
| `permission-denied-by-policy` | 2026-09-01 | `hook`, `thinking`, `bash`, `response` → `success.completed` | `!perm-deny-policy` | single turn | 60 KB |
| `permission-denied-by-user` | 2026-09-02 | `hook`, `thinking`, `bash`, `response` → `success.completed` | `!perm-deny-user` | single turn | 52 KB |
| `permission-mode-changed` | 2026-09-02 | `hook`, `thinking`, `response` → `success.completed` | `!perm-allow-standing-mode` | single turn | 32 KB |
| `permission-undecidable-parked` | 2026-09-02 | `hook`, `thinking`, `bash` → `success.interrupted` | `!perm-hold` — NOT `!perm-undecidable` (see `!perm-undecidable`'s own entry below) | single turn | 32 KB |
| `plan-mode-enter-exit` | 2026-09-01 | `hook`, `thinking`, `planMode`, `subagent`, `response`, `bash` → `success.completed` | `!plan` | single turn | 116 KB |
| `prose-streamed` | 2026-09-01 | `hook`, `thinking`, `response` → `success.completed` | `` (the default `""` prose scenario) | single turn | 56 KB |
| `push-notification-not-sent` | 2026-09-01 | `hook`, `thinking`, `pushNotification`, `response` → `success.completed` | `!push-config-off` + `!push-user-present` + `!push-no-transport` | single turn | 92 KB |
| `push-notification-sent` | 2026-09-01 | `hook`, `thinking`, `pushNotification`, `response` → `success.completed` | `!push-sent` | single turn | 76 KB |
| `question-free-text` | 2026-09-01 | `hook`, `thinking`, `response` → `success.completed` | `!ask-free` | single turn | 64 KB |
| `question-multi-select` | 2026-09-01 | `hook`, `thinking`, `response` → `success.completed` | `!ask-multi` | single turn | 60 KB |
| `question-multiple-in-one-batch` | 2026-09-01 | `hook`, `thinking`, `response` → `success.completed` | `!ask-multi` (same scenario as `question-multi-select`, different assertion) | single turn | 68 KB |
| `question-single-select` | 2026-09-01 | `hook`, `thinking`, `response` → `success.completed` | `!ask-single` | single turn | 92 KB |
| `question-unanswered` | 2026-09-01 | `hook`, `thinking`, `response` → `success.completed` | `!ask-unanswered` | single turn | 60 KB |
| `read-whole-head-range` | 2026-09-01 | `hook`, `thinking`, `response`, `read` → `success.completed` | `!read` + `!read-head` + `!read-range` | single turn | 84 KB |
| `report-findings` | 2026-09-01 | `hook`, `thinking`, `read`, `reportFindings`, `response` → `success.completed` | `!findings` | single turn | 80 KB |
| `schedule-wakeup-schedule-and-stop` | 2026-09-01 | `hook`, `thinking`, `skillUse`, `bash`, `response` → `success.completed` | `!wakeup-schedule` + `!wakeup-stop` | single turn | 148 KB |
| `send-message-queued-and-resumed` | 2026-09-01 | `hook`, `thinking`, `response`, `subagent`, `sendMessage`, `bash` → `success.completed` | `!send-message` + `!send-message-resumed` | single turn | 140 KB |
| `skill-invocation` | 2026-09-01 | `hook`, `thinking`, `skillUse`, `read`, `response` → `success.completed` | `!skill` | single turn | 76 KB |
| `subagent-detached` | 2026-09-02 | `hook`, `thinking`, `subagent`, `bash`, `response` → `success.completed` | `!subagent-detached` | single turn | 92 KB |
| `subagent-sync-nested-activity` | 2026-09-01 | `hook`, `thinking`, `subagent`, `bash`, `read`, `response` → `success.completed` | `!subagent` | single turn | 104 KB |
| `task-acts-create-change-reject` | 2026-09-01 | `hook`, `thinking`, `taskAct`, `response` → `success.completed` | `!task-create` + `!task-change` + `!task-reject` | single turn | 144 KB |
| `turn-stop-error-during-execution` | 2026-09-01 | `hook`, `thinking`, `response` → `success.interrupted` | `!fail-execution` (DECLARED-ONLY) | single turn | 32 KB |
| `turn-stop-hook-stop` | 2026-09-01 | `hook`, `thinking`, `response` → `success.completed` | `!fail-stop-hook` (DECLARED-ONLY) | residue `vendor_specific/system/notification` | 188 KB |
| `turn-stop-max-budget-usd` | 2026-09-01 | `hook`, `thinking`, `response` → `failure.budgetExhausted` | `!fail-budget` | single turn | 60 KB |
| `turn-stop-max-structured-output-retries` | 2026-09-01 | `hook`, `thinking`, `response`, `unmodeled` → `success.completed` | `!fail-structured-output` (DECLARED-ONLY) | single turn | 128 KB |
| `turn-stop-max-turns` | 2026-09-01 | `hook`, `thinking`, `bash` → `failure.maxTurns` | `!fail-max-turns` | single turn | 88 KB |
| `vendor-answered-slash-commands` | 2026-09-01 | `hook`, `response` → `success.completed` | `!slash` | single turn | 44 KB |
| `web-fetch` | 2026-09-01 | `hook`, `thinking`, `webFetch`, `response` → `success.completed` | `!web-fetch` | single turn | 72 KB |
| `web-search` | 2026-09-01 | `hook`, `thinking`, `webSearch`, `response` → `success.completed` | `!web-search` | single turn | 76 KB |
| `worktree-enter-exit-kept-and-removed` | 2026-09-01 | `hook`, `thinking`, `response`, `skillUse`, `read`, `bash`, `write` → `success.completed` | `!worktree-keep` + `!worktree-remove` | residue `vendor_specific/system/vcs_state_changed` | 664 KB |
| `write-created-and-updated` | 2026-09-01 | `hook`, `thinking`, `read`, `write`, `response` → `success.completed` | `!write-create` + `!write-update` | single turn | 92 KB |

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
  pending a real grounding capture. STILL UNGROUNDED after two further
  Haiku attempts (2026-09-03): attempt 1 gave the model a real 40×200KB
  `bulk/` corpus (via a new `cwd_init`, replacing the old fully-manual note)
  but the model shortcut the "read every file" instruction with `Bash`+`md5`
  after one real `Read`, so it never occupied enough window to be warned.
  Attempt 2 forbade `Bash`/hashing and demanded literal reads, but each
  200KB file exceeds the `Read` tool's own 25000-token per-call cap, and the
  model declined the task outright rather than page through a file — so this
  scenario, too, produced no budget-warning attachment. Bailed per cost
  discipline after the second attempt. `prompts.json`'s `cwd_init` now
  generates smaller (80000-byte) files and the prompt tells the model to page
  a file with successive offset/limit `Read` calls rather than one call per
  file. STILL UNGROUNDED after that third lever was RUN (2026-09-04, Haiku,
  `--only context-budget-warning`): the run produced NO attachment of any
  kind — no `context_budget_warning`, no `context_tip`, no
  `total_tokens_reminder` — and instead ended in a hard API 400. What the
  vendor does as the window fills is now recorded evidence rather than
  conjecture: it emits NO warning beat at all, then answers with a SYNTHETIC
  assistant message (`model: "<synthetic>"`, `stop_reason: "stop_sequence"`,
  all usage counters zero) whose only content is the text `Prompt is too
  long`, carrying `error: "invalid_request"` and `is_api_error_message:
  true`, and the turn's `result` is `is_error: true` with
  `api_error_status: 400` and `terminal_reason: "prompt_too_long"`. That is a
  FAILED run, so by the harness's own rule it is not a golden and is not
  committed; the evidence sits at
  `~/.config/doom-overhaul/captures-0904/_failed/context-budget-warning/`.
  Two consequences for the taxonomy: `!context-budget-warning` remains
  UNGROUNDED and INVENTED after THREE attempts, and the previously
  DECLARED-ONLY `!context-window` / `!fail-prompt-too-long` arms now have
  real observed evidence (not a golden) of the shape the vendor actually
  produces. The scenario is not retried again without a NEW lever: three
  attempts have shown the vendor has no low-context warning on this path.

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

- **`!subagent-detached-utterance`** (`src/fake/scenarios/subagents.ts`).
  GROUNDED in shape: `subagent-detached` (this manifest, above) remains the
  golden for the detach/completion machinery; this scenario reuses the same
  launch shape but stops after ONE ordinary sidechain assistant text line and
  never completes the agent — no capture records a live subagent's mid-flight
  utterance in isolation (every capture with a detached subagent runs it to
  completion), so the utterance's OWN placement (post-turn, no terminal) is
  invented from the family's established pattern rather than a specific
  recording. Retires `subagentrouting_e2e_test.go`'s `sidechainResponseLine`
  fabrication.

- **`!usage-historical`** (`src/fake/scenarios/subagents.ts`). UNGROUNDED,
  INVENTED (per the inventory's own step 2b instruction): no capture in this
  manifest carries a FILE-plane-only historical usage record — one with no
  paired STREAM-plane `message_start` — attributed to a NESTED (spawnDepth 2)
  subagent id, carrying `cache_creation`'s ephemeral 5m/1h split,
  `server_tool_use` counts, `service_tier`, `speed` and `inference_geo`. The
  field VALUES mirror `sidecarAssistantUsageEvent`
  (`tokenutilization_e2e_test.go`), the daemon/e2e harness's own invented
  fixture, since no vendor recording exists to ground them from. Retires
  `tokenutilization_e2e_test.go`'s `sidecarAssistantUsageEvent` call sites.

- **`!monitor-deadline` / `!monitor-persistent`** (`src/fake/scenarios/automation.ts`).
  GROUNDED IN THE TOOL'S OWN DECLARED SCHEMA, `MonitorInput` in
  `@anthropic-ai/claude-agent-sdk/sdk-tools.d.ts`, which declares
  `description`, `timeout_ms` and `persistent` REQUIRED on the CALL and names
  `command` / `ws.url` as the only two sources. Both scenarios previously
  supplied the description only through `task_started` and spelled the deadline
  `timeoutMs` (the OUTPUT's spelling), and the persistent one named its source
  with `server`/`tool`, which appear on no declared input — so a shim reading
  the call, as the shim does, saw a monitor with no description, no lifetime
  and no source. The tool RESULT keeps `timeoutMs`, which is how `MonitorOutput`
  spells it.

- **task spools, created by `startTask`** (`src/fake/index.ts`). GROUNDED in the
  vendor's own behavior rather than in a capture: a background run's spool
  exists from the moment the run does, and `task_notification.output_file`
  (declared in `sdk.d.ts`) names a file the vendor has been writing all along.
  The fake used to create a spool only where a scenario appended to one, so a
  run that ended by TIMING OUT or by being CANCELLED had no file on disk for a
  tailer to open. `task_started` now names the path too, for the two kinds that
  OWN a spool (`local_bash`, whose spool is its output, and `local_agent`, whose
  spool is its own transcript) and for no other — a monitor has no spool, and an
  empty file under a `b*` name is bytes the vendor never writes. `sdk.d.ts` does
  not declare `output_file` on `task_started`, so the FIELD's presence there is
  the fake's own, and only the path it carries is grounded.

## Reconciliation: every registered scenario's grounding (2026-09-03)

The table above states, per golden, which registered `!<name>`(s) reproduce it
(the `Scenarios:` column). This section states the same fact in the OTHER
direction — for every name `scenarioNames()` returns, either the golden(s)
above it grounds, or an explicit UNGROUNDED reason — so neither list can
drift from `src/fake/registry.ts` without a human seeing it (guarded by
`test/fake/registry.test.ts`'s `MANIFEST.md agrees with the registry, in
both directions` case).

**Grounded, beyond the table above** (same arm, a second name or a residue
carrier, not a distinct golden of its own):

- `!usage-available` — the SAME `available` shape `account-usage` grounds
  through `!usage-full` (`session.ts`'s own comment: "two names for one
  arm").
- `!tokens-reminder` — GROUNDED (residue): the ONE token-budget carrier any
  real capture holds, observed in `artifact-publish-and-list`'s own run (see
  "Evidence gaps" above: "the nearest carrier observed is a
  `total_tokens_reminder` attachment").
- `!fail-marker` — GROUNDED IN PRODUCTION USAGE, not a capture: the daemon's
  own merge-pipeline acceptance gate (`mergeactions_e2e_test.go`) spells this
  exact marker; no capture backs it and none is needed (see registry.ts's own
  doc comment on `FAIL_TURN_MARKER`).

**Already reconciled above, in `e2ecleanup/fakesdk-ext`** (GROUNDED-IN-SHAPE
or UNGROUNDED/INVENTED, each with its own paragraph there — not repeated
here): `!slash-shape-a`, `!slash-shape-a-unnamed`, `!bash-detach-poll`,
`!subagent-detached-utterance`, `!usage-historical`.

**UNGROUNDED — declared sibling of a grounded family, no capture recorded
the narrower/alternate state**:

- `!usage-opus-absent`, `!usage-service-unavailable`,
  `!usage-window-unavailable`, `!usage-utilization-unavailable`,
  `!usage-sampling-failure` — declared `account-usage` outcomes; every
  capture that touches account usage recorded the `available` shape only.
- `!fast-off`, `!fast-cooldown` — declared fast-mode states; no capture ran
  with fast mode off or in cooldown.
- `!mcp-healthy` — a narrower MCP catalog than `!mcp-all` (which
  `mcp-server-healths` grounds); no capture recorded a single-healthy-server
  session.
- `!rate-limit`, `!rate-limit-five-hour`, `!rate-limit-seven-day` — declared
  `rate_limit_event` shapes; no capture recorded one.
- `!compact-failed` — a declared compaction variant; no run recorded a
  compaction FAILURE. Its shape is aligned to the two real compaction
  groundings (`src/fake/scenarios/session.ts`): the `status{compacting}` start
  beat is real, and only the `compact_result: "failed"` end beat is invented,
  since no capture carries one. (`!compact-auto` is NO LONGER in this list —
  see `auto-compaction`'s own row in the table above, captured 2026-09-04.)
- `!read-truncated`, `!read-image` — declared `Read` extents; no capture's
  model truncated a read by length or read an image back through `Read`
  itself (the one captured image round trip, `bash-image-output`, read it
  back through `Bash`).
- `!bash-hold`, `!bash-detach-fail`, `!bash-detach-live` — declared `Bash`
  states (parked, a failed detach, a still-live tail); no capture recorded
  any of the three.
- `!web-fetch-redirect` — declared `WebFetch` redirect; no capture recorded
  one.
- `!skill-fail` — declared failed `Skill` invocation; no capture recorded
  one.
- `!send-message-refused` — declared `SendMessage` refusal; no capture
  recorded one.
- `!subagent-detached-live`, `!subagent-detached-hold`, `!subagent-failed` —
  declared subagent states; no capture recorded any of the three.
- `!subagent-interleaved` — GROUNDED IN SHAPE by `subagent-detached` (the
  launch, the sidechain attribution and the completion notification), but the
  INTERLEAVING itself is ungrounded: no capture streams a subagent's response
  into an open main block. The order mirrors a live session's logs, where a
  background subagent's `message_start` arrived between two deltas of the main
  agent's open thinking and text blocks.
- `!perm-no-standing` — declared "ask offered no standing" arm; untested by
  any capture.
- `!perm-undecidable` — a KNOWN-OPEN arm by the scenario's own doc comment:
  `sdk.d.ts` declares no discriminator separating "nobody could decide" from
  an ordinary policy deny, so this is the closest producer, not a grounded
  one; `permission-undecidable-parked` is grounded through `!perm-hold`
  instead (see that row's note above).
- `!context-tip` — despite the scenario's own doc comment, the "Evidence
  gaps" section above is explicit: no capture carries a `context_tip`
  attachment of any kind (only `total_tokens_reminder` was observed).
  UNGROUNDED.
- `!away-summary`, `!residue` — declared vendor-residue / bookkeeping
  producers; no capture recorded either.
- `!cold-seed` — GROUNDED BY SHAPE (2026-09-05): the cold-context gate is the
  SHIM's own judgment, not a vendor response shape (`engine/cold.ts`'s
  `readTranscriptFacts`/`judgeCold` reads an ORDINARY transcript's last
  assistant line — `timestamp` and `message.usage` — and compares the age to
  the cache TTL; the vendor emits nothing distinct for "cold"). So `!cold-seed`
  needs no vendor capture of a `SessionCold` refusal; it needs its seeded
  assistant transcript line to carry the same field shape any ordinary
  assistant line does. Verified field-by-field against the last assistant
  line of three captures (`bash-foreground-completed`, `edit`,
  `context-usage`): `timestamp` (ISO-8601, millisecond precision, `Z`
  suffix) matches; `message.model` is a string field in both; every
  `message.usage` key the gate reads — `input_tokens`,
  `cache_creation_input_tokens`, `cache_read_input_tokens`,
  `cache_creation.ephemeral_1h_input_tokens`,
  `cache_creation.ephemeral_5m_input_tokens` — and the adjacent
  `output_tokens`/`service_tier` keys are all present with matching types in
  the scenario's `fakeUsage()` (`src/fake/index.ts`) and in all three
  captures. No divergence found; no fake code change was needed.
- `!md` — a webapp markdown-rendering demo, not a vendor shape at all; no
  capture could ground it.

**UNGROUNDED — declared taxonomy arm, unrecordable by a successful capture
(the capture harness's own rule: "a failed run is never a golden")**:

- `!fail-blocking-limit`, `!fail-rapid-refill`, `!fail-prompt-too-long`,
  `!fail-image`, `!fail-model`, `!fail-malformed-tool-use`,
  `!fail-tool-deferred`, `!fail-tool-deferred-unavailable`,
  `!fail-turn-setup`, `!fail-aborted-tools`, `!fail-hook-stopped`,
  `!fail-continuation-prevented` — declared turn-stop failure taxonomy
  (`sdk.d.ts`'s stop-reason arms), the same DECLARED-ONLY status as the three
  already named in "Evidence gaps" above (`!fail-execution`,
  `!fail-stop-hook`, `!fail-structured-output`) — no capture reaches any of
  these either.
- `!api-429`, `!api-529`, `!api-401`, `!api-403`, `!api-400`, `!api-413`,
  `!api-404`, `!api-500`, `!api-billing`, `!api-oauth-org`,
  `!api-max-output`, `!api-unmodeled` — declared API-error shapes; no capture
  recorded a real API error.
- `!refusal-fallback`, `!refusal-no-fallback` — declared model-REFUSAL
  shapes, distinct from the GROUNDED `!model-fallback` (an unsolicited swap,
  not a refusal); no capture recorded a refusal.
- `!context-window` — declared `context-window-exceeded` terminal; no
  capture reached it.
- `!fault-converter`, `!fault-recover` — shim-internal fault-injection
  scenarios, not vendor shapes at all; no capture could ground them.
- `!query-eof`, `!query-eof-mid-ask`, `!keepalive` — declared
  vendor-process-death and keepalive shapes; unrecordable by definition (a
  capture is, by the harness's own rule, a completed run).
- `!query-fail` — PARTIALLY GROUNDED, evidence only, not a golden: a Haiku
  run (2026-09-03, `--only prose-streamed` into a scratch `--out`, not this
  repo's `captures/`) had its spawned vendor child `kill -9`'d mid-stream
  (`pgrep -P <capture pid>` found the real
  `.../claude-agent-sdk-darwin-arm64/claude` child). The SDK's async
  iterator does NOT end silently and does NOT synthesize a result message:
  it THROWS synchronously — `Error: Claude Code process terminated by signal
  SIGKILL` (`sdk.mjs`'s `getProcessExitError`) — and the turn never reaches
  any `result`/terminal message at all. capture.mjs's own quarantine rule
  (a run that threw, or never reached a terminal, is never promoted to a
  golden) applies here exactly as documented, so this is NOT committed to
  `testdata/captures/` — the raw evidence (`stream.jsonl`, `meta.json`) is
  kept at `~/.config/doom-overhaul/captures/_failed/query-death/` for the
  project lead's own read, per this scenario's own manual note ("the golden
  is whatever the SDK's async iterator does at that moment"). Whichever
  shim arm answers `query_died`/`query_eof` must therefore treat vendor
  child death as a THROW to catch, never a result to interpret.
- `cold-resume` — CLOSED, no longer needed for grounding. `!cold-seed`
  grounds through FIELD SHAPE against ordinary captures instead (see the
  reconciliation note above): the cold-context gate is the shim's own
  judgment on an ordinary transcript, not a distinct vendor response shape,
  so no capture of an actual vendor `SessionCold` refusal is required. The
  earlier attempt to CAPTURE one by resuming a cache-lapsed session (two
  Haiku attempts, 2026-09-03; the harness fix and `resume_capture` lever
  built 2026-09-04) is therefore abandoned as unnecessary rather than merely
  blocked. `resume_capture` (`worlds.mjs`, plus `readCapturedSession` and
  `seedResumableSession` in `capture.mjs`, all unit tested) stays in the
  harness as tooling for a future capture that does need a cache-lapsed
  resume; the "needs an owner-supplied credential token" note that had
  parked `cold-resume` no longer gates anything and is not a pending item.

  SEPARATE HYGIENE BUG, found while working this and NOT fixed here: under
  `--config-root` the per-scenario reclaim and the sweep-end `lateReclaimAll`
  do not fully clean up. `~/.claude/projects/` held seven
  `*agent-repl-capture-*` directories — from the 2026-09-03 runs and from
  2026-09-04's — each with one file the vendor late-flushed after the reclaim
  pass had already run. They are unambiguously capture residue rather than
  the operator's own projects, but the harness claims to "LEAVE THE ACCOUNT
  AS FOUND" and does not.

This reconciliation is now COMPLETE and AUTHORITATIVE: every one of the 69
goldens above names its registered scenario(s), every registered scenario
either names its grounding golden (here or in the table above) or carries an
explicit UNGROUNDED reason, and `test/fake/registry.test.ts` walks this file
mechanically to keep both directions honest as scenarios are added or
renamed.
