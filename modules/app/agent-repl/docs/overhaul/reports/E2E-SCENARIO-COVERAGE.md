# E2E scenario coverage audit (modules/app/agent-repl/e2e)

Scope: does a test in the rebuilt `modules/app/agent-repl/e2e` suite drive
each of the 69 named golden-capture scenarios through a real shim + real
store + real sidecar against a hosted daemon, and assert on daemon-rendered
output? Re-derived from source (20 area files, `SPEC.md` sections C and D,
the golden captures themselves). See Methodology for exactly what counted.

**This audit is BY READING SOURCE ONLY. The suite has never been executed —
not the compile gate, not a single test.** "Covered" below means *a test
exists that drives the named scenario and asserts on daemon-rendered
output*. It does NOT mean the test passes, compiles, or is green. No
production code was read for defects and none was changed; this document
records what was written, not what runs.

## Headline finding

This supersedes the prior "0 of 69" baseline recorded when `daemon/e2e` had
been deleted and the named-scenario fake SDK did not yet exist on this
branch. That mechanism now exists (`agent-shim/claude/shim/src/fake/`,
registered `!name` scenarios) and a new suite — 20 area files, 98 `Test*`
functions — has been built against it in
`modules/app/agent-repl/e2e/*_e2e_test.go`.

- **69 of 69 goldens have at least one test written that names the scenario
  and asserts against daemon output** (compile-gate only; never executed).
- **67 of 69 goldens have a test that actually runs its assertions.** The
  remaining 2 (`ctrl-b-detach-of-foreground-work`,
  `ctrl-b-detach-of-foreground-subagent`) have ONLY a `t.Skip`, ruled out of
  scope — see "Skipped tests" below.
- Of the 67 that run, several assert something narrower than a
  scenario-specific discriminator: 3 are DECLARED-ONLY (the mock emits an
  arm no real capture ever reached), 1 is UNGROUNDED (a wire shape with no
  grounding capture at all), and 5 are reachability-only or weaker than the
  golden's own name implies (the frontend surface has no typed field to pin
  the distinguishing fact). See "Not covered / partially covered" for the
  full, honest breakdown.
- 0 goldens are fully uncovered.
- 0 goldens are undetermined.

## Per-scenario table

"Registered scenario" is the fake SDK's own `!name` prompt selector where it
differs from the golden's capture-directory name (see the preserved drift
section at the end of this document for the full list of known
name-mismatch cases; this column repeats the ones visible from the test
files read for this audit).

| Golden | Covered? | Test(s) | File:line |
|---|---|---|---|
| account-usage | yes | `TestAccountUsage` (drives `!usage-full`, not `!usage-subagent`) | `e2e/accounting_e2e_test.go:128` |
| artifact-publish-and-list | yes | `TestArtifactPublishAndList` (`!artifact-publish` + `!artifact-list`) | `e2e/remainder_e2e_test.go:190` |
| bash-detached | yes | `TestBashDetachedStartAndComplete` (`!bash-detach`) | `e2e/detachedbash_e2e_test.go:210` |
| bash-foreground-completed | yes | `TestBashForegroundCompleted` (`!bash`) | `e2e/detachedbash_e2e_test.go:409` |
| bash-image-output | yes | `TestBashImageOutput` (`!bash-image`) | `e2e/detachedbash_e2e_test.go:496` |
| bash-interrupted-by-timeout | yes | `TestBashInterruptedByTimeout` (registered `!bash-timeout`, not the golden's own name) | `e2e/interrupt_e2e_test.go:153` |
| bash-nonzero-exit | yes | `TestBashNonzeroExit` (`!bash-fail`) | `e2e/detachedbash_e2e_test.go:452` |
| bash-partial-output-with-spill | yes | `TestBashPartialOutputWithSpill` (`!bash-spill`) | `e2e/detachedbash_e2e_test.go:534` |
| compaction-directed | yes | `TestCompactionDirected` (`!compact`); `TestCompactionDirectedWithSummaryOverride` (`!compact [summary]`, landed addition) | `e2e/compaction_e2e_test.go:192`, `:238` |
| context-budget-warning | yes (UNGROUNDED — see below) | `TestContextBudgetWarning` (`!context-budget-warning`) | `e2e/compaction_e2e_test.go:392` |
| context-injected-memory | yes (reachability-only — see below) | `TestContextInjectedMemory` (registered `!memory`) | `e2e/remainder_e2e_test.go:239` |
| context-injected-skills | yes (reachability-only — see below) | `TestContextInjectedSkills` (registered `!skills-injected`) | `e2e/remainder_e2e_test.go:271` |
| context-usage | yes | `TestContextUsage` | `e2e/accounting_e2e_test.go:163` |
| cron-create-list-delete | yes | `TestCronCreateListDelete` (registered `!cron`) | `e2e/remainder_e2e_test.go:299` |
| ctrl-b-detach-of-foreground-subagent | **no — skipped** | `TestCtrlBDetachOfForegroundSubagent` | `e2e/detachedbash_e2e_test.go:374` |
| ctrl-b-detach-of-foreground-work | **no — skipped** | `TestCtrlBDetachOfForegroundWork` | `e2e/detachedbash_e2e_test.go:321` |
| diagnostics | yes (no registered scenario at all — assert-by-absence, per the drift section) | `TestDiagnostics` (plain prompt) | `e2e/remainder_e2e_test.go:337` |
| edit | yes | `TestEdit` | `e2e/filetools_e2e_test.go:92` |
| fan-wide-cancel | yes | `TestFanWideCancel` | `e2e/mergequeue_e2e_test.go:629` |
| fast-mode | yes | `TestFastMode` | `e2e/turnlifecycle_e2e_test.go:410` |
| glob | yes | `TestGlob` | `e2e/filetools_e2e_test.go:116` |
| grep-content-files-count | yes | `TestGrepContentFilesCount` | `e2e/filetools_e2e_test.go:143` |
| held-turn-gate | yes | `TestHeldTurnGate` (driven with `!hold`, the daemon queue mechanism, not a scenario of its own name) | `e2e/permission_e2e_test.go:413` |
| hook-blocked | yes | `TestHookBlocked` | `e2e/hooks_e2e_test.go:168` |
| hook-cancelled | yes | `TestHookCancelled` | `e2e/hooks_e2e_test.go:232` |
| hook-failed | yes | `TestHookFailed` | `e2e/hooks_e2e_test.go:198` |
| hook-succeeded | yes | `TestHookSucceeded` (registered `!hook-success`) | `e2e/hooks_e2e_test.go:123` |
| ide-diagnostics-after-edit | yes | `TestIdeDiagnosticsAfterEdit` | `e2e/filetools_e2e_test.go:301` |
| identity-rotation-clear | yes | `TestClearRotatesIdentity` (`!rotate`); `TestSecondRotateUnderRotatedIdentity` (a second `!rotate`) | `e2e/identityrotation_e2e_test.go:144`, `:185` |
| interrupt | yes (weaker than the golden's own description — see below) | `TestInterruptAfterTextDelta` (`!interrupt`) | `e2e/interrupt_e2e_test.go:81` |
| max-tokens | yes | `TestMaxTokens` | `e2e/turnlifecycle_e2e_test.go:446` |
| mcp-server-healths | yes | `TestMcpServerHealths` (registered `!mcp-all`) | `e2e/mcpmonitors_e2e_test.go:74` |
| mcp-unmodeled-tool | yes | `TestMcpUnmodeledTool` (registered `!unmodeled`) | `e2e/mcpmonitors_e2e_test.go:217` |
| model-changed | yes | `TestModelChanged` | `e2e/turnlifecycle_e2e_test.go:371` |
| monitor-deadline | yes | `TestMonitorDeadline` | `e2e/mcpmonitors_e2e_test.go:310` |
| monitor-persistent | yes | `TestMonitorPersistent` | `e2e/mcpmonitors_e2e_test.go:330` |
| permission-allow-once | yes | `TestPermissionAskAnsweredArms` (subtest, `!perm-allow-once`) | `e2e/permission_e2e_test.go:179` |
| permission-allow-standing | yes | `TestPermissionAskAnsweredArms` (subtest, `!perm-allow-standing`) | `e2e/permission_e2e_test.go:179` |
| permission-denied-by-policy | yes | `TestPermissionDeniedByPolicy` (`!perm-deny-policy`) | `e2e/permission_e2e_test.go:317` |
| permission-denied-by-user | yes | `TestPermissionAskAnsweredArms` (subtest, `!perm-deny-user`) | `e2e/permission_e2e_test.go:179` |
| permission-mode-changed | yes | `TestPermissionModeChangedMidSession` (registered `!perm-allow-standing-mode`) | `e2e/permission_e2e_test.go:485` |
| permission-undecidable-parked | yes | `TestPermissionUndecidableParked` (registered `!perm-hold`, NOT `!perm-undecidable`) | `e2e/permission_e2e_test.go:369` |
| plan-mode-enter-exit | yes | `TestPlanModeEnterExit` (registered `!plan`) | `e2e/remainder_e2e_test.go:366` |
| prose-streamed | yes | `TestTurnStartToCompletion`; `TestProseStreamedFourBlockShape` (the specific four-block shape) | `e2e/turnlifecycle_e2e_test.go:122`, `:489` |
| push-notification-not-sent | yes (reachability-only per arm — see below) | `TestPushNotificationNotSent` (3 sub-tests: `push-config-off`, `push-user-present`, `push-no-transport`) | `e2e/remainder_e2e_test.go:419` |
| push-notification-sent | yes | `TestPushNotificationSent` | `e2e/remainder_e2e_test.go:391` |
| question-free-text | yes | `TestQuestionFreeText` (registered `!ask-free`) | `e2e/questions_e2e_test.go:191` |
| question-multi-select | yes | `TestQuestionMultiSelect` (registered `!ask-multi`, shared with #question-multiple-in-one-batch) | `e2e/questions_e2e_test.go:325` |
| question-multiple-in-one-batch | yes | `TestQuestionMultipleInOneBatch` (registered `!ask-multi`, same prompt as above, different assertion) | `e2e/questions_e2e_test.go:363` |
| question-single-select | yes | `TestQuestionSingleSelect` (registered `!ask-single`) | `e2e/questions_e2e_test.go:237` |
| question-unanswered | yes | `TestQuestionUnanswered` (registered `!ask-unanswered`) | `e2e/questions_e2e_test.go:436` |
| read-whole-head-range | yes | `TestReadWholeHeadRange` | `e2e/filetools_e2e_test.go:211` |
| report-findings | yes | `TestReportFindings` (registered `!findings`) | `e2e/remainder_e2e_test.go:444` |
| schedule-wakeup-schedule-and-stop | yes | `TestScheduleWakeupScheduleAndStop` (`!wakeup-schedule` + `!wakeup-stop`) | `e2e/remainder_e2e_test.go:481` |
| send-message-queued-and-resumed | yes (weak discriminator — see below) | `TestSendMessageQueuedAndResumed` (`!send-message` + `!send-message-resumed`) | `e2e/remainder_e2e_test.go:509` |
| skill-invocation | yes | `TestSkillInvocation` | `e2e/skills_e2e_test.go:106` |
| subagent-detached | yes | `TestSubagentDetached` | `e2e/subagents_e2e_test.go:283` |
| subagent-sync-nested-activity | yes | `TestSubagentSyncNestedActivity` | `e2e/subagents_e2e_test.go:210` |
| task-acts-create-change-reject | yes | `TestTaskActsCreateChangeReject` (`!task-create` + `!task-change` + `!task-reject`) | `e2e/remainder_e2e_test.go:536` |
| turn-stop-error-during-execution | yes (DECLARED-ONLY — see below) | `TestTurnStopErrorDuringExecution` (`!fail-execution`) | `e2e/turnlifecycle_e2e_test.go:281` |
| turn-stop-hook-stop | yes (DECLARED-ONLY — see below) | `TestTurnStopHookStop` (`!fail-stop-hook`) | `e2e/turnlifecycle_e2e_test.go:321` |
| turn-stop-max-budget-usd | yes | `TestTurnStopMaxBudgetUsd` (`!fail-budget`) | `e2e/turnlifecycle_e2e_test.go:193` |
| turn-stop-max-structured-output-retries | yes (DECLARED-ONLY — see below) | `TestTurnStopMaxStructuredOutputRetries` (`!fail-structured-output`) | `e2e/turnlifecycle_e2e_test.go:240` |
| turn-stop-max-turns | yes | `TestTurnStopMaxTurns` (`!fail-max-turns`) | `e2e/turnlifecycle_e2e_test.go:164` |
| vendor-answered-slash-commands | yes | `TestVendorAnsweredSlashCommand` | `e2e/slashcommands_e2e_test.go:203` |
| web-fetch | yes | `TestWebFetch` | `e2e/remainder_e2e_test.go:561` |
| web-search | yes | `TestWebSearch` | `e2e/remainder_e2e_test.go:587` |
| worktree-enter-exit-kept-and-removed | yes | `TestWorktreeEnterExitKeptAndRemoved` (`!worktree-keep` + `!worktree-remove`) | `e2e/remainder_e2e_test.go:613` |
| write-created-and-updated | yes | `TestWriteCreatedAndUpdated` | `e2e/filetools_e2e_test.go:263` |

## Per-area summary

| Area file | Tests | Goldens covered |
|---|---|---|
| `accounting_e2e_test.go` | 2 | account-usage, context-usage |
| `adoption_e2e_test.go` | 4 | none of the 69 (restart/handover/replay mechanics; `TestColdBootReadsReplayFromStore`/`TestSessionStartedReAnnouncedOnEveryNewWatch` incidentally drive `!prose-streamed` as a precondition, not as this file's own coverage of that golden) |
| `compaction_e2e_test.go` | 5 | compaction-directed (2 tests), context-budget-warning (`CompactionAuto`/`CompactionFailed` are landed additions not tied to a named golden) |
| `degradedstate_e2e_test.go` | 2 | none of the 69 (a real store-outage mechanism, no golden capture) |
| `detachedbash_e2e_test.go` | 8 | bash-detached, bash-foreground-completed, bash-nonzero-exit, bash-image-output, bash-partial-output-with-spill, ctrl-b-detach-of-foreground-work (**skipped**), ctrl-b-detach-of-foreground-subagent (**skipped**) |
| `filetools_e2e_test.go` | 6 | edit, glob, grep-content-files-count, read-whole-head-range, write-created-and-updated, ide-diagnostics-after-edit |
| `hibernation_e2e_test.go` | 3 | none of the 69 (hibernation/keep-alive is not a golden-capture family) |
| `hooks_e2e_test.go` | 4 | hook-succeeded, hook-blocked, hook-failed, hook-cancelled |
| `identityrotation_e2e_test.go` | 2 | identity-rotation-clear |
| `interrupt_e2e_test.go` | 2 | interrupt, bash-interrupted-by-timeout |
| `mcpmonitors_e2e_test.go` | 4 | mcp-server-healths, mcp-unmodeled-tool, monitor-deadline, monitor-persistent |
| `mergequeue_e2e_test.go` | 5 | fan-wide-cancel (the other 4 tests pin daemon-synthesized merge/displaced-turn contract facts with no named golden) |
| `permission_e2e_test.go` | 5 | permission-allow-once, permission-allow-standing, permission-denied-by-user, permission-denied-by-policy, permission-undecidable-parked, held-turn-gate, permission-mode-changed |
| `questions_e2e_test.go` | 5 | question-free-text, question-single-select, question-multi-select, question-multiple-in-one-batch, question-unanswered |
| `refusals_e2e_test.go` | 6 (1 **skipped**) | none of the 69 (refusal-arm contract facts — `bubble_refused`, `unknown_agent`, `vendor_start_failed`, `not_the_open_turn` — are not named golden captures) |
| `remainder_e2e_test.go` | 15 | artifact-publish-and-list, context-injected-memory, context-injected-skills, cron-create-list-delete, diagnostics, plan-mode-enter-exit, push-notification-sent, push-notification-not-sent, report-findings, schedule-wakeup-schedule-and-stop, send-message-queued-and-resumed, task-acts-create-change-reject, web-fetch, web-search, worktree-enter-exit-kept-and-removed |
| `skills_e2e_test.go` | 2 | skill-invocation (`SkillNamedAndArgsParameterized` extends the same golden's shape, not a separate one) |
| `slashcommands_e2e_test.go` | 4 | vendor-answered-slash-commands (`SlashShapeANamed`/`SlashShapeAUnnamed`/`SlashShapeBViaSlash` are landed additions/existing-scenario reuse, not separate goldens) |
| `subagents_e2e_test.go` | 4 | subagent-sync-nested-activity, subagent-detached |
| `turnlifecycle_e2e_test.go` | 10 | prose-streamed (2 tests), turn-stop-max-turns, turn-stop-max-budget-usd, turn-stop-max-structured-output-retries, turn-stop-error-during-execution, turn-stop-hook-stop, model-changed, fast-mode, max-tokens |

98 tests total across 20 files. Every one of the 69 goldens is owned by
exactly one area file (no golden is claimed by two files); several area
files (`adoption`, `degradedstate`, `hibernation`, `mergequeue` partially,
`refusals`) exist to pin contract facts that are not named golden captures
at all, per `SPEC.md` section C.

## Not covered / partially covered

No golden has zero tests. The gaps are all in *what a written test actually
proves*, honestly reported:

### Skipped tests (assert nothing at runtime)

- **`TestCtrlBDetachOfForegroundWork`** (`e2e/detachedbash_e2e_test.go:321`,
  skip at line 353) and **`TestCtrlBDetachOfForegroundSubagent`**
  (`:374`, skip at line 397) — golden `ctrl-b-detach-of-foreground-work` and
  `ctrl-b-detach-of-foreground-subagent`. Skipped per the file's own header
  citing `docs/overhaul/PROTO-CHANGES.md`, Landing 8, RULED-no-proto:
  "Ctrl-b detach of foreground work has no daemon verb; out of scope for the
  overhaul, recorded as a follow-up; the two e2e tests stay skipped pointing
  here." The subagent variant adds a second reason: no fake-SDK scenario
  backgrounds a subagent the way `shell.ts`'s CTRL_B backgrounds a Bash call.
  These are written up to the point the real wire can reach, then skip —
  they are not deleted, so they remain visible in `go test -list`.
- **`TestKillTurnNotTheOpenTurn`** (`e2e/refusals_e2e_test.go:479`, not a
  named golden) — skipped because `KillTurnFailure.not_the_open_turn` is
  produced only by an internal daemon/shim race with no client-observable
  trigger, no documented scenario or env lever, and no harness
  synchronization primitive to force the interleaving deterministically.
  The file's header cites `SPEC.md` section F's "flag rather than fabricate"
  precedent; left open for the project lead rather than raced or guessed.

### Reachability-only assertions (the field the golden names is absent from the surface)

- **`push-notification-not-sent`** (`TestPushNotificationNotSent`,
  `e2e/remainder_e2e_test.go:419`) — three registered arms exist
  (`push-config-off`, `push-user-present`, `push-no-transport`), each with
  its own disabled-reason. The generic `FeedSimpleToolCall` shell this tool
  renders through carries no typed disabled-reason field, so each sub-test
  asserts only that its own named arm reaches the daemon and settles
  successfully — it does not, and per the header cannot, distinguish *which*
  reason was recorded.
- **`send-message-queued-and-resumed`** (`TestSendMessageQueuedAndResumed`,
  `e2e/remainder_e2e_test.go:509`) — the corpus discriminator between queued
  and resumed deliveries is a structured `resumedAgentId` field that does
  not ride the generic tool-call shell. The test substitutes a substring
  check ("resumed" appears in the tool's composed text output) rather than
  asserting the real field, per its own header.
- **`context-injected-memory`** (`TestContextInjectedMemory`,
  `e2e/remainder_e2e_test.go:239`) and **`context-injected-skills`**
  (`TestContextInjectedSkills`, `:271`) — `AgentContextInjected` is a
  file-plane-only fact absent from the shim's live `WatchSession` stream per
  `daemon.md`; no frontend `Watch*Agent*` rpc exists for it. RULED and
  RESOLVED (project lead, 2026-09-02): the footer's momentary
  `FooterStatusLoading` push is NOT the surface — nothing in the contract
  ties it to this fact. Both tests now assert through the READ-ONLY store
  verb `OpenAgentSession`, checking the injected attachment landed as an
  `AgentActivity.context_injected` page line in the main agent's book. No
  frontend assertion. (This entry previously described the withdrawn footer
  approach; the audit was taken before that update merged.)"
- **`interrupt`** (`TestInterruptAfterTextDelta`,
  `e2e/interrupt_e2e_test.go:81`) — `SPEC.md`'s own entry describes this
  golden as "prose truncated exactly after the observed `text_delta`," but
  the grounded fake-SDK scenario (`lifecycle.ts` INTERRUPT_MID_TOOL) has a
  withheld thinking block and never streams a `text_delta` at all — there is
  structurally nothing to interrupt "after." The test's header documents
  this as a genuine contract-vs-grounded-scenario disagreement (not papered
  over) and instead asserts the weaker, grounded fact: the Bash tool call
  reaches a settled/succeeded state at the same moment the turn's terminal
  row lands `FeedTurnEndedInterrupted`.

### DECLARED-ONLY (the mock emits an arm no real capture ever reached)

- **`turn-stop-max-structured-output-retries`**
  (`TestTurnStopMaxStructuredOutputRetries`,
  `e2e/turnlifecycle_e2e_test.go:240`) — the golden capture itself ended
  `success.completed`; the mock still deliberately emits the declared
  `structured_output_retry_exhausted` stop-reason. The test pins the shape
  the mock declares, not a golden-verified fact.
- **`turn-stop-error-during-execution`** (`TestTurnStopErrorDuringExecution`,
  `:281`) — same pattern; the capture ended `success.interrupted`, the mock
  still emits the declared `execution_error` arm with no terminal reason.
- **`turn-stop-hook-stop`** (`TestTurnStopHookStop`, `:321`) — same pattern;
  the capture ended `success.completed`. This test additionally cannot
  assert the vendor's own `system:stop_hook_summary` residue record at all —
  the harness exposes no residue-read verb, and the header explicitly
  declines to invent one.

### UNGROUNDED (a wire shape with no grounding capture)

- **`context-budget-warning`** (`TestContextBudgetWarning`,
  `e2e/compaction_e2e_test.go:392`) — `AgentUpdate.context_budget_warning`
  is marked UNGROUNDED/INVENTED in the shim's own manifest per `PROTO-
  CHANGES.md` Landing 4, "provisionally settled... pending a grounding
  capture." The test asserts the wire shape lands unmodified, not that it
  matches a real vendor recording, and says so in its own header.

### Goldens with no test at all

None. All 69 have at least one `Test*` function that names the scenario.

### Undetermined

None. Every golden's coverage state resolved from a test's own header
comment plus the corresponding capture/manifest citation it names.

## Methodology

**Counted as coverage**: a `modules/app/agent-repl/e2e` test whose header
comment and prompt selector name one of the 69 golden-capture scenarios (by
its literal capture name or its documented registered `!name`, where the two
differ — see the drift section below), driven through the real shim's
`--fake` engine, into a real store via the real sidecar, hosted by a real
daemon, with assertions on frames the daemon renders over its Connect API
(`OpenFeed`/`WatchFeed`/`WatchTopbar`/`WatchDaemonHolds`/footer streams).

**Evidence used**: each area file's own header comment (which names the
scenario(s) driven, the contract point pinned, and any grounding caveat —
DECLARED-ONLY, UNGROUNDED/INVENTED, reachability-only, or open question),
cross-checked against `SPEC.md` sections C (test list by area) and D (the
69-golden mapping table), and against the golden capture directory names at
`/Users/dodgecoates/.config/doom-overhaul/captures` (71 entries: 69 named
goldens plus `_inflight` and `SKIPPED.json`, neither a golden). `git grep`
for `^func Test` supplied every test's file:line.

**Not counted as coverage, and not present in this suite by design**: tests
that call `sidecar*Event(...)`/`ingestTranscriptAsSidecar(...)` or otherwise
write store facts by hand. `main_test.go`'s own grep gate is understood (from
file headers, not independently re-verified by running it) to refuse that
shape in this package; no area file's header claims to use it.

**Not run**: no test suite was executed — not `go build`, not `go vet`, not
`go test -run xxx`, not the compile gate, not any daemon or cross-system
integration run. Every finding above comes from reading the 20 `*_e2e_test.go`
files' Go source (headers, prompts, and assertions), `SPEC.md`, the capture
directory listing, and the existing report's own drift section. Whether
this suite currently compiles is unknown and out of scope for this document;
the project lead runs it and dispatches remediation.

**Open questions surfaced by area writers, not resolved here** (repeated
from the cited headers so a reader does not have to reopen every file):
whether the `interrupt`
golden's own description ("prose truncated after a `text_delta`") and its
grounded fake-SDK scenario (which never streams one) are reconcilable, or the
contract text needs revisiting; whether `HibernateError.kind.
compaction_failed{error}` and `.no_session` are reachable by any documented,
already-tested lever (hibernation area, not part of the 69 goldens but
flagged for completeness); whether the `fan-wide-cancel` golden's
agent-spool `EXIT=` terminator defect flagged in `E2E-SIDECAR-PLAN.md` is
fixed on this branch (the test's header instructs writing it to compile
either way rather than depending on the fix).

---

## Manifest/registry naming drift (recorded 2026-09-02; NOT a blocker)

While writing the rebuilt `modules/app/agent-repl/e2e` suite, area writers
found that a GOLDEN's name and the fake SDK's REGISTERED scenario prompt
often differ. `src/fake/registry.ts` matches on the scenario's own registered
name, so driving a golden by its capture name silently reaches nothing. Every
writer resolved this by reading the scenario source and documenting the
mapping in its test header; no test guessed.

The project lead has ruled this a SHIM-SIDE CLEANUP for later. It blocks
nothing: the e2e tests drive the registered names and are correct as written.

### One golden, one differently-named scenario

| Golden (capture / manifest) | Registered prompt |
|---|---|
| `hook-succeeded` | `!hook-success` |
| `mcp-server-healths` | `!mcp-all` |
| `mcp-unmodeled-tool` | `!unmodeled` |
| `context-injected-memory` | `!memory` |
| `context-injected-skills` | `!skills-injected` |

### One golden, SEVERAL scenarios driven in combination

| Golden (capture / manifest) | Registered prompts |
|---|---|
| `artifact-publish-and-list` | `!artifact-publish` + `!artifact-list` |
| `schedule-wakeup-schedule-and-stop` | `!wakeup-schedule` + `!wakeup-stop` |
| `send-message-queued-and-resumed` | `!send-message` + `!send-message-resumed` |
| `task-acts-create-change-reject` | `!task-create` + `!task-change` + `!task-reject` |
| `worktree-enter-exit-kept-and-removed` | `!worktree-keep` + `!worktree-remove` |

### A golden with NO registered scenario at all

- `diagnostics` — no `!`-prefixed scenario exists. RULED: a healthy shim's
  diagnostics push produces no topbar warning, so the e2e test asserts
  exactly that, plus that the topbar stream delivered at least one view (so
  "no warning" can never pass as "no stream").

This list comes from the area writers' own reports and is not claimed to be
exhaustive; a shim-side pass should reconcile the manifest against
`registry.ts` in full.
