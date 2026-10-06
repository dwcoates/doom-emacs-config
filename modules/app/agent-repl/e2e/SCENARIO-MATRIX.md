# Fake-SDK scenario coverage matrix — agent-repl

SELF-VERIFYING. Every mechanical fact below is DERIVED and asserted by
`e2e/scenariomatrix_test.go` (`TestScenarioMatrixMatchesReality`), which runs
with the e2e suite, spawns nothing, and drives no scenario. This document was
hand-maintained until it was caught lying in both directions on the same day
— twelve `!api-*` rows reading `uncovered` while one file drove every one of
them, twenty session and tool rows reading `uncovered` with strong tests
already standing behind nineteen, and summary counts that disagreed with the
table they summarized. Two agents nearly wrote duplicate tests off it. So it
is no longer maintained by hand; it is regenerated:

```bash
AGENT_REPL_MATRIX_WRITE=1 go test ./e2e -run TestScenarioMatrixMatchesReality
```

## What is derived, and from where

- **The canonical scenario list** comes from the mocked vendor's own source,
  `agent-shim/claude/shim/src/fake/scenarios/*.ts`, cross-checked against the
  registry-GENERATED prompt table in `agent-shim/claude/shim/AGENTS.md` (that
  table is rendered by `scripts/scenario-table.ts` from `SCENARIOS`, and
  `test/fake/registry.test.ts` asserts the committed copy matches the
  renderer in both directions). Two independent readings of one registry: a
  scenario added, renamed or deleted there fails the check until this
  document follows.
- **Which test file drives which scenario** comes from the TESTS, never from
  this document's prose. The check applies the vendor's own selection rule
  (`fake/registry.ts`'s `selectScenario`, golden-name `ALIASES` included) to
  the string literals the tests actually submit, and resolves the bare-name
  arguments of the drive helpers that prepend the `!` themselves
  (`driveScenarioToCompletion` in Go; `driveTurn`/`driveScenario`/
  `driveScenarioRow`, and one level of local wrapper, in the webapp layer).
  An argument shape the check cannot read is a FAILURE, not a silent zero.
- **Sections (b) and (c)** — the counts and the uncovered/weak lists — are
  computed from table (a)'s own columns, so the document cannot contradict
  itself.

## What this check cannot decide

Whether an assertion is STRONG or WEAK is a reading of the test body, and no
script can make that judgment. So three things stay HAND-WRITTEN and are not
checked: the `Grounded?` column, the `Strongest assertion` column, and the
choice between `covered` and `weak`. What the check does enforce is the
mechanical half of the verdict: a row reading `uncovered` must have no test
driving it, and a row reading `covered` or `weak` must have one. It never
promotes `weak` to `covered`, and a regenerated row for a newly-driven
scenario defaults to `weak` for a human to read and raise.

Two further limits, stated rather than hidden:

- **The default prose scenario has no row.** It is selected by every prompt
  that names no scenario, so "which tests drive it" would be nearly every
  file in the suite and would say nothing. `!prose-streamed`, its registered
  alias, is treated as selecting it and therefore contributes no row either.
- **Counted layers are Go (`e2e/*_e2e_test.go`, excluding `emacs_*`), webapp
  (`webapp/test/webapp-layer/*.layer.test.ts`) and Emacs (`e2e/emacs_*`).**
  Per the owner's ruling, daemon/integration, shim test/integration and
  sidecar integration tests do NOT count as e2e coverage even where they
  exercise the same scenario. Section (d) below and the reconciliation
  sections at the end are hand-written context and are not derived; section
  (e), dead triggers, IS enforced.

## (a) Full matrix

| Scenario | Grounded? | Go e2e | Webapp layer | Emacs e2e | Strongest assertion | Verdict |
|---|---|---|---|---|---|---|
| `!api-400` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestApiRequestFailedArms/InvalidRequest asserts the named arm `FeedTurnEndedErrored.invalid_request` (feed.proto:887-888, 400), the EXACT per-arm headline “the vendor refused the request as malformed” (equality is also a specific negative — no mid-turn evidence clause), and the exact vendor message “The request was invalid.”. | covered |
| `!api-401` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestApiRequestFailedArms/AuthenticationFailed asserts the named arm `FeedTurnEndedErrored.authentication_failed` (feed.proto:883-884, 401), the EXACT per-arm headline “the credential was rejected — sign in again” (equality is also a specific negative — no mid-turn evidence clause), and the exact vendor message “Authentication failed.”. | covered |
| `!api-403` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestApiRequestFailedArms/PermissionDenied asserts the named arm `FeedTurnEndedErrored.permission_denied` (feed.proto:885-886, 403), the EXACT per-arm headline “the credential lacks permission for this request” (equality is also a specific negative — no mid-turn evidence clause), and the exact vendor message “Permission denied for this request.”. | covered |
| `!api-404` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestApiRequestFailedArms/NotFound asserts the named arm `FeedTurnEndedErrored.not_found` (feed.proto:891-892, 404), the EXACT per-arm headline “the model or resource does not exist” (equality is also a specific negative — no mid-turn evidence clause), and the exact vendor message “The requested model was not found.”. | covered |
| `!api-413` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestApiRequestFailedArms/RequestTooLarge asserts the named arm `FeedTurnEndedErrored.request_too_large` (feed.proto:889-890, 413), the EXACT per-arm headline “the request exceeded the vendor's size limit” (equality is also a specific negative — no mid-turn evidence clause), and the exact vendor message “The request was too large.”. | covered |
| `!api-429` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestApiRequestFailedArms/RateLimited asserts the named arm `FeedTurnEndedErrored.rate_limited` (feed.proto:879-880, 429), the EXACT per-arm headline “rate limited by the vendor” (equality is also a specific negative — no mid-turn evidence clause), and the exact vendor message “Rate limited; retry after 30 seconds.”. TestApiRateLimitedCarriesTheVendorWait additionally pins `FeedTurnErrorRateLimited.retry_after_ms == 549`, the exact wait the vendor stated. | covered |
| `!api-500` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestApiRequestFailedArms/Internal asserts the named arm `FeedTurnEndedErrored.internal` (feed.proto:893-894, 500), the EXACT per-arm headline “the vendor API hit its own internal error” (equality is also a specific negative — no mid-turn evidence clause), and the exact vendor message “The service raised.”. | covered |
| `!api-529` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestApiRequestFailedArms/Overloaded asserts the named arm `FeedTurnEndedErrored.overloaded` (feed.proto:881-882, 529), the EXACT per-arm headline “the vendor API is overloaded” (equality is also a specific negative — no mid-turn evidence clause), and the exact vendor message “The API is overloaded.”. | covered |
| `!api-billing` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestApiRequestFailedArms/BillingError asserts the named arm `FeedTurnEndedErrored.billing_error` (feed.proto:904-905, 402 + the vendor class), the EXACT per-arm headline “the account could not be charged — check your billing” (equality is also a specific negative — no mid-turn evidence clause), and the exact vendor message “The account has a billing problem.”. | covered |
| `!api-max-output` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestApiRequestFailedArms/MaxOutputTokens asserts the named arm `FeedTurnEndedErrored.max_output_tokens` (feed.proto:911-913, no status — the vendor class alone), the EXACT per-arm headline “the request asked for more output than the model will produce” (equality is also a specific negative — no mid-turn evidence clause), and the exact vendor message “The response hit the max output tokens.”. | covered |
| `!api-oauth-org` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestApiRequestFailedArms/OauthOrgNotAllowed asserts the named arm `FeedTurnEndedErrored.oauth_org_not_allowed` (feed.proto:909-910, 403 + the vendor class), the EXACT per-arm headline “your organization does not allow this OAuth access” (equality is also a specific negative — no mid-turn evidence clause), and the exact vendor message “This organization is not permitted to use OAuth here.”. | covered |
| `!api-unmodeled` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestApiRequestFailedArms/VendorUnmodeled asserts the named arm `FeedTurnEndedErrored.vendor_unmodeled` (feed.proto:895-896), the EXACT per-arm headline “the vendor reported an error class we do not model yet” (equality is also a specific negative — no mid-turn evidence clause), and the exact vendor message “An error class this build does not model.”. TestApiUnmodeledKeepsTheVendorClassByName additionally pins `FeedTurnErrorVendorUnmodeled.type == "unknown"` — the class kept BY NAME, not collapsed. | covered |
| `!artifact-list` | declared-only | remainder_e2e_test.go | feed-families.layer.test.ts | — | Go: TestArtifactPublishAndList asserts only listTurn.GetValue()!= (turn minted); contract says list produces no row (weak). Web: feed-families.layer asserts a specific negative — artifact row count unchanged (strong). | covered |
| `!artifact-publish` | declared-only | remainder_e2e_test.go | feed-families.layer.test.ts | — | Go: TestArtifactPublishAndList asserts FeedArtifact.Published non-nil, non-empty Heading.Text/Url.Url. Web: feed-families.layer asserts .artifact-url element. | covered |
| `!ask-free` | grounded | questions_e2e_test.go | cards.layer.test.ts | — | Go: TestQuestionFreeText asserts exact free-text echo in OtherText, single_select arm. Web: cards.layer answers via [data-question-other], asserts submit control removed. | covered |
| `!ask-multi` | grounded | questions_e2e_test.go | cards.layer.test.ts | — | Go: TestQuestionMultiSelect/TestQuestionMultipleInOneBatch assert multi_select arm, exact 2-item Chosen slice, header/answer keyed by text. Web: cards.layer asserts [data-question-mode]. | covered |
| `!ask-single` | grounded | questions_e2e_test.go | cards.layer.test.ts, perf.layer.test.ts | — | Go: TestQuestionSingleSelect asserts exact chosen-label round-trip, OtherText unset. Web: cards.layer asserts [data-question-option]/[data-question-submit] and answered [data-state]. | covered |
| `!ask-unanswered` | grounded | questions_e2e_test.go | cards.layer.test.ts | — | Go: TestQuestionUnanswered asserts settled state is specifically FeedQuestionExpired (not Answered). Web: cards.layer asserts [data-state] present. | covered |
| `!away-summary` | ungrounded | producerfaults_e2e_test.go | — | — | Go: TestAwaySummaryDrawsNoRow asserts a plain Concluded terminal and, through pfAssertOnlyProseRows, the SPECIFIC NEGATIVE that the turn drew prompt/response/terminal rows and not one row more — the whole contract for a `system:away_summary` record no conversation.v1 arm models. | covered |
| `!bash` | grounded | detachedbash_e2e_test.go | feed-families.layer.test.ts | — | Go: TestBashForegroundCompleted asserts Succeeded verdict, exact stdout text, and absence of a detached_shell row. Web: feed-families.layer asserts [data-input-form]/[data-output-body]. | covered |
| `!bash-detach` | grounded | detachedbash_e2e_test.go | feed-families.layer.test.ts | — | Go: TestBashDetachedStartAndComplete asserts Exit.Code==0, live growth, spool text. Web: feed-families.layer asserts dataset.rowKind==detachedShell. | covered |
| `!bash-detach-fail` | ungrounded | detachedbash_e2e_test.go | — | — | Go: TestBashDetachedNonzeroExit asserts the two-sided rule feed.proto states — the outcome arm is `completed` (never cancelled, never lost: "a non-zero exit still COMPLETED") and the badness rides entirely on FeedShellExit.code == 3 — plus the spool's own appended line. | covered |
| `!bash-detach-live` | ungrounded | detachedbash_e2e_test.go, detachedlost_e2e_test.go | — | — | Go: TestBashDetachedLiveNeverSettles asserts the live, never-settling shape. detachedlost_e2e_test.go then drives all three `DetachedLost` arms off this same scenario — TestDetachedLostWentSilent, TestDetachedLostFileVanished, TestDetachedLostSweptUp — each asserting `FeedShellSettled.outcome.lost` plus the sidecar's own `reason` key naming the arm. | covered |
| `!bash-detach-poll` | ungrounded | detachedbash_e2e_test.go | — | — | Go: TestBashDetachExplicitPoll asserts settled exit 0, spool text, and a negative check that no TopbarWarning.UnmodeledTool names TaskOutput. | covered |
| `!bash-fail` | grounded | detachedbash_e2e_test.go | — | — | Go: TestBashNonzeroExit asserts Succeeded verdict despite nonzero exit, exact stdout text, and FeedToolCallReturned.exit.code == 3 — the command's own verdict on itself, drawn on the same FeedShellExit chip the detached shell wears. | covered |
| `!bash-hold` | ungrounded | deploy_e2e_test.go, detachedbash_e2e_test.go | — | — | Go: TestBashHoldStaysForegroundUntilInterrupted asserts the live FOREGROUND Bash unit, the negative that no detached_shell row is ever drawn for it, and that a real daemon Interrupt is the only terminal it reaches (turn_ended.interrupted). GAP recorded at the test: DetachForeground's `unsupported` refusal is a shim.v1 verb with no caller-facing rpc, so that arm stays out of reach from this layer. | covered |
| `!bash-image` | grounded | detachedbash_e2e_test.go | — | — | Go: TestBashImageOutput asserts Succeeded verdict and specifically the FeedToolCallReturned.image arm — a FeedImageBlock whose src is the data url the daemon composed, captioned with the command line. | covered |
| `!bash-spill` | grounded | detachedbash_e2e_test.go | — | — | Go: TestBashPartialOutputWithSpill asserts Succeeded verdict, exact truncation-phrase text. | covered |
| `!bash-timeout` | grounded | interrupt_e2e_test.go | — | — | Go: TestBashInterruptedByTimeout asserts terminal is Concluded (not Interrupted), detached shell stays Live, non-empty spool text. | covered |
| `!cancel-all` | grounded | mergequeue_e2e_test.go | — | — | Go: TestFanWideCancel asserts InterruptedDetached.Count==3 and a second call returns NothingRunning. | covered |
| `!cold-seed` | ungrounded | coldgate_e2e_test.go | — | — | Go: TestColdGate subtests assert exact ContextTokens value, non-empty model name/compact menu, and resolved arm matches the chosen button. | covered |
| `!compact` | grounded | compaction_e2e_test.go | feed-families.layer.test.ts | — | Go: TestCompactionDirected(+summary override) asserts exact FeedContextCutCompacted.Summary text, non-nil separation tokens. Web: feed-families.layer asserts .sep-compacted. | covered |
| `!compact-auto` | ungrounded | compaction_e2e_test.go | — | — | Go: TestCompactionAuto asserts exact summary string + non-nil tokens. | covered |
| `!compact-failed` | ungrounded | compaction_e2e_test.go | — | — | Go: TestCompactionFailed asserts exact FeedContextCutCompactionFailed.Error, no Compacted row drawn. | covered |
| `!context-budget-warning` | ungrounded | compaction_e2e_test.go | — | — | Go: TestContextBudgetWarning asserts FooterStatusActivityContextBudget.text equals `The conversation is approaching its context window budget.` VERBATIM — the scenario's own attachment content, copied unchanged by the converter and stored uncomposed by the resolver, so one exact string pins the whole path. | covered |
| `!context-tip` | ungrounded | producerfaults_e2e_test.go | — | — | Go: TestContextTipDrawsNoRow asserts the same only-prose-rows negative AND a second, named one — `FooterStatusBlockedActivity.context_budget` is nil, so a GENERIC CLI tip never draws as “your context is filling”. | covered |
| `!context-usage-drift` | grounded | accounting_e2e_test.go | — | — | Go: TestContextUsage asserts ContextPanelView.Header non-empty and differs across two reads, Categories non-empty. | covered |
| `!context-window` | ungrounded | producerfaults_e2e_test.go | — | — | Go: TestContextWindowExceededIsDrawnAsTurnFailed asserts the named arm `FeedTurnEndedErrored.turn_failed`, the exact `stop_reason` `prompt_too_long`, a non-empty composed headline, and the specific negative that `request_too_large` (feed.proto's 413-only arm) was NOT drawn. | covered |
| `!cron` | grounded | remainder_e2e_test.go | — | — | Go: TestCronCreateListDelete asserts turn Concluded and footer LiveWork.Crons chip Count positive. | covered |
| `!edit` | grounded | filetools_e2e_test.go | feed-families.layer.test.ts | — | Go: TestEdit asserts a diff output form. Web: feed-families.layer asserts [data-diff-line] count>0. | covered |
| `!fail-aborted-tools` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestTurnAbortedWhileToolsRunning asserts the terminal is specifically FeedTurnEndedInterrupted and specifically NOT Errored, plus the fate of the in-flight work — exactly one settled response row carrying the partial answer. | covered |
| `!fail-blocking-limit` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestTerminalReasonTurnFailedArms/BlockingLimit asserts TurnFailed.StopReason==blocking_limit, the exact headline "an account-level block stopped the run", the exact vendor message, and the surviving partial answer. | covered |
| `!fail-budget` | grounded | turnlifecycle_e2e_test.go | — | — | Go: TestTurnStopMaxBudgetUsd asserts errored.GetMaxBudget()!=nil plus headline. | covered |
| `!fail-continuation-prevented` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestTurnStopContinuationPrevented asserts the StopHookPrevented arm, the exact headline "a Stop hook ended the run", the exact vendor message, and the specific negative that NO response row is fabricated (this fake emits no assistant content). | covered |
| `!fail-execution` | declared-only | turnlifecycle_e2e_test.go | refusals.layer.test.ts | — | Go: TestTurnStopErrorDuringExecution asserts errored.GetExecutionError()!=nil. Web: refusals.layer asserts zero refusal rows and zero failureArms (specific negative). | covered |
| `!fail-hook-stopped` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestTerminalReasonTurnFailedArms/HookStopped asserts TurnFailed.StopReason==hook_stopped, the exact headline "a hook ended the run" (distinct from the Stop hook's own sentence), the exact vendor message, and the surviving partial answer. | covered |
| `!fail-image` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestTerminalReasonTurnFailedArms/ImageError asserts TurnFailed.StopReason==image_error, the exact headline "an image in the request could not be processed", the exact vendor message, and the surviving partial answer. | covered |
| `!fail-malformed-tool-use` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestTerminalReasonTurnFailedArms/MalformedToolUseExhausted asserts TurnFailed.StopReason==malformed_tool_use_exhausted, the exact headline, the exact vendor message, and the surviving partial answer. | covered |
| `e2e-fail-this-turn` (marker) | ungrounded | mergequeue_e2e_test.go | — | — | Go: TestFailMarkerFailsABeforeActionRunAndRidesAnAfterActionTerminal configures the marker as a merge ACTION (the one prompt the daemon submits verbatim) and asserts the pair the marker exists for: the same failing turn settles the pre_prompt tab failed with the daemon's composed summary and ends the run at FeedMergeError.failed with no later tab ever opened, while as an after-action it settles the post_prompt tab failed and the run still reaches FeedMergeSuccess. | covered |
| `!fail-max-turns` | grounded | turnlifecycle_e2e_test.go | — | — | Go: TestTurnStopMaxTurns asserts errored.GetMaxTurns()!=nil plus headline. | covered |
| `!fail-model` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestTerminalReasonTurnFailedArms/ModelError asserts TurnFailed.StopReason==model_error and the exact headline "the model errored in a way the API did not classify" — specifically NOT the refusal sentence, which needs a witnessing response frame — plus the exact vendor message and the surviving partial answer. | covered |
| `!fail-prompt-too-long` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestTerminalReasonTurnFailedArms/PromptTooLong asserts TurnFailed.StopReason==prompt_too_long (NOT request_too_large, the vendor's 413), the exact headline, the exact vendor message, and the surviving partial answer. | covered |
| `!fail-rapid-refill` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestTerminalReasonTurnFailedArms/RapidRefillBreaker asserts TurnFailed.StopReason==rapid_refill_breaker, the exact headline naming it a wait rather than a fault, the exact vendor message, and the surviving partial answer. | covered |
| `!fail-stop-hook` | declared-only | turnlifecycle_e2e_test.go | — | — | Go: TestTurnStopHookStop asserts errored.GetStopHookPrevented()!=nil plus headline. | covered |
| `!fail-structured-output` | declared-only | turnlifecycle_e2e_test.go | — | — | Go: TestTurnStopMaxStructuredOutputRetries asserts exact StopReason==structured_output_retry_exhausted. | covered |
| `!fail-tool-deferred` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestTerminalReasonTurnFailedArms/ToolDeferred asserts TurnFailed.StopReason==tool_deferred, the exact headline "the run ended waiting on a deferred tool call", the exact vendor message, and the fate of the deferred work — the partial answer survives, settled. | covered |
| `!fail-tool-deferred-unavailable` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestTerminalReasonTurnFailedArms/ToolDeferredUnavailable asserts TurnFailed.StopReason==tool_deferred_unavailable, the exact headline distinguishing it from the plain deferral, the exact vendor message, and the surviving partial answer. | covered |
| `!fail-turn-setup` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestApiRequestFailedArms/TurnSetupFailed asserts the named arm `FeedTurnEndedErrored.turn_failed`, the exact stop reason `turn_setup_failed`, the exact per-arm headline “the run could not be set up and never reached the model”, the exact vendor message, and that partial work in flight survives the failure. | covered |
| `!fast-cooldown` | ungrounded | — | — | — | No counted e2e layer drives this scenario: the topbar's fast-mode cell was retired (owner ruling, 2026-10-02), so no frontend surface draws the state. | uncovered |
| `!fast-off` | ungrounded | — | — | — | No counted e2e layer drives this scenario: the topbar's fast-mode cell was retired (owner ruling, 2026-10-02), so no frontend surface draws the state. | uncovered |
| `!fast-on` | grounded | turnlifecycle_e2e_test.go | — | — | Go: TestFastMode asserts the turn concludes on the exact prose “Fast mode is on.”, which pins which state the vendor reported; the topbar no longer draws fast mode (owner ruling, 2026-10-02). | covered |
| `!fault-converter` | ungrounded | producerfaults_e2e_test.go | — | — | Go: TestConverterDefectOpensADegradedWindow asserts three things — the turn still CONCLUDES, the malformed hook_started drew NO hook card (it reached no arm), and the diagnostics opened a TopbarDegradedWindowWarningDetail with a stated component, a stated reason and a positive began_at_ms. | covered |
| `!fault-recover` | ungrounded | producerfaults_e2e_test.go | — | — | Go: TestConverterRecoveryClosesTheDegradedWindow asserts THAT window closed — matched by component plus began_at_ms, not merely "some closed window exists" — with ended_at_ms >= began_at_ms and a dropped_count >= 1 for the one message the converter refused. | covered |
| `!findings` | grounded | remainder_e2e_test.go | feed-families.layer.test.ts | — | Go: TestReportFindings asserts 3 rows with exact Verdict/Outcome per row. Web: feed-families.layer asserts [data-finding] count>0. | covered |
| `!glob` | declared-only | filetools_e2e_test.go | — | — | Go: TestGlob asserts a lines output form and exact matched-path line count. | covered |
| `!grep-content` | declared-only | filetools_e2e_test.go | — | — | Go: TestGrepContentFilesCount asserts lines form, min line count, omitted-floor composed. | covered |
| `!grep-count` | declared-only | filetools_e2e_test.go | — | — | Go: TestGrepContentFilesCount (count subtest) asserts text output form, non-empty composed count. | covered |
| `!grep-files` | declared-only | filetools_e2e_test.go | — | — | Go: TestGrepContentFilesCount (files subtest) asserts lines output, no omitted floor. | covered |
| `!hold` | grounded | permission_e2e_test.go | perf.layer.test.ts, roster.layer.test.ts, surfaces.layer.test.ts | emacs_workspace_e2e_test.go | Go: TestHeldTurnGate asserts DaemonHoldTray carries the held prompt, tray empties on release. Web: roster.layer asserts [data-held-turn] elements and unchanged userPrompt count; surfaces.layer only checks status differs (weak component). | covered |
| `!hook-blocked` | grounded | hooks_e2e_test.go | feed-families.layer.test.ts | — | Go: TestHookBlocked asserts exact FeedHookBlocked.Reason string. Web: feed-families.layer only checks dataset.unit==hook (tautological, weak). | covered |
| `!hook-cancelled` | grounded | hooks_e2e_test.go | — | — | Go: TestHookCancelled asserts no hook card drawn plus exact log Level==info. | covered |
| `!hook-failed` | grounded | hooks_e2e_test.go | feed-families.layer.test.ts | — | Go: TestHookFailed asserts exact ExitCode==1 and exact Output.Text. Web: feed-families.layer only checks dataset.unit==hook (weak). | covered |
| `!hook-success` | grounded | hooks_e2e_test.go | feed-families.layer.test.ts | — | Go: TestHookSucceeded asserts no hook card drawn plus exact log Context[hook]==PreToolUse:Read. Web: feed-families.layer asserts specific negative (no hook row). | covered |
| `!ide-diagnostics` | grounded | filetools_e2e_test.go | — | — | Go: TestIdeDiagnosticsAfterEdit asserts a diff form alongside Diagnostics with composed lines. | covered |
| `!ide-diagnostics-write` | grounded | filetools_e2e_test.go | — | — | Go: TestIdeDiagnosticsAfterWrite asserts the diagnostics hang off the tool card NAMED "Write", with composed lines — the write arm of the adjacency join, which a defect once folded onto the edit arm. | covered |
| `!interrupt` | grounded | interrupt_e2e_test.go, keepalive_e2e_test.go | — | emacs_interrupt_e2e_test.go | Go: TestInterruptAfterTextDelta asserts InterruptedTurn!=nil, terminal specifically Interrupted. Emacs: TestEmacsForcedRestartInterruptsTheTurn asserts roster arm settles to :interrupted specifically. | covered |
| `!keepalive` | ungrounded | producerfaults_e2e_test.go | — | — | Go: TestKeepaliveTurnIsOrdinaryAndUnmarked asserts the ordinary conclusion, the exact row set (prompt + response + terminal and nothing more), and the specific negative that no `<!--agent-repl:keepalive-->` marker is minted onto the prompt row. (TestKeepAliveNeverAppearsOnWire in hibernation_e2e_test.go is the daemon-minted keep-alive, a different fact.) | covered |
| `!max-tokens` | grounded | turnlifecycle_e2e_test.go | — | — | Go: TestMaxTokens asserts Concluded plus an exact truncated prose string. | covered |
| `!mcp-all` | grounded | mcpmonitors_e2e_test.go, sessionfacts_e2e_test.go | — | — | Go: TestMcpServerHealths asserts exact per-server oneof-arm mapping and exact Failed.Detail.Text. | covered |
| `!mcp-healthy` | ungrounded | sessionfacts_e2e_test.go | — | — | Go: TestMcpCatalogNarrowedToHealthyKeepsTheOmittedRows states the five-server catalog first, then narrows to it, and asserts all five McpPanelRow rows STAND with their healths unchanged — the specific negative that "absent from the newest catalog" is not "gone" (only an UNSET health arm drops a row). | covered |
| `!mcp-tool` | grounded | mcpmonitors_e2e_test.go | feed-families.layer.test.ts | — | Go: TestMcpToolCall asserts the turn's settled FeedSimpleToolCall headed mcp__echo__echo carries the tool's text, and no TopbarWarning.UnmodeledTool names it. Web: feed-families.layer asserts the generic shell's .tool-name and output body. | covered |
| `!md` | ungrounded | — | feed-families.layer.test.ts | — | Web: feed-families.layer asserts literal !md text in the user_prompt row and exact 'Markdown showcase' body text. | covered |
| `!memory` | declared-only | remainder_e2e_test.go | — | — | Go: TestContextInjectedMemory asserts AgentContextInjected.GetMemory() non-nil with non-empty Path/Content. | covered |
| `!model-fallback` | grounded | turnlifecycle_e2e_test.go | — | — | Go: TestModelChanged asserts TopbarModelSelector.selected.model.name is `fake-sonnet-5` — the model the VENDOR swapped to unasked, which session.proto:173-174 says is stated in one authoritative place whether or not the consumer asked for it. | covered |
| `!monitor-deadline` | grounded | mcpmonitors_e2e_test.go | — | — | Go: TestMonitorDeadline asserts exact Description, Persistent==false, chip count==1. | covered |
| `!monitor-persistent` | grounded | mcpmonitors_e2e_test.go | — | — | Go: TestMonitorPersistent asserts exact Description, Persistent==true. | covered |
| `<!--agent-repl:network-resume-->` (marker) | ungrounded | — | — | — | No counted e2e layer drives this scenario; the shim's own network-resume prompt selects it, and the shim integration suite drives it end to end (test/integration/detached.test.ts). | uncovered |
| `!perm-allow-once` | grounded | permission_e2e_test.go | — | — | Go: TestPermissionAskAnsweredArms/AllowOnce asserts AllowedOnce()!=nil, tool succeeded, turn concluded. | covered |
| `!perm-allow-standing` | grounded | permission_e2e_test.go | — | — | Go: TestPermissionAskAnsweredArms/AllowStanding asserts AllowedStanding()!=nil. | covered |
| `!perm-allow-standing-mode` | grounded | permission_e2e_test.go | cards.layer.test.ts | — | Go: TestPermissionModeChangedMidSession asserts exact topbar PermissionModePicker.Current.Mode==accept_edits. Web: cards.layer asserts [data-permission=allowStanding] selector. | covered |
| `!perm-deny-policy` | grounded | permission_e2e_test.go | cards.layer.test.ts | — | Go: TestPermissionDeniedByPolicy asserts DeniedByPolicy()!=nil, sawOpen false. Web: cards.layer asserts a new tool-call row and zero refusalArms. | covered |
| `!perm-deny-user` | grounded | permission_e2e_test.go | — | — | Go: TestPermissionAskAnsweredArms/DeniedByUser asserts DeniedByUser()!=nil, tool Denied not Failed. | covered |
| `!perm-hold` | grounded | daemonstop_e2e_test.go, permission_e2e_test.go | cards.layer.test.ts | emacs_roster_e2e_test.go | Go: TestPermissionUndecidableParked asserts ask stays Open, no push while parked, then Interrupted. Web: cards.layer asserts [data-permission] buttons/.perm-waiting. Emacs: TestEmacsPermissionAskFiresTheAttentionMarker pins exact blink-schedule sequence. | covered |
| `!perm-no-standing` | ungrounded | permission_e2e_test.go | — | — | Go: TestPermissionAskOffersNoStanding asserts FeedPermission.standing_offered UNSET on the open card (feed.proto: "PRESENT iff the vendor offered a standing form"), still UNSET after answering, the allowed_once settle, and that the gated call then ran and the turn concluded. | covered |
| `!perm-undecidable` | ungrounded | permission_e2e_test.go | cards.layer.test.ts | — | Go: TestPermissionDeniedForWantOfDecider drives it for real and asserts FeedPermissionAnswered.denied_undecidable BY NAME (landing 10's own arm, never folded onto denied_by_policy), the composed "denied for want of a decider" wording plus the vendor's own detail clause, the negative that no open ask was ever drawn, and the concluded turn. | covered |
| `!plan` | grounded | remainder_e2e_test.go | feed-families.layer.test.ts | — | Go: TestPlanModeEnterExit asserts FeedPlan.Planned non-nil with non-empty Prose/Edit.Path. Web: feed-families.layer asserts .plan-prose,.plan-planned. | covered |
| `!push-config-off` | grounded | remainder_e2e_test.go | — | — | Go: TestPushNotificationNotSent's `push-config-off` subtest asserts the SPECIFIC NEGATIVE — no feed row of the whole workspace draws the pushed message as an agent_prompt address or as a response — PLUS the surface a push does reach: FooterStatusActivityNotification.text equal to `The offline run finished.` verbatim, read under whichever status arm stands. The disabled reason lives on the vendor's RESULT, so the standing line is the same as the sent arm's; what this row pins is that the `config_off` arm reaches the daemon and draws nothing extra. | covered |
| `!push-no-transport` | grounded | remainder_e2e_test.go | — | — | Go: TestPushNotificationNotSent's `push-no-transport` subtest asserts the SPECIFIC NEGATIVE — no feed row of the whole workspace draws the pushed message as an agent_prompt address or as a response — PLUS the surface a push does reach: FooterStatusActivityNotification.text equal to `The offline run finished.` verbatim, read under whichever status arm stands. The disabled reason lives on the vendor's RESULT, so the standing line is the same as the sent arm's; what this row pins is that the `no_transport` arm reaches the daemon and draws nothing extra. | covered |
| `!push-sent` | grounded | remainder_e2e_test.go | — | — | Go: TestPushNotificationSent asserts the SPECIFIC NEGATIVE — no feed row of the whole workspace draws the pushed message as an agent_prompt address or as a response — PLUS the surface a push does reach: FooterStatusActivityNotification.text equal to `The offline run finished.` verbatim, read under whichever status arm stands. | covered |
| `!push-user-present` | grounded | remainder_e2e_test.go | — | — | Go: TestPushNotificationNotSent's `push-user-present` subtest asserts the SPECIFIC NEGATIVE — no feed row of the whole workspace draws the pushed message as an agent_prompt address or as a response — PLUS the surface a push does reach: FooterStatusActivityNotification.text equal to `The offline run finished.` verbatim, read under whichever status arm stands. The disabled reason lives on the vendor's RESULT, so the standing line is the same as the sent arm's; what this row pins is that the `user_present` arm reaches the daemon and draws nothing extra. | covered |
| `!query-eof` | ungrounded | producerfaults_e2e_test.go | feed-families.layer.test.ts | — | Go: TestQueryEofEndsTheTurnAsQueryDied asserts the terminal is specifically FeedTurnEndedErrored.query_died with cause unexpected_eof (never concluded, never a generic vendor failure) plus a composed headline; TestQueryDiedFailsTheFootersTurn asserts the footer's failed turn (FooterSubStatusIdleTurnFailed; a dead query is a failed turn, never a block) and its FooterStatusActivityQueryDied line. | covered |
| `!query-eof-mid-ask` | ungrounded | producerfaults_e2e_test.go | — | — | Go: TestQueryEofMidAskDeniesTheOpenAsk asserts the ask genuinely OPENED, then that the death settles it — FeedPermissionAnswered.denied_by_user, the gate's stand-down — and that the turn still ends on query_died. The fate of the in-flight ask, not only the notice. | covered |
| `!query-fail` | ungrounded | producerfaults_e2e_test.go | query-death.layer.test.ts | — | Go: TestQueryFailEndsTheTurnAsQueryDied asserts FeedTurnEndedErrored.query_died with cause iterator_failure specifically, separating the rejecting iterable from the EOF half. | covered |
| `!queue-vendor-turn` | ungrounded | hibernation_e2e_test.go, keepalive_e2e_test.go | — | — | Go: TestCompletedTaskBesideKeepAliveAnchorsTheRewind asserts the next rewind anchors on the completed task's adopted turn and discards exactly [keepalive], no rewind is refused, and the vendor's own answer is drawn; TestKeepAliveAnswerAfterVendorTurnNeverServed asserts no keep-alive row is served beside it. | covered |
| `!rate-limit` | ungrounded | sessionfacts_e2e_test.go | — | — | Go: TestRateLimitOverageWindowFeedsTheOverageAllowance asserts the exact conclusion prose, the event's `allowed_warning` verdict and 0.79 utilization on the overage `FooterAllowance` of the enduring usage line, a non-zero `resets_at_s`, and the specific negative that no salient line stands, with the harness warning sweep holding that the retired `daemon.footer.rate_limit_overage` warn stays gone. | covered |
| `!rate-limit-five-hour` | ungrounded | sessionfacts_e2e_test.go | — | — | Go: TestRateLimitFiveHourWindowFeedsTheSessionAllowance asserts the session `FooterAllowance` of the enduring usage line carries the event's `allowed_warning` verdict, its 0.82 utilization and a non-zero `resets_at_s`, and the specific negative that no salient line stands. | covered |
| `!rate-limit-seven-day` | ungrounded | sessionfacts_e2e_test.go | — | — | Go: TestRateLimitSevenDayWindowFeedsTheWeeklyAllowance asserts the weekly `FooterAllowance` of the enduring usage line carries the event's `allowed_warning` verdict, its 0.91 utilization and a non-zero `resets_at_s`, the specific negative that no salient line stands, and the event's session-arm record. | covered |
| `!read` | grounded | filetools_e2e_test.go, footeractivity_e2e_test.go | feed-families.layer.test.ts | — | Go: TestReadWholeHeadRange asserts code output form, paint spans, omitted-line composed for the cut. Web: feed-families.layer asserts .tool-name/[data-output-body]. | covered |
| `!read-head` | grounded | filetools_e2e_test.go | — | — | Go: TestReadWholeHeadRange (head subtest) asserts omitted line composed for the cut. | covered |
| `!read-image` | grounded | filetools_e2e_test.go | — | — | Go: TestReadImageSettlesWithNoOutput asserts the card SETTLED with the succeeded verdict and specifically the FeedToolCallNoOutput arm — an unset AgentReadSuccess.extent draws no output form, and a perpetually-running card would be the defect. | covered |
| `!read-range` | grounded | filetools_e2e_test.go | — | — | Go: TestReadWholeHeadRange (range subtest) asserts paint spans, no omitted line. | covered |
| `!read-truncated` | ungrounded | filetools_e2e_test.go | — | — | Go: TestReadTruncatedStatesTheCut asserts a code output form with non-empty spans and the exactly composed FeedToolCallCodeOutput.omitted line "showing 2 of 4 lines". The cut ARM is deliberately not asserted here (the two cuts are indistinguishable downstream); it is pinned at the converter. | covered |
| `!refusal-fallback` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestModelRefusalWithFallback asserts the recovery itself — a CONCLUDED turn whose answering response markdown is exactly the fallback model's prose, not merely that the turn ended well. | covered |
| `!refusal-no-fallback` | ungrounded | failurearms_e2e_test.go | — | — | Go: TestModelRefusalWithoutFallback asserts FeedTurnEndedErrored.refusal BY NAME (feed.proto's arm for "the vendor refused to continue; there is no answer prose") plus the composed message. | covered |
| `!residue` | ungrounded | producerfaults_e2e_test.go | — | — | Go: TestResidueAttachmentsDrawNoRow asserts the specific negative the arms line demands — the turn's row set is exactly prompt + response + terminal, so neither `deferred_tools_delta` nor `agent_listing_delta` reached any conversation.v1 arm. | covered |
| `!rotate` | grounded | identityrotation_e2e_test.go | feed-families.layer.test.ts | — | Go: TestClearRotatesIdentity/TestSecondRotateUnderRotatedIdentity assert exact new session-line value, distinct Cleared rows. Web: feed-families.layer only checks dataset.rowKind==separation (tautological, weak). | covered |
| `!send-message` | grounded | remainder_e2e_test.go | — | — | Go: TestSendMessageQueuedAndResumed waits on the DELIVERED version of the sender's own row (FeedAgentPrompt.delivery = queued_to_live), then asserts the composed address `→ a1234567890abcde` verbatim and the body as EXACTLY ONE text block carrying the caller's summary `check the branch`. | covered |
| `!send-message-refused` | ungrounded | remainder_e2e_test.go | — | — | Go: TestSendMessageRefused asserts the attempt SURVIVES the refusal as a FeedAgentPrompt addressed "→ <recipient>" verbatim (no invented identity), the body as the caller's summary only, the delivery's `refused` arm BY NAME (never the unset oneof, which reads as "not yet delivered"), the refusal reason as the vendor's own prose verbatim, and the concluded turn. The GAP this row recorded is CLOSED by landing 14, which added FeedAgentPrompt.delivery.refused{reason}. | covered |
| `!send-message-resumed` | ungrounded | remainder_e2e_test.go | feed-families.layer.test.ts | — | Go: TestSendMessageQueuedAndResumed waits on FeedAgentPrompt.resumed_recipient (the arm that says a DORMANT agent was restarted), asserts the address is the composed `→ <recipient>` form naming the minted recipient — never the `an agent the send did not name` stand-in — and the body is the summary `resume the sweep`. | covered |
| `!skill` | grounded | skills_e2e_test.go | feed-families.layer.test.ts | — | Go: TestSkillInvocation/TestSkillNamedAndArgsParameterized assert exact byte-for-byte Document.Markdown match. Web: feed-families.layer only checks dataset.unit==skill (weak). | covered |
| `!skill-fail` | ungrounded | skills_e2e_test.go | feed-families.layer.test.ts | — | Go: TestSkillFailed asserts the skill card's `failed` arm against all three neighbours (not loaded, not denied), the EXACT composed reason “Error: no such skill: absent-skill”, and that the invocation line still names the attempted skill. Web: feed-families.layer only checks [data-state] is present and non-empty, which the running and loaded arms satisfy too. | covered |
| `!skills-injected` | declared-only | remainder_e2e_test.go | — | — | Go: TestContextInjectedSkills asserts injected.GetSkills()!=nil and non-empty Skills slice. | covered |
| `!slash` | grounded | slashcommands_e2e_test.go | — | — | Go: TestVendorAnsweredSlashCommand/TestSlashShapeBViaSlash assert exact answering prose string and exact transcript record shape. | covered |
| `!slash-shape-a` | ungrounded | slashcommands_e2e_test.go | — | — | Go: TestSlashShapeANamed asserts exact transcript content string and exact conclusion text. | covered |
| `!slash-shape-a-unnamed` | ungrounded | slashcommands_e2e_test.go | — | — | Go: TestSlashShapeAUnnamed asserts exact transcript content string, absence of command-name, exact conclusion text. | covered |
| `!stop-on-rewind` | ungrounded | keepalive_e2e_test.go | — | — | Go: TestStopReplayedByEveryRewindNeverLoops asserts, over three replays of the real CLI's transcript-only stop, that every rewind anchors on the arming real turn and discards exactly [keepalive], no rewind is refused, the feed draws nothing of the stop and holds exactly the two real turns, and the store holds no row carrying it. | covered |
| `!subagent` | grounded | refusals_e2e_test.go, subagents_e2e_test.go | feed-families.layer.test.ts, subfeeds.layer.test.ts | — | Go: TestSubagentSyncNestedActivity/TestBubbleRefusedNotDeliverable assert exact Label/Description text, sub-feed confinement. Web: subfeeds.layer asserts exact SYNC_COMMISSION string, feedContainer structure. | covered |
| `!subagent-detached` | grounded | subagents_e2e_test.go | feed-families.layer.test.ts, subfeeds.layer.test.ts | — | Go: TestSubagentDetached asserts root row is the detached wrapper, exact completion text confined to sub-feed. Web: subfeeds.layer asserts exact DETACHED_COMMISSION string. | covered |
| `!subagent-detached-hold` | ungrounded | subagents_e2e_test.go | — | — | Go: TestSubagentDetachedSurvivesAnInterjection interjects the held turn with an explicit `stop` and asserts turn_ended.interrupted, then proves the agent is still LIVE by an answer (Interrupt(detached) on its bubble succeeds with interrupted_detached, which a stopped agent refuses) and that the bubble then settles cancelled. | covered |
| `!subagent-detached-live` | ungrounded | refusals_e2e_test.go | — | — | Go: TestBubbleRefusedAgentBusy asserts AgentBusy()!=nil; TestUnknownAgentOnUpdateAgent asserts exact Connect code + message substring. | covered |
| `!subagent-detached-utterance` | grounded-in-shape | subagents_e2e_test.go | — | — | Go: TestSubagentDetachedUtteranceStaysOffTopLevel asserts bubble stays Live, exact utterance text confined to sub-feed (positive+negative on exact string). | covered |
| `!subagent-failed` | ungrounded | subagents_e2e_test.go | — | — | Go: TestSubagentFailed asserts the DETACHED wrapper placement, the head still naming the commission, and FeedSubagentSettled.outcome == failed with all three neighbouring arms (succeeded, cancelled, lost) checked ABSENT — the discrimination a resolver that collapsed the four would fail. | covered |
| `!subagent-interleaved` | grounded-in-shape | subagents_e2e_test.go | — | — | Go: TestSubagentInterleavedResponsesStayOnTheirOwnFeeds asserts the main turn draws exactly ONE thinking unit and ONE final_answer response, each with its whole exact text, the subagent's two exact responses on its sub-feed and absent from the root feed, and the bubble settled succeeded. | covered |
| `!subagent-network-failed` | grounded | — | — | — | No counted e2e layer drives this scenario; the shim integration suite drives it (test/integration/detached.test.ts). Grounded in the 2026-09-27 outage's production records, not a capture. | uncovered |
| `!subagent-resumed` | ungrounded | — | — | — | No counted e2e layer drives this scenario; the shim integration suite drives it across a shim restart (test/integration/detached.test.ts). | uncovered |
| `!task-change` | grounded | remainder_e2e_test.go | — | — | Go: TestTaskActsCreateChangeReject asserts a checklist row with Status.GetRunning()!=nil. | covered |
| `!task-create` | grounded | remainder_e2e_test.go | — | — | Go: TestTaskActsCreateChangeReject asserts turn concluded and footer LiveWork.Tasks.Total==2. | covered |
| `!task-reject` | grounded | remainder_e2e_test.go | — | — | Go: TestTaskActsCreateChangeReject asserts checklist rows persist and LiveWork.Tasks chip still non-nil after rejection. | covered |
| `!tokens-reminder` | ungrounded | producerfaults_e2e_test.go | — | — | Go: TestTokensReminderDrawsNoRow asserts the only-prose-rows negative AND the named negative that no `context_budget` footer line was minted from a TOKEN-COUNT reminder. | covered |
| `!unmodeled` | grounded | mcpmonitors_e2e_test.go | surfaces.layer.test.ts | — | Go: TestUnmodeledTool asserts exactly 1 TopbarWarning.UnmodeledTool for StructuredOutput, feed rows never mention it. Web: surfaces.layer asserts .topbar-warnings present and unchanged tool-call row count. | covered |
| `!usage-available` | grounded | accounting_e2e_test.go | — | — | Go: TestAccountUsageAvailableArm drives this arm's OWN spelling (rather than only its `!usage-full` alias) and asserts both halves: the resolver's `daemon.footer.on_session_update` record with arm=account_usage, AND the specific negative that these sub-threshold figures draw NO FooterStatusActivityRateLimited line — so the sample landed and the newsworthiness gate held. | covered |
| `!usage-full` | grounded | accounting_e2e_test.go | surfaces.layer.test.ts | — | Go: TestAccountUsage (same log-record assertion as usage-available). Web: surfaces.layer asserts .footer-tokens/.footer-clock elements and a footer panel with named data-panel. | covered |
| `!usage-historical` | ungrounded | subagents_e2e_test.go | — | — | Go: TestNestedSubagentHistoricalUsage asserts specifically that NO subagent bubble row is fabricated on the root feed (a shape-absence check) alongside Concluded. | covered |
| `!usage-opus-absent` | ungrounded | sessionfacts_e2e_test.go | — | — | Go: TestAccountUsageOpusAbsentIsReadAndRetiresTheLine stands a rate line on the weekly EVENT over an unread probe, drives this scenario, and asserts the drawn consequence of an available outcome that is NOT an unavailability: its reprobe READS and re-files the sub-threshold weekly figure, so `FooterStatusActivityRateLimited` goes away entirely (a specific negative an unread arm would not produce). Exact conclusion prose pinned too. | covered |
| `!usage-sampling-failure` | ungrounded | sessionfacts_e2e_test.go | — | — | Go: the /sampling_failure subtest of TestAccountUsageUnreadArmsLeaveTheReadFiguresStanding (the standing 0.41 figure, its read instant, and NO caveat drawn, per the 2026-09-15 ruling). TestAccountUsageSamplingFailureCarriesACause additionally pins the shim's own cause on the daemon's `usage_sample_unreadable` breadcrumb. This arm was UNREACHABLE until the mock was fixed to raise (see PROTO-CHANGES Landing 13's dead-trigger note). | covered |
| `!usage-service-unavailable` | ungrounded | sessionfacts_e2e_test.go | — | — | Go: TestAccountUsageUnreadArmsLeaveTheReadFiguresStanding/service_unavailable asserts the daemon's `usage_sample_unreadable` breadcrumb names this reason, and that the line the weekly event then opens still draws the session 0.41 a prior READABLE sample filed, with that reading's `figures_read_at_ms` and no unread caveat (owner ruling fc4917be4). Exact conclusion prose pinned. | covered |
| `!usage-utilization-unavailable` | ungrounded | sessionfacts_e2e_test.go | — | — | Go: the /utilization_unavailable subtest of the same table — the breadcrumb names the reason, the standing 0.41 figure and its read instant are untouched, no caveat is drawn, and the exact conclusion prose. | covered |
| `!usage-window-unavailable` | ungrounded | sessionfacts_e2e_test.go | — | — | Go: the /window_unavailable subtest of the same table — the breadcrumb names the reason, the standing 0.41 figure and its read instant are untouched, no caveat is drawn, and the exact conclusion prose. | covered |
| `!vendor-backgrounded` | grounded | detachedbash_e2e_test.go | — | — | Go: TestVendorBackgroundedTaskStartIsNoDetachment asserts the half that IS reachable — the vendor announces a live `local_bash` task for a call that has not left the turn, and the surface draws NO detached_shell row for it (convert/detached.ts: "A SHELL TASK IS NOT A DETACHMENT YET") while the unit's fate stays a running foreground FeedSimpleToolCall. GAP, recorded at the test: the detachment itself needs shim.v1's DetachForeground, which no agentrepl.v1 rpc exposes and the daemon never issues (PROTO-CHANGES.md Landing 8), so `backgroundedByUser`/AgentSuccess.backgrounded stay unreachable from this layer. | covered |
| `!wakeup-schedule` | grounded | remainder_e2e_test.go | — | — | Go: TestScheduleWakeupScheduleAndStop asserts the no-feed-row fact's positive half on the footer: FooterSubStatusWaitingWakeup stands while the schedule does, and the ⏱ live-work chip counts the pending wakeup — both read off the ONE view they are resolved from. | covered |
| `!wakeup-stop` | grounded | remainder_e2e_test.go | — | — | Go: TestScheduleWakeupScheduleAndStop asserts the stop RETIRES both arms the schedule raised: the waiting-wakeup status is gone and live_work.crons is unset. | covered |
| `!web-fetch` | grounded | remainder_e2e_test.go | — | — | Go: TestWebFetch asserts tool call Succeeded, non-empty Text. Web: feed-families.layer only checks .tool-head exists and non-empty text (weak; no link-specific selector despite the file's own comment claiming one). | covered |
| `!web-fetch-redirect` | ungrounded | webtools_e2e_test.go | — | — | Go: TestWebFetchRedirect asserts four named facts — the succeeded verdict for a 302 (and not failed), the body opening "302 Found", the vendor's redirect instruction kept verbatim, and the input link naming the URL THE AGENT ASKED FOR rather than the redirect destination. | covered |
| `!web-search` | grounded | remainder_e2e_test.go | feed-families.layer.test.ts | — | Go: TestWebSearch asserts tool call Succeeded, non-empty Links. | covered |
| `!worktree-keep` | declared-only | remainder_e2e_test.go | feed-families.layer.test.ts | — | Go: TestWorktreeEnterExitKeptAndRemoved asserts exact WorktreeEntered/WorktreeLeft.Kept Path.Text. Web: feed-families.layer asserts count grew by >=2 and .sep-worktree class. | covered |
| `!worktree-remove` | declared-only | remainder_e2e_test.go | — | — | Go: TestWorktreeEnterExitKeptAndRemoved asserts Separation.WorktreeLeft.Removed.Discarded!=nil. | covered |
| `!write-create` | grounded | filetools_e2e_test.go | — | — | Go: TestWriteCreatedAndUpdated asserts a diff output form with the write's hunk lines. | covered |
| `!write-update` | grounded | filetools_e2e_test.go | — | — | Go: TestWriteCreatedAndUpdated (update subtest) asserts diff form with hunk lines. | covered |

## (b) Counts

DERIVED FROM TABLE (a) ABOVE, by counting its verdict and layer columns.
`TestScenarioMatrixMatchesReality` recomputes every figure here and fails on
a disagreement, so these are not hand tallies (they were, and they were
wrong: the by-layer lines once read 33 and 5 where the table's columns held
32 and 3).

- Covered (at least one STRONG, specific-shape assertion in a counted layer): **153**
- Weak (a counted layer drives the scenario but only asserts turn-completion or a non-specific field, never a named arm/shape): **0**
- Uncovered (no counted layer drives the scenario at all): **5**
- Total canonical scenarios: 158

By layer, scenarios with at least one hit:
- Go e2e (non-emacs): 152 scenarios referenced across 28 files
- Webapp layer: 36 scenarios referenced across 8 files
- Emacs e2e: 3 scenarios referenced across 3 files

## (c) Uncovered and weak scenarios

DERIVED FROM TABLE (a)'s verdict column. This section used to be
hand-written prose grouped by priority area, and it lagged (a) badly enough
that the two disagreed about whether a scenario was covered at all — one
scenario was present in (a) and absent here. The area grouping is gone
because it was the thing that drifted; the judgment that produced it lives in
each row's `Strongest assertion` cell, which is where a reader can act on it.

### Uncovered — no counted e2e layer drives these at all

<!-- BEGIN DERIVED: uncovered -->

- `!fast-cooldown`
- `!fast-off`
- `!subagent-network-failed`
- `!subagent-resumed`
- `<!--agent-repl:network-resume-->` (marker)

<!-- END DERIVED: uncovered -->

### Weak — a counted layer drives these, but the assertion is not specific

A weak verdict is a HUMAN reading of the test, so this list is only as good
as the `Strongest assertion` cells behind it; the check verifies that each
scenario named here really is driven, not that the reading is right.

<!-- BEGIN DERIVED: weak -->

_None._

<!-- END DERIVED: weak -->

## (d) Ungrounded fakes (no real capture backs them)

Reproduced from `agent-shim/claude/shim/AGENTS.md` and
`testdata/captures/MANIFEST.md`; "what a real capture would need" is this
report's own inference from each scenario's declared shape, not a landed
plan.

| Scenario | Why ungrounded | What a real capture would need |
|---|---|---|
| `!api-*` (all 12) | The capture harness quarantines API errors — a failed run is never a golden. | A capture run that deliberately provokes each HTTP status (429/529/401/403/400/413/404/500/402/403-oauth/null/418) from the real API, or an explicit exception to the "no failed runs" capture rule. |
| `!refusal-fallback` / `!refusal-no-fallback` | Declared-only; model refusals are not reproducible on demand. | A capture run whose prompt reliably triggers a real model refusal, with and without a configured fallback model. |
| `!context-window` | Declared-only. | A capture run whose prompt is long enough to hit `prompt_too_long` for real. |
| `!query-eof` / `!query-eof-mid-ask` / `!query-fail` | Producer-side failures — the vendor process dying is not a scenario a capture can record (there is no vendor output to capture). | Not captureable by definition; would need a fault-injection layer at the shim's spawn boundary instead of a vendor recording. |
| `!fault-converter` / `!fault-recover` | Same — a malformed vendor message is a fabricated defect, not an observed one. | Not captureable; the malformed shape is inherently synthetic (missing a required field the real vendor never omits). |
| `!cold-seed` | The cold-context condition is tripped on a LATER resume, which no single capture run spans. | Two linked captures: an initial run, then a resume captured ≥2 hours later against the same session. |
| `!compact-auto` / `!compact-failed` | `!compact` itself is now grounded (re-captured 2026-09-03), but no capture exists with `trigger:"auto"` or with a failed compaction. | An auto-triggered compaction capture (context filled past the auto threshold without `/compact`), and a capture where `/compact` itself errors. |
| `!context-budget-warning` | Explicit orchestrator ruling: invented so the converter's already-built arm has a fake-SDK path; two Haiku capture attempts (2026-09-03) both failed to produce a real budget-warning attachment. | A capture run that actually fills context enough to trigger the vendor's own budget-warning attachment — the MANIFEST records a third, untried lever (paginated `Read` over smaller files) as the next attempt. |
| `!bash-detach-poll` | `TaskOutput` is a declared vendor tool but no capture ever calls it — every recorded backgrounded run was checked by re-reading its spool. | A capture run where the model explicitly calls `TaskOutput` to poll a backgrounded task instead of re-reading the spool. |
| `!usage-historical` | No capture carries a FILE-plane-only historical usage record attributed to a nested (spawnDepth 2) subagent. | A capture with a nested subagent whose usage record arrives file-plane-only, with no paired stream-plane `message_start`. |
| `!subagent-detached-live` | No capture leaves a subagent live post-turn with a gated ask raised under it. | A capture where a detached subagent is still running when the turn ends, and itself raises a `canUseTool` ask before the capture stops. |
| `!subagent-detached-utterance` | Grounded in shape only (reuses `subagent-detached`'s launch machinery); the utterance's own placement is invented. | A capture where a detached subagent's only post-turn activity is one ordinary sidechain text line with no completion. |
| `!subagent-failed` | Declared-only; no capture has a subagent end in failure. | A capture run where a launched subagent errors out rather than completing. |
| `!slash-shape-a` / `!slash-shape-a-unnamed` | Invented; built from `machinery_e2e_test.go`'s own hand-fabricated constants, not a capture. | A capture of the CLI's own raw slash-command bookkeeping record (distinct from `!slash`'s vendor-answered case), named and unnamed. |
| `!perm-no-standing` | No capture has an ask with zero `suggestions`. | A capture where a gated call's ask offers no standing-rule suggestions at all. |
| `!perm-undecidable` | KNOWN-OPEN: the similarly-named `permission-undecidable-parked` capture actually grounds `!perm-hold`, not this scenario; `sdk.d.ts` declares no classifier-vs-policy discriminator. | A capture (or an `sdk.d.ts` update) that distinguishes a classifier's "no verdict reached" from an ordinary policy deny. |
| `!rate-limit` / `!rate-limit-five-hour` / `!rate-limit-seven-day` | No capture carries a `rate_limit_event` record at all. | A capture run made while the account is actually near/over a rate-limit window. |
| `!read-truncated` | No capture hits the token cap on a `Read` call. | A capture reading a file large enough to trip the `Read` tool's own per-call token ceiling. |
| `!web-fetch-redirect` | No capture receives a 302 from `WebFetch`. | A capture fetching a URL that actually redirects. |
| `!usage-opus-absent` / `!usage-sampling-failure` / `!usage-service-unavailable` / `!usage-utilization-unavailable` / `!usage-window-unavailable` | No capture reproduces any of the five account-usage negative/absent shapes (only the `available`/`full` shape is captured). | Captures taken under each specific account-usage degraded condition (opus window absent from the plan, backend sampling failure, service outage, utilization unavailable, window unavailable). |
| `!away-summary` | No capture carries this record. | A capture of a session resumed after enough elapsed time that the vendor emits its own away-summary recap. |
| `!context-tip` | The one real `context_tip` capture record is a generic CLI tip, not tied to a dedicated capture directory for this scenario. | A capture explicitly isolating the `context_tip` attachment's appearance conditions. |
| `!tokens-reminder` | The one real `total_tokens_reminder` capture (from `artifact-publish-and-list`) is incidental, not a dedicated capture. | A capture dedicated to reproducing the `total_tokens_reminder` attachment on demand. |
| `!residue` | No dedicated capture; the two residue attachments are authored from corpus fixtures. | A capture whose stream happens to carry `deferred_tools_delta`/`agent_listing_delta` attachments. |
| `!keepalive` | No dedicated capture; the keep-alive marker is the shim's own convention, not a vendor shape. | Not really captureable — the marker is added by the shim, not the vendor; grounding would mean confirming the vendor never strips or alters it, via any ordinary capture. |
| `!md` | No capture; the markdown showcase text is authored, not observed. | Not meaningfully captureable — it's a fixed demo string, not a vendor behavior. |
| `!mcp-healthy` | No capture narrows `mcpServerStatus()` to a single connected server (only the five-server `!mcp-all` catalog is captured). | A capture run with exactly one configured, healthy MCP server. |
| `!fast-off` / `!fast-cooldown` | No capture for these two fast-mode states (only `!fast-on`'s `fast-mode` capture exists). | Captures taken with fast mode explicitly disabled by preference, and with it in cooldown from extra-usage exhaustion. |
| `!glob` / `!grep-content` / `!grep-files` / `!grep-count` | The model chose `Bash` instead of the typed tool in the recorded runs; a capture exists but never calls the typed tool. | A capture where the model is steered (or simply happens) to call `Glob`/`Grep` directly instead of shelling out. |
| `!artifact-publish` / `!artifact-list` | Same model-choice gap: the source capture (`artifact-publish-and-list`) exists but the model never called the typed `Artifact` tool for either act. | A capture where the model calls `Artifact` publish/list directly rather than Bash/Skill. |
| `!wakeup-schedule` / `!wakeup-stop` | Same model-choice gap: captured via Skill/Bash in the source run. | A capture where the model calls `ScheduleWakeup` directly. |
| `!worktree-keep` / `!worktree-remove` | Same model-choice gap. | A capture where the model calls `EnterWorktree`/`ExitWorktree` directly rather than Skill/Bash. |
| `!memory` / `!skills-injected` | Same model-choice/typed-arm gap — the captured run never surfaced the typed `contextInjected` arm. | A capture where the injected-memory/injected-skills record is the one actually asserted, not inferred. |
| `!send-message-resumed` / `!send-message-refused` | No capture addresses a subagent (resume or refusal case). | Captures of a `SendMessage` call landing on an idle (resume) and a user-stopped (refusal) recipient. |
| `!fail-execution` / `!fail-stop-hook` / `!fail-structured-output` | DECLARED-ONLY: each has a same-named capture, but that capture ended on a DIFFERENT terminal than the one the scenario declares (`success.interrupted`/`success.completed`/`success.completed` respectively, not the failure terminal). | A re-capture of each run engineered to actually reach its intended failure terminal, the same way `compaction-directed` was re-captured to actually compact. |
| `e2e-fail-this-turn` (marker) | Producer-side failure marker; no capture reaches it by construction. | Not captureable — it's a synthetic gate for the daemon's merge-pipeline test, not a vendor shape. |

## (e) Dead triggers

NOW ENFORCED, not surveyed. `TestScenarioMatrixMatchesReality` fails on any
scenario-shaped `!name` literal, in any of the three counted layers, that the
mocked vendor does not register — a prompt that reads like a scenario
selection but silently drives plain prose instead. A literal is
scenario-shaped when its first token is lower-case letters, digits and
hyphens, so a `t.Fatalf` format opening `!%s` is not mistaken for one.

The check found and closed exactly one: `rosterarm_e2e_test.go` submitted
`"!hello"` where it wanted the ordinary default turn. It now submits plain
prose with no `!`, which reaches the same scenario without reading like a
selection that has gone stale.

Two standing notes on tokens that LOOK dead and are not:

- `!prose-streamed` is a registered golden-name ALIAS of the default prose
  scenario, so it resolves. The check reads `registry.ts`'s `ALIASES` for
  exactly this reason, and since it resolves to the default it contributes no
  matrix row.
- `!perm-undecidable` appears inside a doc comment in
  `permission_e2e_test.go` as the negative that comment contrasts
  `!perm-hold` against. A comment is not a literal, so it is not read as a
  trigger; the scenario's real coverage comes from `cards.layer.test.ts`,
  which submits it.

## Sidecar mock-scenario reconciliation (context only — not counted coverage)

The sidecar's own table
(`agent-shim/claude/shim-sidecar/integration/mock_scenarios_test.go`,
`mockScenarios`, 133 rows) is drawn from the SAME registry and is explicitly
documented as restating AGENTS.md's table as data. Reconciling its 133
prompts against the registry's 147 named scenarios:

- **16 registered scenarios have no sidecar mock-table row**: `!bash-detach-poll`,
  `!bash-hold`, `!context-budget-warning`, `!context-tip`, `!perm-allow-standing-mode`,
  `!perm-hold`, `!perm-no-standing`, `!query-eof-mid-ask`, `!rate-limit-five-hour`,
  `!rate-limit-seven-day`, `!slash-shape-a`, `!slash-shape-a-unnamed`,
  `!subagent-detached-live`, `!subagent-detached-utterance`, `!tokens-reminder`,
  `!usage-historical`. These are all scenarios added or renamed after the sidecar
  table was last generated/updated.
- **One row in the sidecar table does not match any registered scenario name**:
  `{Prompt: "!context-budget", ...}` (line ~242). The registry has no scenario
  named `context-budget` — only `context-budget-warning`. Because
  `selectScenario` requires the token to be followed by whitespace/EOF,
  `"!context-budget"` as written does NOT prefix-match `!context-budget-warning`
  (the next character is `-`), so this row's prompt literal falls through to the
  plain-prose default scenario instead of the budget-warning scenario its own
  `BudgetWarning: true` field implies. This is a real drift in the sidecar's own
  test data (out of scope for e2e coverage per the owner's ruling, but likely worth
  a one-line fix — rename the prompt to `"!context-budget-warning"` — whenever
  that file is next touched).

## Arms without an e2e lever (project-lead ruling, 2026-09-04)

Two SPEC.md arms were dropped as e2e tests because they skipped
unconditionally on every run — a perpetual skip is noise, not coverage, per
project-lead ruling. Each is covered only at the unit level, cited below; a
future documented scenario or env lever could re-open an e2e test for
either, but none exists on this branch.

- **KillTurnFailure.cause.not_the_open_turn** (formerly SPEC.md #55,
  `TestKillTurnNotTheOpenTurn`): an internal daemon/shim race — the daemon
  always names its own currently-tracked open turn, so no client input can
  select this arm — with no documented scenario or env lever and no harness
  seam to force the underlying TOCTOU race deterministically. Covered by
  shim unit/integration tests only: `agent-shim/claude/shim/test/service/failures.test.ts`
  (`describe("killTurnFailure", ...)`, the `notTheOpenTurn` case),
  `agent-shim/claude/shim/test/engine/turn.test.ts` (asserts
  `failureKind(response)` is `"notTheOpenTurn"`), and
  `agent-shim/claude/shim/test/integration/turn.test.ts` ("a TurnId that is
  not the open turn is refused not_the_open_turn").

- **DetachForeground applied to a foreground subagent** (formerly SPEC.md
  #34, `TestVendorBackgroundedSubagent`, golden
  `ctrl-b-detach-of-foreground-subagent`): `DetachForeground` (shim.v1) has
  no caller-facing `agentrepl.v1` rpc or daemon-internal trigger on this
  branch (`daemon/internal/sessionwatcher/fakes_test.go`'s fake client
  panics on it — "sessionwatcher must not call DetachForeground" — and
  `endpoint_interrupt.proto` is STOP-only), and no fake-SDK scenario
  backgrounds a subagent the way `shell.ts`'s `VENDOR_BACKGROUNDED`
  backgrounds a Bash call. Covered by shim unit tests, now including a
  subagent-shaped activity id:
  `agent-shim/claude/shim/test/engine/turn.test.ts`
  (`describe("DetachForeground", ...)` — `unknownUnit`, `alreadyConcluded`,
  success, generic over `AgentActivityId`; `describe("DetachForeground on a
  live foreground unit", ...)` — `unsupported`, the two CONFIRMS cases, over
  a bash unit; and `describe("DetachForeground on a live foreground
  subagent", ...)` — a live foreground subagent addressed by its tool_use id
  is refused `unsupported` (the same declared contract gap as a bash call:
  the pinned SDK offers no verb to INITIATE a detachment) and is CONFIRMED
  once `backgroundTasks(unit)` reports the vendor already holds live
  background work for it, while a stale/unknown subagent tool_use id is
  refused `unknownUnit`) and `agent-shim/claude/shim/test/engine/session.test.ts`.
  Still no caller-facing `agentrepl.v1` rpc or daemon-internal trigger, and
  no fake-SDK scenario backgrounds a subagent the way `shell.ts`'s
  `VENDOR_BACKGROUNDED` backgrounds a Bash call, so the deleted e2e test's
  scenario remains an e2e-level GAP — this closes only the shim-unit
  portion of it.

## Closed gap: the sidecar's three LOST arms (2026-09-04)

`DetachedLost {file_vanished | went_silent | swept_up}`
(`conversation/v1/agent_activity.proto`), drawn by the daemon as
`FeedShellLost` (`frontend/v1/feed.proto`), used to have NO e2e coverage:
reaching any of the three arms needs the sidecar's own staleness ruling to
ELAPSE or its boot sweep to run, and its production windows (30s grace, 30m
shell silence) cannot be reached inside this suite's budget.

CLOSED by `detachedlost_e2e_test.go`, which buys short windows through
`NewWorldWithSidecarStaleness` (`world_test.go`, wrapping the sidecar's own
`--stale-grace` / `--stale-shell-silence` / `--stale-agent-silence` /
`--stale-workflow-silence` / `--unowned-spool-window` flags) and drives each
arm off `!bash-detach-live`, whose spool never carries an `EXIT=` terminator:

- `went_silent` — a 400ms shell-silence window over the scenario's own
  silence. Observed conclusion 404ms after the last append.
- `file_vanished` — a 400ms grace over a spool the test deletes from the
  world's own scratch spool root. Observed conclusion 436ms after the delete.
- `swept_up` — the leftover spool stamped pre-boot and the sidecar RESTARTED
  over it (`Sidecar.Restart`), since `Tracker.BootSweep` runs once per
  PROCESS and boot time is read from the kernel rather than from a flag.

CLOSED TOO, as of Landing 11: `FeedShellLost` and `FeedSubagentLost` carry
`oneof how {file_vanished | went_silent | swept_up}`, mirroring `DetachedLost`
one-to-one, and the daemon relays the arm by name. All three tests now pin the
arm AT THE FEED (`FeedShellSettled.outcome.lost.how`) as well as on the
sidecar's own `lost-terminal` `reason` key. The two remain distinct claims:
the reason is the conclusion the sidecar reached, the `how` is the arm the
daemon relayed onward, and only asserting both catches a relay that renames or
drops the arm.

REMAINING GAP, recorded rather than absorbed: the SUBAGENT lost arms
(`AgentSubagentFailure.cause.lost`, `FeedSubagentLost`) remain undriven by any
e2e test.
