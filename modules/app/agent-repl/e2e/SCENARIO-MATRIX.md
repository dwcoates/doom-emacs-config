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
- **Covered means e2e drives the scenario, OR both of its halves are pinned
  and its seam is wired** (the owner's ruling (a) of 2026-10-14,
  docs/investigations/2026-10-14-test-tier-audit-e2e-edj.md, "Rulings").
  - The e2e columns are Go (`e2e/*_e2e_test.go`, excluding `emacs_*`), webapp
    (`webapp/test/webapp-layer/*.layer.test.ts`) and Emacs (`e2e/emacs_*`),
    derived exactly as before.
  - The halves are goldens in `testdata/scenario-goldens/<scenario>/`:
    `frames.json` is the shim's half, what its fold produces for the scenario
    (`agent-shim/claude/shim/test/fake/goldens.test.ts`); `feed.json` is the
    daemon's half, what its root feed draws from those frames
    (`daemon/integration/scenariogoldens_test.go`). Each is a byte-exact
    comparison, so a scenario covered by its halves reads `covered`.
  - The `Seam` column is the golden's `meta.json` seam: the family of
    scenarios whose halves meet over one cross-system path. Each family has
    exactly the e2e tests that say `Seam: <family>` in their doc comment, and
    the check fails a family with none, and a `Seam:` line naming a family no
    golden is in.
  - Section (d) below and the reconciliation sections at the end are
    hand-written context and are not derived; section (e), dead triggers, IS
    enforced.

## (a) Full matrix

| Scenario | Grounded? | Go e2e | Webapp layer | Emacs e2e | Shim half | Daemon half | Seam | Strongest assertion | Verdict |
|---|---|---|---|---|---|---|---|---|---|
| `!api-400` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!api-401` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!api-403` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!api-404` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!api-413` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!api-429` | ungrounded | failurearms_e2e_test.go | — | — | ✓ | ✓ | turn-failure | Go: TestApiRequestFailedArms/RateLimited asserts the named arm `FeedTurnEndedErrored.rate_limited` (feed.proto:879-880, 429), the EXACT per-arm headline “rate limited by the vendor” (equality is also a specific negative — no mid-turn evidence clause), and the exact vendor message “Rate limited; retry after 30 seconds.”. TestApiRateLimitedCarriesTheVendorWait additionally pins `FeedTurnErrorRateLimited.retry_after_ms == 549`, the exact wait the vendor stated. | covered |
| `!api-500` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!api-529` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!api-billing` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!api-max-output` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!api-oauth-org` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!api-unmodeled` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!artifact-list` | declared-only | — | feed-families.layer.test.ts | — | ✓ | ✓ | stream-tool-card | Go: TestArtifactPublishAndList asserts only listTurn.GetValue()!= (turn minted); contract says list produces no row (weak). Web: feed-families.layer asserts a specific negative — artifact row count unchanged (strong). | covered |
| `!artifact-publish` | declared-only | — | feed-families.layer.test.ts | — | ✓ | ✓ | stream-tool-card | Web: feed-families.layer asserts .artifact-url element. | covered |
| `!ask-free` | grounded | — | cards.layer.test.ts | — | ✓ | ✓ | answer-path | Go: TestQuestionFreeText asserts exact free-text echo in OtherText, single_select arm. Web: cards.layer answers with a typed send (SubmitPromptAskAnswered), asserts the open batch removed and the typed text drawn. | covered |
| `!ask-multi` | grounded | — | cards.layer.test.ts | — | ✓ | ✓ | answer-path | Go: TestQuestionMultiSelect/TestQuestionMultipleInOneBatch assert multi_select arm, exact 2-item Chosen slice, header/answer keyed by text. Web: cards.layer asserts [data-question-mode]. | covered |
| `!ask-single` | grounded | questions_e2e_test.go | cards.layer.test.ts, perf.layer.test.ts | — | ✓ | ✓ | answer-path | Go: TestQuestionSingleSelect asserts exact chosen-label round-trip, OtherText unset. Web: cards.layer asserts [data-question-option]/[data-ask-hint], and that one option click delivers the one-question batch (answered [data-state]). | covered |
| `!ask-unanswered` | grounded | — | cards.layer.test.ts | — | ✓ | ✓ | answer-path | Go: TestQuestionUnanswered asserts settled state is specifically FeedQuestionExpired (not Answered). Web: cards.layer asserts [data-state] present. | covered |
| `!away-summary` | ungrounded | — | — | — | ✓ | ✓ | two-plane-episode | Both halves are pinned by its goldens (frames.json, feed.json); the `two-plane-episode` seam is wired by TestPlanModeCoalescesOntoOneBubble, TestResidueAttachmentsDrawNoRow. | covered |
| `!bash` | grounded | detachedbash_e2e_test.go | feed-families.layer.test.ts | — | ✓ | ✓ | stream-tool-card | Go: TestBashForegroundCompleted asserts Succeeded verdict, exact stdout text, and absence of a detached_shell row. Web: feed-families.layer asserts [data-input-form]/[data-output-body]. | covered |
| `!bash-detach` | grounded | detachedbash_e2e_test.go | feed-families.layer.test.ts | — | ✓ | ✓ | shell-spool | Go: TestBashDetachedStartAndComplete asserts Exit.Code==0, live growth, spool text. Web: feed-families.layer asserts dataset.rowKind==detachedShell. | covered |
| `!bash-detach-fail` | ungrounded | — | — | — | ✓ | ✓ | shell-spool | Both halves are pinned by its goldens (frames.json, feed.json); the `shell-spool` seam is wired by TestBashDetachedStartAndComplete. | covered |
| `!bash-detach-live` | ungrounded | detachedlost_e2e_test.go | — | — | ✓ | ✓ | shell-spool | Go: TestBashDetachedLiveNeverSettles asserts the live, never-settling shape. detachedlost_e2e_test.go then drives all three `DetachedLost` arms off this same scenario — TestDetachedLostWentSilent, TestDetachedLostFileVanished, TestDetachedLostSweptUp — each asserting `FeedShellSettled.outcome.lost` plus the sidecar's own `reason` key naming the arm. | covered |
| `!bash-detach-poll` | ungrounded | detachedbash_e2e_test.go | — | — | — | — | — | Go: TestBashDetachExplicitPoll asserts settled exit 0, spool text, and a negative check that no TopbarWarning.UnmodeledTool names TaskOutput. | covered |
| `!bash-fail` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!bash-hold` | ungrounded | deploy_e2e_test.go | — | — | — | — | — | Go: TestBashHoldStaysForegroundUntilInterrupted asserts the live FOREGROUND Bash unit, the negative that no detached_shell row is ever drawn for it, and that a real daemon Interrupt is the only terminal it reaches (turn_ended.interrupted). GAP recorded at the test: DetachForeground's `unsupported` refusal is a shim.v1 verb with no caller-facing rpc, so that arm stays out of reach from this layer. | covered |
| `!bash-image` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!bash-rereported` | TODO | — | — | — | — | — | — | No counted e2e layer drives this scenario. | uncovered |
| `!bash-spill` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!bash-timeout` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!cancel-all` | grounded | mergequeue_e2e_test.go | — | — | ✓ | ✓ | fan-cancel | Go: TestFanWideCancel asserts InterruptedDetached.Count==3 and a second call returns NothingRunning. | covered |
| `!chess-board` | ungrounded | remainder_e2e_test.go | — | — | — | — | — | Go: TestChessBoard asserts the board row's exact heading and its unavailable arm's reason (no explanation-engine checkout in a test daemon). | covered |
| `!cold-seed` | ungrounded | coldgate_e2e_test.go | — | — | ✓ | ✓ | cold-gate | Go: TestColdGate subtests assert exact ContextTokens value, non-empty model name/compact menu, and resolved arm matches the chosen button. | covered |
| `!compact` | grounded | compaction_e2e_test.go | feed-families.layer.test.ts | — | ✓ | ✓ | compaction | Go: TestCompactionDirected(+summary override) asserts exact FeedContextCutCompacted.Summary text, non-nil separation tokens. Web: feed-families.layer asserts .sep-compacted. | covered |
| `!compact-auto` | ungrounded | — | — | — | ✓ | ✓ | compaction | Both halves are pinned by its goldens (frames.json, feed.json); the `compaction` seam is wired by TestCompactionDirected. | covered |
| `!compact-failed` | ungrounded | — | — | — | ✓ | ✓ | compaction | Both halves are pinned by its goldens (frames.json, feed.json); the `compaction` seam is wired by TestCompactionDirected. | covered |
| `!context-tip` | ungrounded | producerfaults_e2e_test.go | — | — | — | — | — | Go: TestContextTipDrawsNoRow asserts the same only-prose-rows negative AND a second one — no footer salient line stands after the tip turn, so a GENERIC CLI tip never draws as “your context is filling”. | covered |
| `!context-usage-drift` | grounded | accounting_e2e_test.go | — | — | ✓ | ✓ | session-footer | Go: TestContextUsage asserts ContextPanelView.Header non-empty and differs across two reads, Categories non-empty. | covered |
| `!context-window` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!cron` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!cron-create` | grounded | remainder_e2e_test.go | — | — | — | — | — | Go: TestCronCreateSetsTheFootersCronsChip asserts turn Concluded and footer LiveWork.Crons chip Count exactly 1 once the turn has ended. | covered |
| `!edit` | grounded | — | feed-families.layer.test.ts | — | ✓ | ✓ | stream-tool-card | Go: TestEdit asserts a diff output form. Web: feed-families.layer asserts [data-diff-line] count>0. | covered |
| `!fail-aborted-tools` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!fail-blocking-limit` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!fail-budget` | grounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!fail-continuation-prevented` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!fail-execution` | declared-only | — | refusals.layer.test.ts | — | ✓ | ✓ | turn-failure | Go: TestTurnStopErrorDuringExecution asserts errored.GetExecutionError()!=nil. Web: refusals.layer asserts zero refusal rows and zero failureArms (specific negative). | covered |
| `!fail-hook-stopped` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!fail-image` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!fail-malformed-tool-use` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `e2e-fail-this-turn` (marker) | ungrounded | mergequeue_e2e_test.go | — | — | — | — | — | Go: TestFailMarkerFailsABeforeActionRunAndRidesAnAfterActionTerminal configures the marker as a merge ACTION (the one prompt the daemon submits verbatim) and asserts the pair the marker exists for: the same failing turn settles the pre_prompt tab failed with the daemon's composed summary and ends the run at FeedMergeError.failed with no later tab ever opened, while as an after-action it settles the post_prompt tab failed and the run still reaches FeedMergeSuccess. | covered |
| `!fail-max-turns` | grounded | sharedworld_e2e_test.go | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!fail-model` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!fail-prompt-too-long` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!fail-rapid-refill` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!fail-stop-hook` | declared-only | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!fail-structured-output` | declared-only | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!fail-tool-deferred` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!fail-tool-deferred-unavailable` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!fail-turn-setup` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!fast-cooldown` | ungrounded | — | — | — | ✓ | ✓ | session-footer | Both halves are pinned by its goldens (frames.json, feed.json); the `session-footer` seam is wired by TestAccountUsage. | covered |
| `!fast-off` | ungrounded | — | — | — | ✓ | ✓ | session-footer | Both halves are pinned by its goldens (frames.json, feed.json); the `session-footer` seam is wired by TestAccountUsage. | covered |
| `!fast-on` | grounded | — | — | — | ✓ | ✓ | session-footer | Both halves are pinned by its goldens (frames.json, feed.json); the `session-footer` seam is wired by TestAccountUsage. | covered |
| `!fault-converter` | ungrounded | producerfaults_e2e_test.go | — | — | ✓ | ✓ | shim-diagnostics | Go: TestConverterDefectOpensADegradedWindow asserts three things — the turn still CONCLUDES, the malformed hook_started drew NO hook card (it reached no arm), and the diagnostics opened a TopbarDegradedWindowWarningDetail with a stated component, a stated reason and a positive began_at_ms. | covered |
| `!fault-recover` | ungrounded | producerfaults_e2e_test.go | — | — | ✓ | ✓ | shim-diagnostics | Go: TestConverterRecoveryClosesTheDegradedWindow asserts THAT window closed — matched by component plus began_at_ms, not merely "some closed window exists" — with ended_at_ms >= began_at_ms and a dropped_count >= 1 for the one message the converter refused. | covered |
| `!findings` | grounded | — | feed-families.layer.test.ts | — | ✓ | ✓ | stream-tool-card | Web: feed-families.layer asserts [data-finding] count>0. | covered |
| `!glob` | declared-only | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!grep-content` | declared-only | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!grep-count` | declared-only | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!grep-files` | declared-only | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!hold` | grounded | — | perf.layer.test.ts, roster.layer.test.ts, surfaces.layer.test.ts | emacs_workspace_e2e_test.go | ✓ | ✓ | interrupt-live-tool | Web: roster.layer asserts [data-held-turn] elements and unchanged userPrompt count; surfaces.layer only checks status differs (weak component). | covered |
| `!hook-blocked` | grounded | hooks_e2e_test.go | feed-families.layer.test.ts | — | ✓ | ✓ | stream-tool-card | Go: TestHookBlocked asserts exact FeedHookBlocked.Reason string. Web: feed-families.layer only checks dataset.unit==hook (tautological, weak). | covered |
| `!hook-cancelled` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!hook-failed` | grounded | hooks_e2e_test.go | feed-families.layer.test.ts | — | ✓ | ✓ | stream-tool-card | Go: TestHookFailed asserts exact ExitCode==1 and exact Output.Text. Web: feed-families.layer only checks dataset.unit==hook (weak). | covered |
| `!hook-success` | grounded | — | feed-families.layer.test.ts | — | ✓ | ✓ | stream-tool-card | Web: feed-families.layer asserts specific negative (no hook row). | covered |
| `!ide-diagnostics` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!ide-diagnostics-write` | grounded | filetools_e2e_test.go | — | — | — | — | — | Go: TestIdeDiagnosticsAfterWrite asserts the diagnostics hang off the tool card NAMED "Write", with composed lines — the write arm of the adjacency join, which a defect once folded onto the edit arm. | covered |
| `!interrupt` | grounded | interrupt_e2e_test.go | — | emacs_interrupt_e2e_test.go | ✓ | ✓ | interrupt-live-tool | Go: TestInterruptAfterTextDelta asserts InterruptedTurn!=nil, terminal specifically Interrupted. Emacs: TestEmacsForcedRestartInterruptsTheTurn asserts roster arm settles to :interrupted specifically. | covered |
| `!keepalive` | ungrounded | — | — | — | ✓ | ✓ | keepalive-marker | Both halves are pinned by its goldens (frames.json, feed.json); the `keepalive-marker` seam is wired by TestKeepAliveStoresNothingOnEitherPlane. | covered |
| `!max-tokens` | grounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!mcp-all` | grounded | — | — | — | ✓ | ✓ | session-footer | Both halves are pinned by its goldens (frames.json, feed.json); the `session-footer` seam is wired by TestAccountUsage. | covered |
| `!mcp-healthy` | ungrounded | — | — | — | ✓ | ✓ | session-footer | Both halves are pinned by its goldens (frames.json, feed.json); the `session-footer` seam is wired by TestAccountUsage. | covered |
| `!mcp-tool` | grounded | mcpmonitors_e2e_test.go | feed-families.layer.test.ts | — | — | — | — | Go: TestMcpToolCall asserts the turn's settled FeedSimpleToolCall headed mcp__echo__echo carries the tool's text, and no TopbarWarning.UnmodeledTool names it. Web: feed-families.layer asserts the generic shell's .tool-name and output body. | covered |
| `!md` | ungrounded | — | feed-families.layer.test.ts | — | ✓ | ✓ | prompt-round-trip | Web: feed-families.layer asserts literal !md text in the user_prompt row and exact 'Markdown showcase' body text. | covered |
| `!memory` | declared-only | — | — | — | ✓ | ✓ | two-plane-episode | Both halves are pinned by its goldens (frames.json, feed.json); the `two-plane-episode` seam is wired by TestPlanModeCoalescesOntoOneBubble, TestResidueAttachmentsDrawNoRow. | covered |
| `!model-fallback` | grounded | — | — | — | ✓ | ✓ | session-footer | Both halves are pinned by its goldens (frames.json, feed.json); the `session-footer` seam is wired by TestAccountUsage. | covered |
| `!monitor-deadline` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!monitor-persistent` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `<!--agent-repl:network-resume-->` (marker) | ungrounded | — | — | — | — | — | — | No counted e2e layer drives this scenario; the shim's own network-resume prompt selects it, and the shim integration suite drives it end to end (test/integration/detached.test.ts). | uncovered |
| `!perm-allow-once` | grounded | — | — | — | ✓ | ✓ | answer-path | Both halves are pinned by its goldens (frames.json, feed.json); the `answer-path` seam is wired by TestPermissionAskAnsweredArms. | covered |
| `!perm-allow-standing` | grounded | permission_e2e_test.go | — | — | ✓ | ✓ | answer-path | Go: TestPermissionAskAnsweredArms/AllowStanding asserts AllowedStanding()!=nil. | covered |
| `!perm-allow-standing-mode` | grounded | — | cards.layer.test.ts | — | — | — | — | Go: TestPermissionModeChangedMidSession asserts exact topbar PermissionModePicker.Current.Mode==accept_edits. Web: cards.layer asserts [data-permission=allowStanding] selector. | covered |
| `!perm-deny-policy` | grounded | — | cards.layer.test.ts | — | ✓ | ✓ | answer-path | Go: TestPermissionDeniedByPolicy asserts DeniedByPolicy()!=nil, sawOpen false. Web: cards.layer asserts a new tool-call row and zero refusalArms. | covered |
| `!perm-deny-user` | grounded | — | — | — | ✓ | ✓ | answer-path | Both halves are pinned by its goldens (frames.json, feed.json); the `answer-path` seam is wired by TestPermissionAskAnsweredArms. | covered |
| `!perm-hold` | grounded | daemonstop_e2e_test.go | cards.layer.test.ts | emacs_roster_e2e_test.go | — | — | — | Go: TestPermissionUndecidableParked asserts ask stays Open, no push while parked, then Interrupted. Web: cards.layer asserts [data-permission] buttons/.perm-waiting. Emacs: TestEmacsPermissionAskFiresTheAttentionMarker pins exact blink-schedule sequence. | covered |
| `!perm-no-standing` | ungrounded | — | — | — | — | — | — | No counted e2e layer drives this scenario. | uncovered |
| `!perm-undecidable` | ungrounded | — | cards.layer.test.ts | — | ✓ | ✓ | answer-path | Both halves pinned, seam answer-path wired. | covered |
| `!plan` | grounded | remainder_e2e_test.go | feed-families.layer.test.ts | — | ✓ | ✓ | two-plane-episode | Go: TestPlanModeEnterExit asserts FeedPlan.Planned non-nil with non-empty Prose/Edit.Path. Web: feed-families.layer asserts .plan-prose,.plan-planned. | covered |
| `!push-config-off` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!push-no-transport` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!push-sent` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!push-user-present` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!query-eof` | ungrounded | producerfaults_e2e_test.go | feed-families.layer.test.ts | — | ✓ | ✓ | query-death | Go: TestQueryEofEndsTheTurnAsQueryDied asserts the terminal is specifically FeedTurnEndedErrored.query_died with cause unexpected_eof (never concluded, never a generic vendor failure) plus a composed headline; TestQueryDiedRaisesTheFootersTurnDiedFault asserts the footer's agent_repl_fault · turn_died (a dead query is agent-repl's fault, never a block) and its FooterStatusActivityTurnEnded line. | covered |
| `!query-eof-mid-ask` | ungrounded | producerfaults_e2e_test.go | — | — | — | — | — | Go: TestQueryEofMidAskDeniesTheOpenAsk asserts the ask genuinely OPENED, then that the death settles it — FeedPermissionAnswered.denied_by_user, the gate's stand-down — and that the turn still ends on query_died. The fate of the in-flight ask, not only the notice. | covered |
| `!query-fail` | ungrounded | — | query-death.layer.test.ts | — | ✓ | ✓ | query-death | Go: TestQueryFailEndsTheTurnAsQueryDied asserts FeedTurnEndedErrored.query_died with cause iterator_failure specifically, separating the rejecting iterable from the EOF half. | covered |
| `!queue-vendor-turn` | ungrounded | hibernation_e2e_test.go | — | — | — | — | — | Go: TestCompletedTaskBesideKeepAliveAnchorsTheRewind asserts the next rewind anchors on the completed task's adopted turn and discards exactly [keepalive], no rewind is refused, and the vendor's own answer is drawn; TestKeepAliveAnswerAfterVendorTurnNeverServed asserts no keep-alive row is served beside it. | covered |
| `!rate-limit` | ungrounded | — | — | — | ✓ | ✓ | session-footer | Both halves are pinned by its goldens (frames.json, feed.json); the `session-footer` seam is wired by TestAccountUsage. | covered |
| `!rate-limit-five-hour` | ungrounded | — | — | — | — | — | — | No counted e2e layer drives this scenario. | uncovered |
| `!rate-limit-seven-day` | ungrounded | sessionfacts_e2e_test.go | — | — | — | — | — | Go: TestRateLimitSevenDayWindowFeedsTheWeeklyAllowance asserts the weekly `FooterAllowance` of the enduring usage line carries the event's `allowed_warning` verdict, its 0.91 utilization and a non-zero `resets_at_s`, the specific negative that no salient line stands, and the event's session-arm record. | covered |
| `!read` | grounded | footeractivity_e2e_test.go | feed-families.layer.test.ts | — | ✓ | ✓ | stream-tool-card | Go: TestReadWholeHeadRange asserts code output form, paint spans, omitted-line composed for the cut. Web: feed-families.layer asserts .tool-name/[data-output-body]. | covered |
| `!read-head` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!read-image` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!read-range` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!read-truncated` | ungrounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!refusal-fallback` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!refusal-no-fallback` | ungrounded | — | — | — | ✓ | ✓ | turn-failure | Both halves are pinned by its goldens (frames.json, feed.json); the `turn-failure` seam is wired by TestApiRateLimitedCarriesTheVendorWait. | covered |
| `!residue` | ungrounded | producerfaults_e2e_test.go | — | — | ✓ | ✓ | two-plane-episode | Go: TestResidueAttachmentsDrawNoRow asserts the specific negative the arms line demands — the turn's row set is exactly prompt + response + terminal, so neither `deferred_tools_delta` nor `agent_listing_delta` reached any conversation.v1 arm. | covered |
| `!rotate` | grounded | identityrotation_e2e_test.go | feed-families.layer.test.ts | — | ✓ | ✓ | clear-rotation | Go: TestClearRotatesIdentity/TestSecondRotateUnderRotatedIdentity assert exact new session-line value, distinct Cleared rows. Web: feed-families.layer only checks dataset.rowKind==separation (tautological, weak). | covered |
| `!send-message` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!send-message-refused` | ungrounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!send-message-resumed` | ungrounded | — | feed-families.layer.test.ts | — | ✓ | ✓ | stream-tool-card | Go: TestSendMessageQueuedAndResumed waits on FeedAgentPrompt.resumed_recipient (the arm that says a DORMANT agent was restarted), asserts the address is the composed `→ <recipient>` form naming the minted recipient — never the `an agent the send did not name` stand-in — and the body is the summary `resume the sweep`. | covered |
| `!skill` | grounded | skills_e2e_test.go | feed-families.layer.test.ts | — | ✓ | ✓ | skill | Go: TestSkillInvocation/TestSkillNamedAndArgsParameterized assert exact byte-for-byte Document.Markdown match. Web: feed-families.layer only checks dataset.unit==skill (weak). | covered |
| `!skill-fail` | ungrounded | — | feed-families.layer.test.ts | — | ✓ | ✓ | skill | Web: feed-families.layer only checks [data-state] is present and non-empty, which the running and loaded arms satisfy too. | covered |
| `!skills-injected` | declared-only | — | — | — | ✓ | ✓ | two-plane-episode | Both halves are pinned by its goldens (frames.json, feed.json); the `two-plane-episode` seam is wired by TestPlanModeCoalescesOntoOneBubble, TestResidueAttachmentsDrawNoRow. | covered |
| `!slash` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!slash-shape-a` | ungrounded | — | — | — | — | — | — | No counted e2e layer drives this scenario. | uncovered |
| `!slash-shape-a-unnamed` | ungrounded | slashcommands_e2e_test.go | — | — | — | — | — | Go: TestSlashShapeAUnnamed asserts exact transcript content string, absence of command-name, exact conclusion text. | covered |
| `!stop-on-rewind` | ungrounded | keepalive_e2e_test.go | — | — | — | — | — | Go: TestStopReplayedByEveryRewindNeverLoops asserts, over three replays of the real CLI's transcript-only stop, that every rewind anchors on the arming real turn and discards exactly [keepalive], no rewind is refused, the feed draws nothing of the stop and holds exactly the two real turns, and the store holds no row carrying it. | covered |
| `!subagent` | grounded | refusals_e2e_test.go, subagents_e2e_test.go | feed-families.layer.test.ts, subfeeds.layer.test.ts | — | ✓ | ✓ | subagent-frames | Go: TestSubagentSyncNestedActivity/TestBubbleRefusedNotDeliverable assert exact Label/Description text, sub-feed confinement. Web: subfeeds.layer asserts exact SYNC_COMMISSION string, feedContainer structure. | covered |
| `!subagent-detached` | grounded | subagents_e2e_test.go | feed-families.layer.test.ts, subfeeds.layer.test.ts | — | ✓ | ✓ | subagent-file | Go: TestSubagentDetached asserts root row is the detached wrapper, exact completion text confined to sub-feed. Web: subfeeds.layer asserts exact DETACHED_COMMISSION string. | covered |
| `!subagent-detached-hold` | ungrounded | subagents_e2e_test.go | — | — | — | — | — | Go: TestSubagentDetachedSurvivesAnInterjection interjects the held turn with an explicit `stop` and asserts turn_ended.interrupted, then proves the agent is still LIVE by an answer (Interrupt(detached) on its bubble succeeds with interrupted_detached, which a stopped agent refuses) and that the bubble then settles cancelled. | covered |
| `!subagent-detached-live` | ungrounded | refusals_e2e_test.go | — | — | — | — | — | Go: TestUnknownAgentOnUpdateAgent asserts exact Connect code + message substring. | covered |
| `!subagent-detached-utterance` | grounded-in-shape | refusals_e2e_test.go, subagents_e2e_test.go | — | — | — | — | — | Go: TestSubagentDetachedUtteranceStaysOffTopLevel asserts bubble stays Live, exact utterance text confined to sub-feed (positive+negative on exact string); TestBubbleRefusedAgentBusy asserts AgentBusy()!=nil. | covered |
| `!subagent-failed` | ungrounded | — | — | — | ✓ | ✓ | subagent-file | Both halves are pinned by its goldens (frames.json, feed.json); the `subagent-file` seam is wired by TestSubagentDetached. | covered |
| `!subagent-interleaved` | grounded-in-shape | subagents_e2e_test.go | — | — | — | — | — | Go: TestSubagentInterleavedResponsesStayOnTheirOwnFeeds asserts the main turn draws exactly ONE thinking unit and ONE final_answer response, each with its whole exact text, the subagent's two exact responses on its sub-feed and absent from the root feed, and the bubble settled succeeded. | covered |
| `!subagent-network-failed` | grounded | — | — | — | — | — | — | No counted e2e layer drives this scenario; the shim integration suite drives it (test/integration/detached.test.ts). Grounded in the 2026-09-27 outage's production records, not a capture. | uncovered |
| `!subagent-resumed` | ungrounded | — | — | — | — | — | — | No counted e2e layer drives this scenario; the shim integration suite drives it across a shim restart (test/integration/detached.test.ts). | uncovered |
| `!task-change` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!task-create` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!task-reject` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!tokens-reminder` | ungrounded | producerfaults_e2e_test.go | — | — | — | — | — | Go: TestTokensReminderDrawsNoRow asserts the only-prose-rows negative AND the negative that no footer salient line was minted from a TOKEN-COUNT reminder. | covered |
| `!unmodeled` | grounded | — | surfaces.layer.test.ts | — | ✓ | ✓ | stream-tool-card | Go: TestUnmodeledTool asserts exactly 1 TopbarWarning.UnmodeledTool for StructuredOutput, feed rows never mention it. Web: surfaces.layer asserts .topbar-warnings present and unchanged tool-call row count. | covered |
| `!usage-available` | grounded | — | — | — | ✓ | ✓ | session-footer | Both halves are pinned by its goldens (frames.json, feed.json); the `session-footer` seam is wired by TestAccountUsage. | covered |
| `!usage-full` | grounded | accounting_e2e_test.go | surfaces.layer.test.ts | — | ✓ | ✓ | session-footer | Go: TestAccountUsage (same log-record assertion as usage-available). Web: surfaces.layer asserts .footer-tokens/.footer-clock elements and a footer panel with named data-panel. | covered |
| `!usage-historical` | ungrounded | subagents_e2e_test.go | — | — | — | — | — | Go: TestNestedSubagentHistoricalUsage asserts specifically that NO subagent bubble row is fabricated on the root feed (a shape-absence check) alongside Concluded. | covered |
| `!usage-opus-absent` | ungrounded | sessionfacts_e2e_test.go | — | — | ✓ | ✓ | session-footer | Go: TestAccountUsageOpusAbsentIsReadAndRetiresTheLine stands a rate line on the weekly EVENT over an unread probe, drives this scenario, and asserts the drawn consequence of an available outcome that is NOT an unavailability: its reprobe READS and re-files the sub-threshold weekly figure, so `FooterStatusActivityRateLimited` goes away entirely (a specific negative an unread arm would not produce). Exact conclusion prose pinned too. | covered |
| `!usage-sampling-failure` | ungrounded | — | — | — | ✓ | ✓ | session-footer | Both halves are pinned by its goldens (frames.json, feed.json); the `session-footer` seam is wired by TestAccountUsage. | covered |
| `!usage-seat-spend` | ungrounded | — | — | — | — | — | — | No counted e2e layer drives this scenario. | uncovered |
| `!usage-seat-spend-unreported` | ungrounded | — | — | — | — | — | — | No counted e2e layer drives this scenario. | uncovered |
| `!usage-service-unavailable` | ungrounded | sessionfacts_e2e_test.go | — | — | ✓ | ✓ | session-footer | Go: TestAccountUsageUnreadArmsLeaveTheReadFiguresStanding/service_unavailable asserts the daemon's `usage_sample_unreadable` breadcrumb names this reason, and that the line the weekly event then opens still draws the session 0.41 a prior READABLE sample filed, with that reading's `figures_read_at_ms` and no unread caveat (owner ruling fc4917be4). Exact conclusion prose pinned. | covered |
| `!usage-utilization-unavailable` | ungrounded | — | — | — | ✓ | ✓ | session-footer | Both halves are pinned by its goldens (frames.json, feed.json); the `session-footer` seam is wired by TestAccountUsage. | covered |
| `!usage-window-unavailable` | ungrounded | — | — | — | ✓ | ✓ | session-footer | Both halves are pinned by its goldens (frames.json, feed.json); the `session-footer` seam is wired by TestAccountUsage. | covered |
| `!vendor-backgrounded` | grounded | — | — | — | ✓ | ✓ | shell-spool | Both halves are pinned by its goldens (frames.json, feed.json); the `shell-spool` seam is wired by TestBashDetachedStartAndComplete. | covered |
| `!wakeup-schedule` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!wakeup-stop` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!web-fetch` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!web-fetch-redirect` | ungrounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!web-search` | grounded | — | feed-families.layer.test.ts | — | ✓ | ✓ | stream-tool-card | Both halves pinned, seam stream-tool-card wired. | covered |
| `!worktree-keep` | declared-only | — | feed-families.layer.test.ts | — | ✓ | ✓ | stream-tool-card | Web: feed-families.layer asserts count grew by >=2 and .sep-worktree class. | covered |
| `!worktree-remove` | declared-only | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!write-create` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |
| `!write-update` | grounded | — | — | — | ✓ | ✓ | stream-tool-card | Both halves are pinned by its goldens (frames.json, feed.json); the `stream-tool-card` seam is wired by TestBashForegroundCompleted. | covered |

## (b) Counts

DERIVED FROM TABLE (a) ABOVE, by counting its verdict and layer columns.
`TestScenarioMatrixMatchesReality` recomputes every figure here and fails on
a disagreement, so these are not hand tallies (they were, and they were
wrong: the by-layer lines once read 33 and 5 where the table's columns held
32 and 3).

- Shim half pinned: 132 scenarios
- Daemon half pinned: 132 scenarios
- Seam families: 18, each wired by its e2e test
- Covered (at least one STRONG, specific-shape assertion in a counted layer, or both halves pinned with a wired seam): **153**
- Weak (a counted layer drives the scenario but only asserts turn-completion or a non-specific field, never a named arm/shape): **0**
- Uncovered (no counted layer drives the scenario, and its halves are not both pinned with a wired seam): **9**
- Total canonical scenarios: 162

By layer, scenarios with at least one hit:
- Go e2e (non-emacs): 47 scenarios referenced across 27 files
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

- `!bash-rereported`
- `!perm-no-standing`
- `!rate-limit-five-hour`
- `!slash-shape-a`
- `!subagent-network-failed`
- `!subagent-resumed`
- `!usage-seat-spend-unreported`
- `!usage-seat-spend`
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

The sidecar's own table (then
`agent-shim/claude/shim-sidecar/integration/mock_scenarios_test.go`,
`mockScenarios`, 133 rows; now `shim-sidecar/mockgoldens_test.go`,
`goldenScenarios`, one row per distinct on-disk shape over the shim's
`testdata/scenario-goldens`) is drawn from the SAME registry and is explicitly
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
  that file is next touched). MOOT since 2026-10-06: the scenario, its sidecar
  row and the context-budget footer line are all retired (owner ruling).

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
