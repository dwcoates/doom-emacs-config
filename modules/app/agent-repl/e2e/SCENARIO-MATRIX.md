# Fake-SDK scenario coverage matrix — agent-repl

Read-only inventory. Branch `overhaul/integration`. Sources: the mocked
vendor's registry (`agent-shim/claude/shim/src/fake/registry.ts` and
`scenarios/*.ts`), its generated table
(`agent-shim/claude/shim/AGENTS.md` "Mocked vendor: prompt → scenario
table"), the capture manifest
(`agent-shim/claude/shim/testdata/captures/MANIFEST.md`), the sidecar's own
mock table (`agent-shim/claude/shim-sidecar/integration/mock_scenarios_test.go`),
and the three counted e2e layers: Go (`e2e/*_e2e_test.go`, excluding
`emacs_*`), webapp (`webapp/test/webapp-layer/*.layer.test.ts`, driven via
`e2e/webapplayer_e2e_test.go`), and Emacs (`e2e/emacs_*_e2e_test.go`).
Per the owner's ruling, daemon/integration, shim test/integration, and
sidecar integration tests do NOT count as e2e coverage — they are excluded
from every count below even where they exercise the same scenario.

148 scenario tokens are canonical (147 named `!` scenarios plus the
`e2e-fail-this-turn` marker; the unnamed default prose scenario is included
as `!(default prose)`).

## (a) Full matrix

| Scenario | Grounded? | Go e2e | Webapp layer | Emacs e2e | Strongest assertion | Verdict |
|---|---|---|---|---|---|---|
| `!api-400` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!api-401` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!api-403` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!api-404` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!api-413` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!api-429` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!api-500` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!api-529` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!api-billing` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!api-max-output` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!api-oauth-org` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!api-unmodeled` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!artifact-list` | declared-only | remainder_e2e_test.go | feed-families.layer.test.ts | — | Go: TestArtifactPublishAndList asserts only listTurn.GetValue()!= (turn minted); contract says list produces no row (weak). Web: feed-families.layer asserts a specific negative — artifact row count unchanged (strong). | covered |
| `!artifact-publish` | declared-only | remainder_e2e_test.go | feed-families.layer.test.ts | — | Go: TestArtifactPublishAndList asserts FeedArtifact.Published non-nil, non-empty Heading.Text/Url.Url. Web: feed-families.layer asserts .artifact-url element. | covered |
| `!ask-free` | grounded | questions_e2e_test.go | cards.layer.test.ts | — | Go: TestQuestionFreeText asserts exact free-text echo in OtherText, single_select arm. Web: cards.layer answers via [data-question-other], asserts submit control removed. | covered |
| `!ask-multi` | grounded | questions_e2e_test.go | cards.layer.test.ts | — | Go: TestQuestionMultiSelect/TestQuestionMultipleInOneBatch assert multi_select arm, exact 2-item Chosen slice, header/answer keyed by text. Web: cards.layer asserts [data-question-mode]. | covered |
| `!ask-single` | grounded | questions_e2e_test.go | cards.layer.test.ts | — | Go: TestQuestionSingleSelect asserts exact chosen-label round-trip, OtherText unset. Web: cards.layer asserts [data-question-option]/[data-question-submit] and answered [data-state]. | covered |
| `!ask-unanswered` | grounded | questions_e2e_test.go | cards.layer.test.ts | — | Go: TestQuestionUnanswered asserts settled state is specifically FeedQuestionExpired (not Answered). Web: cards.layer asserts [data-state] present. | covered |
| `!away-summary` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!bash` | grounded | detachedbash_e2e_test.go | feed-families.layer.test.ts | — | Go: TestBashForegroundCompleted asserts Succeeded verdict, exact stdout text, and absence of a detached_shell row. Web: feed-families.layer asserts [data-input-form]/[data-output-body]. | covered |
| `!bash-detach` | grounded | detachedbash_e2e_test.go | feed-families.layer.test.ts | — | Go: TestBashDetachedStartAndComplete asserts Exit.Code==0, live growth, spool text. Web: feed-families.layer asserts dataset.rowKind==detachedShell. | covered |
| `!bash-detach-fail` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!bash-detach-live` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!bash-detach-poll` | ungrounded | detachedbash_e2e_test.go | — | — | Go: TestBashDetachExplicitPoll asserts settled exit 0, spool text, and a negative check that no TopbarWarning.UnmodeledTool names TaskOutput. | covered |
| `!bash-fail` | grounded | detachedbash_e2e_test.go | — | — | Go: TestBashNonzeroExit asserts Succeeded verdict despite nonzero exit, exact stdout text. | covered |
| `!bash-hold` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!bash-image` | grounded | detachedbash_e2e_test.go | — | — | Go: TestBashImageOutput asserts Succeeded verdict and specifically the FeedToolCallNoOutput arm. | covered |
| `!bash-spill` | grounded | detachedbash_e2e_test.go | — | — | Go: TestBashPartialOutputWithSpill asserts Succeeded verdict, exact truncation-phrase text. | covered |
| `!bash-timeout` | grounded | interrupt_e2e_test.go | — | — | Go: TestBashInterruptedByTimeout asserts terminal is Concluded (not Interrupted), detached shell stays Live, non-empty spool text. | covered |
| `!cancel-all` | grounded | mergequeue_e2e_test.go | — | — | Go: TestFanWideCancel asserts InterruptedDetached.Count==3 and a second call returns NothingRunning. | covered |
| `!cold-seed` | ungrounded | coldgate_e2e_test.go | — | — | Go: TestColdGate subtests assert exact ContextTokens value, non-empty model name/compact menu, and resolved arm matches the chosen button. | covered |
| `!compact` | grounded | compaction_e2e_test.go, slashcommands_e2e_test.go | feed-families.layer.test.ts | — | Go: TestCompactionDirected(+summary override) asserts exact FeedContextCutCompacted.Summary text, non-nil separation tokens. Web: feed-families.layer asserts .sep-compacted. | covered |
| `!compact-auto` | ungrounded | compaction_e2e_test.go | — | — | Go: TestCompactionAuto asserts exact summary string + non-nil tokens. | covered |
| `!compact-failed` | ungrounded | compaction_e2e_test.go | — | — | Go: TestCompactionFailed asserts exact FeedContextCutCompactionFailed.Error, no Compacted row drawn. | covered |
| `!context-budget-warning` | ungrounded | compaction_e2e_test.go | — | — | Go: TestContextBudgetWarning asserts FooterStatusActivityContextBudget.Text non-empty (named field, not a fixed string). | weak |
| `!context-tip` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!context-usage-drift` | grounded | accounting_e2e_test.go | — | — | Go: TestContextUsage asserts ContextPanelView.Header non-empty and differs across two reads, Categories non-empty. | covered |
| `!context-window` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!cron` | grounded | remainder_e2e_test.go | — | — | Go: TestCronCreateListDelete asserts turn Concluded and footer LiveWork.Crons chip Count positive. | covered |
| `!edit` | grounded | filetools_e2e_test.go | feed-families.layer.test.ts | — | Go: TestEdit asserts a diff output form. Web: feed-families.layer asserts [data-diff-line] count>0. | covered |
| `!fail-aborted-tools` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!fail-blocking-limit` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!fail-budget` | grounded | turnlifecycle_e2e_test.go | — | — | Go: TestTurnStopMaxBudgetUsd asserts errored.GetMaxBudget()!=nil plus headline. | covered |
| `!fail-continuation-prevented` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!fail-execution` | declared-only | turnlifecycle_e2e_test.go | refusals.layer.test.ts | — | Go: TestTurnStopErrorDuringExecution asserts errored.GetExecutionError()!=nil. Web: refusals.layer asserts zero refusal rows and zero failureArms (specific negative). | covered |
| `!fail-hook-stopped` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!fail-image` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!fail-malformed-tool-use` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!fail-max-turns` | grounded | turnlifecycle_e2e_test.go | — | — | Go: TestTurnStopMaxTurns asserts errored.GetMaxTurns()!=nil plus headline. | covered |
| `!fail-model` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!fail-prompt-too-long` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!fail-rapid-refill` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!fail-stop-hook` | declared-only | turnlifecycle_e2e_test.go | — | — | Go: TestTurnStopHookStop asserts errored.GetStopHookPrevented()!=nil plus headline. | covered |
| `!fail-structured-output` | declared-only | turnlifecycle_e2e_test.go | — | — | Go: TestTurnStopMaxStructuredOutputRetries asserts exact StopReason==structured_output_retry_exhausted. | covered |
| `!fail-tool-deferred` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!fail-tool-deferred-unavailable` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!fail-turn-setup` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!fast-cooldown` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!fast-off` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!fast-on` | grounded | turnlifecycle_e2e_test.go | — | — | Go: TestFastMode asserts only Concluded (explicitly documented gap, no fast_mode_state field checked). | weak |
| `!fault-converter` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!fault-recover` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!findings` | grounded | remainder_e2e_test.go | feed-families.layer.test.ts | — | Go: TestReportFindings asserts 3 rows with exact Verdict/Outcome per row. Web: feed-families.layer asserts [data-finding] count>0. | covered |
| `!glob` | declared-only | filetools_e2e_test.go | — | — | Go: TestGlob asserts a lines output form and exact matched-path line count. | covered |
| `!grep-content` | declared-only | filetools_e2e_test.go | — | — | Go: TestGrepContentFilesCount asserts lines form, min line count, omitted-floor composed. | covered |
| `!grep-count` | declared-only | filetools_e2e_test.go | — | — | Go: TestGrepContentFilesCount (count subtest) asserts text output form, non-empty composed count. | covered |
| `!grep-files` | declared-only | filetools_e2e_test.go | — | — | Go: TestGrepContentFilesCount (files subtest) asserts lines output, no omitted floor. | covered |
| `!hold` | grounded | permission_e2e_test.go | roster.layer.test.ts, surfaces.layer.test.ts | emacs_workspace_e2e_test.go | Go: TestHeldTurnGate asserts DaemonHoldTray carries the held prompt, tray empties on release. Web: roster.layer asserts [data-held-turn] elements and unchanged userPrompt count; surfaces.layer only checks status differs (weak component). | covered |
| `!hook-blocked` | grounded | hooks_e2e_test.go | feed-families.layer.test.ts | — | Go: TestHookBlocked asserts exact FeedHookBlocked.Reason string. Web: feed-families.layer only checks dataset.unit==hook (tautological, weak). | covered |
| `!hook-cancelled` | grounded | hooks_e2e_test.go | — | — | Go: TestHookCancelled asserts no hook card drawn plus exact log Level==warn. | covered |
| `!hook-failed` | grounded | hooks_e2e_test.go | feed-families.layer.test.ts | — | Go: TestHookFailed asserts exact ExitCode==1 and exact Output.Text. Web: feed-families.layer only checks dataset.unit==hook (weak). | covered |
| `!hook-success` | grounded | hooks_e2e_test.go | feed-families.layer.test.ts | — | Go: TestHookSucceeded asserts no hook card drawn plus exact log Context[hook]==PreToolUse:Read. Web: feed-families.layer asserts specific negative (no hook row). | covered |
| `!ide-diagnostics` | grounded | filetools_e2e_test.go | — | — | Go: TestIdeDiagnosticsAfterEdit asserts a diff form alongside Diagnostics with composed lines. | covered |
| `!interrupt` | grounded | interrupt_e2e_test.go | cards.layer.test.ts, roster.layer.test.ts | emacs_interrupt_e2e_test.go | Go: TestInterruptAfterTextDelta asserts InterruptedTurn!=nil, terminal specifically Interrupted. Emacs: TestEmacsForcedRestartInterruptsTheTurn asserts roster arm settles to :interrupted specifically. | covered |
| `!keepalive` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!max-tokens` | grounded | turnlifecycle_e2e_test.go | — | — | Go: TestMaxTokens asserts Concluded plus an exact truncated prose string. | covered |
| `!mcp-all` | grounded | mcpmonitors_e2e_test.go | — | — | Go: TestMcpServerHealths asserts exact per-server oneof-arm mapping and exact Failed.Detail.Text. | covered |
| `!mcp-healthy` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!md` | ungrounded | — | feed-families.layer.test.ts | — | Web: feed-families.layer asserts literal !md text in the user_prompt row and exact 'Markdown showcase' body text. | covered |
| `!memory` | declared-only | remainder_e2e_test.go | — | — | Go: TestContextInjectedMemory asserts AgentContextInjected.GetMemory() non-nil with non-empty Path/Content. | covered |
| `!model-fallback` | grounded | turnlifecycle_e2e_test.go | — | — | Go: TestModelChanged asserts only Concluded (explicitly documented gap). | weak |
| `!monitor-deadline` | grounded | mcpmonitors_e2e_test.go | — | — | Go: TestMonitorDeadline asserts exact Description, Persistent==false, chip count==1. | covered |
| `!monitor-persistent` | grounded | mcpmonitors_e2e_test.go | — | — | Go: TestMonitorPersistent asserts exact Description, Persistent==true. | covered |
| `!perm-allow-once` | grounded | permission_e2e_test.go | — | — | Go: TestPermissionAskAnsweredArms/AllowOnce asserts AllowedOnce()!=nil, tool succeeded, turn concluded. | covered |
| `!perm-allow-standing` | grounded | permission_e2e_test.go | — | — | Go: TestPermissionAskAnsweredArms/AllowStanding asserts AllowedStanding()!=nil. | covered |
| `!perm-allow-standing-mode` | grounded | permission_e2e_test.go | cards.layer.test.ts | — | Go: TestPermissionModeChangedMidSession asserts exact topbar PermissionModePicker.Current.Mode==accept_edits. Web: cards.layer asserts [data-permission=allowStanding] selector. | covered |
| `!perm-deny-policy` | grounded | permission_e2e_test.go | cards.layer.test.ts | — | Go: TestPermissionDeniedByPolicy asserts DeniedByPolicy()!=nil, sawOpen false. Web: cards.layer asserts a new tool-call row and zero refusalArms. | covered |
| `!perm-deny-user` | grounded | permission_e2e_test.go | — | — | Go: TestPermissionAskAnsweredArms/DeniedByUser asserts DeniedByUser()!=nil, tool Denied not Failed. | covered |
| `!perm-hold` | grounded | permission_e2e_test.go | cards.layer.test.ts | emacs_roster_e2e_test.go | Go: TestPermissionUndecidableParked asserts ask stays Open, no push while parked, then Interrupted. Web: cards.layer asserts [data-permission] buttons/.perm-waiting. Emacs: TestEmacsPermissionAskFiresTheAttentionMarker pins exact blink-schedule sequence. | covered |
| `!perm-no-standing` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!perm-undecidable` | ungrounded | permission_e2e_test.go | — | — | Go: TestPermissionDeniedByPolicy doc comment names it only as a negative example; no test actually submits !perm-undecidable. | uncovered |
| `!plan` | grounded | remainder_e2e_test.go | feed-families.layer.test.ts | — | Go: TestPlanModeEnterExit asserts FeedPlan.Planned non-nil with non-empty Prose/Edit.Path. Web: feed-families.layer asserts .plan-prose,.plan-planned. | covered |
| `!push-config-off` | grounded | remainder_e2e_test.go | — | — | Go: TestPushNotificationNotSent subtest asserts only Concluded (documented no-feed-row fact). | weak |
| `!push-no-transport` | grounded | remainder_e2e_test.go | — | — | Go: TestPushNotificationNotSent subtest asserts only Concluded. | weak |
| `!push-sent` | grounded | remainder_e2e_test.go | — | — | Go: TestPushNotificationSent asserts only Concluded (documented no-feed-row fact). | weak |
| `!push-user-present` | grounded | remainder_e2e_test.go | — | — | Go: TestPushNotificationNotSent subtest asserts only Concluded. | weak |
| `!query-eof` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!query-eof-mid-ask` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!query-fail` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!rate-limit` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!rate-limit-five-hour` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!rate-limit-seven-day` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!read` | grounded | filetools_e2e_test.go | feed-families.layer.test.ts | — | Go: TestReadWholeHeadRange asserts code output form, paint spans, omitted-line composed for the cut. Web: feed-families.layer asserts .tool-name/[data-output-body]. | covered |
| `!read-head` | grounded | filetools_e2e_test.go | — | — | Go: TestReadWholeHeadRange (head subtest) asserts omitted line composed for the cut. | covered |
| `!read-image` | grounded | — | — | — | no covering test in any layer | uncovered |
| `!read-range` | grounded | filetools_e2e_test.go | — | — | Go: TestReadWholeHeadRange (range subtest) asserts paint spans, no omitted line. | covered |
| `!read-truncated` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!refusal-fallback` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!refusal-no-fallback` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!residue` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!rotate` | grounded | identityrotation_e2e_test.go | feed-families.layer.test.ts | — | Go: TestClearRotatesIdentity/TestSecondRotateUnderRotatedIdentity assert exact new session-line value, distinct Cleared rows. Web: feed-families.layer only checks dataset.rowKind==separation (tautological, weak). | covered |
| `!send-message` | grounded | remainder_e2e_test.go | — | — | Go: TestSendMessageQueuedAndResumed asserts only Concluded. | weak |
| `!send-message-refused` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!send-message-resumed` | ungrounded | remainder_e2e_test.go | — | — | Go: TestSendMessageQueuedAndResumed asserts only Concluded. | weak |
| `!skill` | grounded | skills_e2e_test.go | feed-families.layer.test.ts | — | Go: TestSkillInvocation/TestSkillNamedAndArgsParameterized assert exact byte-for-byte Document.Markdown match. Web: feed-families.layer only checks dataset.unit==skill (weak). | covered |
| `!skill-fail` | ungrounded | — | feed-families.layer.test.ts | — | Web: feed-families.layer only checks [data-state] present and non-empty, no specific arm value. | weak |
| `!skills-injected` | declared-only | remainder_e2e_test.go | — | — | Go: TestContextInjectedSkills asserts injected.GetSkills()!=nil and non-empty Skills slice. | covered |
| `!slash` | grounded | slashcommands_e2e_test.go | — | — | Go: TestVendorAnsweredSlashCommand/TestSlashShapeBViaSlash assert exact answering prose string and exact transcript record shape. | covered |
| `!slash-shape-a` | ungrounded | slashcommands_e2e_test.go | — | — | Go: TestSlashShapeANamed asserts exact transcript content string and exact conclusion text. | covered |
| `!slash-shape-a-unnamed` | ungrounded | slashcommands_e2e_test.go | — | — | Go: TestSlashShapeAUnnamed asserts exact transcript content string, absence of command-name, exact conclusion text. | covered |
| `!subagent` | grounded | detachedbash_e2e_test.go, refusals_e2e_test.go, subagents_e2e_test.go | feed-families.layer.test.ts, subfeeds.layer.test.ts | — | Go: TestSubagentSyncNestedActivity/TestBubbleRefusedNotDeliverable assert exact Label/Description text, sub-feed confinement. Web: subfeeds.layer asserts exact SYNC_COMMISSION string, feedContainer structure. | covered |
| `!subagent-detached` | grounded | subagents_e2e_test.go | feed-families.layer.test.ts, subfeeds.layer.test.ts | — | Go: TestSubagentDetached asserts root row is the detached wrapper, exact completion text confined to sub-feed. Web: subfeeds.layer asserts exact DETACHED_COMMISSION string. | covered |
| `!subagent-detached-live` | ungrounded | refusals_e2e_test.go | — | — | Go: TestBubbleRefusedAgentBusy asserts AgentBusy()!=nil; TestUnknownAgentOnUpdateAgent asserts exact Connect code + message substring. | covered |
| `!subagent-detached-utterance` | grounded-in-shape | subagents_e2e_test.go | — | — | Go: TestSubagentDetachedUtteranceStaysOffTopLevel asserts bubble stays Live, exact utterance text confined to sub-feed (positive+negative on exact string). | covered |
| `!subagent-failed` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!task-change` | grounded | remainder_e2e_test.go | — | — | Go: TestTaskActsCreateChangeReject asserts a checklist row with Status.GetRunning()!=nil. | covered |
| `!task-create` | grounded | remainder_e2e_test.go | — | — | Go: TestTaskActsCreateChangeReject asserts turn concluded and footer LiveWork.Tasks.Total==2. | covered |
| `!task-reject` | grounded | remainder_e2e_test.go | — | — | Go: TestTaskActsCreateChangeReject asserts checklist rows persist and LiveWork.Tasks chip still non-nil after rejection. | covered |
| `!tokens-reminder` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!unmodeled` | grounded | mcpmonitors_e2e_test.go | surfaces.layer.test.ts | — | Go: TestMcpUnmodeledTool asserts exactly 1 TopbarWarning.UnmodeledTool for the tool name, feed rows never mention it. Web: surfaces.layer asserts .topbar-warnings present and unchanged tool-call row count. | covered |
| `!usage-available` | grounded | accounting_e2e_test.go | — | — | Go: TestAccountUsage asserts a specific daemon log record (Operation/Context[arm]) rather than a frontend/v1 shape. | weak |
| `!usage-full` | grounded | accounting_e2e_test.go | surfaces.layer.test.ts | — | Go: TestAccountUsage (same log-record assertion as usage-available). Web: surfaces.layer asserts .footer-tokens/.footer-clock elements and a footer panel with named data-panel. | covered |
| `!usage-historical` | ungrounded | accounting_e2e_test.go, subagents_e2e_test.go | — | — | Go: TestNestedSubagentHistoricalUsage asserts specifically that NO subagent bubble row is fabricated on the root feed (a shape-absence check) alongside Concluded. | covered |
| `!usage-opus-absent` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!usage-sampling-failure` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!usage-service-unavailable` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!usage-utilization-unavailable` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!usage-window-unavailable` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!vendor-backgrounded` | grounded | — | — | — | no e2e-layer test; the Go e2e test for the golden this scenario stands in for was removed (owner ruling, PROTO-CHANGES.md Landing 8, 2026-09-04). Its DetachForeground-confirm path is covered only by shim test/integration/detached.test.ts (excluded from this count per the owner's ruling above). | uncovered |
| `!wakeup-schedule` | grounded | remainder_e2e_test.go | — | — | Go: TestScheduleWakeupScheduleAndStop asserts only Concluded. | weak |
| `!wakeup-stop` | grounded | remainder_e2e_test.go | — | — | Go: TestScheduleWakeupScheduleAndStop asserts only Concluded. | weak |
| `!web-fetch` | grounded | remainder_e2e_test.go | feed-families.layer.test.ts | — | Go: TestWebFetch asserts tool call Succeeded, non-empty Text. Web: feed-families.layer only checks .tool-head exists and non-empty text (weak; no link-specific selector despite the file's own comment claiming one). | covered |
| `!web-fetch-redirect` | ungrounded | — | — | — | no covering test in any layer | uncovered |
| `!web-search` | grounded | remainder_e2e_test.go | — | — | Go: TestWebSearch asserts tool call Succeeded, non-empty Links. | covered |
| `!worktree-keep` | declared-only | remainder_e2e_test.go | feed-families.layer.test.ts | — | Go: TestWorktreeEnterExitKeptAndRemoved asserts exact WorktreeEntered/WorktreeLeft.Kept Path.Text. Web: feed-families.layer asserts count grew by >=2 and .sep-worktree class. | covered |
| `!worktree-remove` | declared-only | remainder_e2e_test.go | — | — | Go: TestWorktreeEnterExitKeptAndRemoved asserts Separation.WorktreeLeft.Removed.Discarded!=nil. | covered |
| `!write-create` | grounded | filetools_e2e_test.go | — | — | Go: TestWriteCreatedAndUpdated asserts a diff output form with the write's hunk lines. | covered |
| `!write-update` | grounded | filetools_e2e_test.go | — | — | Go: TestWriteCreatedAndUpdated (update subtest) asserts diff form with hunk lines. | covered |

## (b) Counts

- Covered (at least one STRONG, specific-shape assertion in a counted layer): **76**
- Weak (a counted layer drives the scenario but only asserts turn-completion or a non-specific field, never a named arm/shape): **14**
- Uncovered (no counted layer drives the scenario at all): **58**
- Total canonical scenarios: 148

By layer, scenarios with at least one hit:
- Go e2e (non-emacs): 90 scenarios referenced across 19 files
- Webapp layer (10 `.layer.test.ts` files): 33 scenarios referenced
- Emacs e2e (9 files): 5 scenarios referenced (`hold`, `interrupt`, `perm-hold`, `prose-streamed`-token, plus the plain gated-prompt tests that use no `!` scenario at all)

## (c) Uncovered list, grouped by priority area

### Permissions (2)
- `!perm-no-standing` — no test submits it in any layer.
- `!perm-undecidable` — named only in a doc comment (`permission_e2e_test.go`) explaining why `!perm-hold` is used instead; never actually submitted. Also KNOWN-OPEN per AGENTS.md: `sdk.d.ts` declares no discriminator separating "nobody could decide" from an ordinary policy deny, so the scenario itself is the closest producer available, not a clean grounding target.

### Interrupts (3)
- `!query-eof`, `!query-eof-mid-ask`, `!query-fail` — all three producer-side query-death paths (`SessionQueryDied`) are undriven by any counted e2e layer.

### Compaction/rotation (0)
Fully covered: `!compact`, `!compact-auto`, `!compact-failed`, `!context-budget-warning` (weak — see (a)), `!rotate` all have Go coverage; `!compact`/`!rotate` also have webapp coverage.

### Subagents (1)
- `!subagent-failed` — no test drives a subagent that ends in failure (distinct from `!subagent-detached-live`'s user-stopped case, which IS covered).

### Detached bash (3)
- `!bash-detach-fail` — no test drives a detached shell ending non-zero.
- `!bash-detach-live` — no test drives a detached shell left running forever (distinct from `!bash-detach-poll`'s live-growth check mid-turn).
- `!bash-hold` — no test drives a live-forever FOREGROUND bash (the lever AGENTS.md says exists specifically to reach DetachForeground's `unsupported` refusal; that refusal path itself appears untested).

### Merge/hold (0)
Fully covered: `!hold`, `!perm-hold`, `!cancel-all` all have Go coverage; `!hold`/`!perm-hold` also have webapp and (for `!hold`, `!perm-hold`) Emacs coverage.

### Failure arms — api-error classes, model refusal, max turns/budget, execution error, stop hook (28)
- Every `!api-*` class is uncovered: `!api-400`, `!api-401`, `!api-403`, `!api-404`, `!api-413`, `!api-429`, `!api-500`, `!api-529`, `!api-billing`, `!api-max-output`, `!api-oauth-org`, `!api-unmodeled`. None of the twelve `AgentFailure.api_request_failed` sub-arms is exercised by any counted e2e test, despite each having a fully declared shape in the mocked vendor.
- Model-refusal recovery: `!refusal-fallback`, `!refusal-no-fallback` — neither the fallback-model recovery nor the no-fallback dead-end is driven.
- `terminal_reason` failure arms with no capture AND no e2e test: `!fail-aborted-tools`, `!fail-blocking-limit`, `!fail-continuation-prevented`, `!fail-hook-stopped`, `!fail-image`, `!fail-malformed-tool-use`, `!fail-model`, `!fail-prompt-too-long`, `!fail-rapid-refill`, `!fail-tool-deferred`, `!fail-tool-deferred-unavailable`, `!fail-turn-setup`. (Contrast with the SAME family's covered members — `!fail-execution`, `!fail-max-turns`, `!fail-budget`, `!fail-stop-hook`, `!fail-structured-output` — all driven by `turnlifecycle_e2e_test.go`, which evidently stops short of the full terminal-reason enumeration.)
- Converter-defect / recovery pair: `!fault-converter`, `!fault-recover` — the `SessionFault.converter_defect`/recovery arm and its degraded-window open/close pair are undriven.

### Other (21)
- Session/footer facts with no dedicated test: `!fast-off`, `!fast-cooldown` (siblings of the covered-but-weak `!fast-on`), `!mcp-healthy` (sibling of the covered `!mcp-all`), `!rate-limit`, `!rate-limit-five-hour`, `!rate-limit-seven-day`, all five `!usage-*-unavailable`/`!usage-opus-absent`/`!usage-sampling-failure` account-usage negative shapes (siblings of the covered `!usage-full`/`!usage-available`).
- Read/web edge shapes: `!read-image`, `!read-truncated`, `!web-fetch-redirect`.
- Vendor bookkeeping/residue: `!away-summary`, `!context-tip`, `!residue`, `!tokens-reminder`, `!context-window` — all residue-only or context-window producers with no arm to assert.
- `!keepalive` — the keep-alive marker convention has no dedicated driving test.
- `!send-message-refused` — the subagent-refusal arm of the send-message family (siblings `!send-message`/`!send-message-resumed` ARE driven, but only weakly — see (a)).

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

## (e) Dead-trigger findings

**None found as live defects.** Every literal `!name` (or bare scenario-name
string passed to `driveScenarioToCompletion`/`driveDocumentedPrompt`) that
any of the three counted e2e layers actually submits to the mocked vendor
resolves to a real registered scenario or a real golden alias.

Two items worth flagging as documentation/naming friction, not dead
triggers:

- `!prose-streamed` is used as a prompt literal in
  `e2e/emacs_handover_e2e_test.go` (`TestEmacsHandoverTransfersAtFreeness`,
  const `emHO40Prompt`) and in `e2e/adoption_e2e_test.go`. It IS a real,
  registered token: `registry.ts`'s `ALIASES` maps the golden name
  `"prose-streamed"` to the canonical empty-string default scenario, and
  `NAMED` (which `selectScenario` matches against) is built from
  `SCENARIOS` **plus** every `ALIASES` entry — so `!prose-streamed`
  resolves through the alias table to the same default `PROSE` scenario a
  bare prompt would reach anyway. Not a dead trigger; flagged only because
  one sub-agent's search misread the alias mechanism as absent.
- `!perm-undecidable` appears only inside a doc comment in
  `permission_e2e_test.go`, explicitly as the negative the comment contrasts
  `!perm-hold` against ("NOT `!perm-undecidable`") — never passed to any
  RPC call. Counted as uncovered above, not as a dead trigger, since nothing
  in a counted e2e layer submits it at all (so there is no wrong-resolution
  risk to report — the scenario is simply undriven).

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
  backgrounds a Bash call. Covered by shim unit tests only, generically
  over `AgentActivityId` (not subagent-specific, since no unit exercises a
  subagent-shaped activity id through this path):
  `agent-shim/claude/shim/test/engine/turn.test.ts`
  (`describe("DetachForeground", ...)` — `unknownUnit`, `alreadyConcluded`,
  success — and `describe("DetachForeground on a live foreground unit",
  ...)` — `unsupported`, the two CONFIRMS cases) and
  `agent-shim/claude/shim/test/engine/session.test.ts`. Applying
  `DetachForeground` specifically to a subagent unit (as opposed to a bash
  unit) is a GAP: no unit test constructs a subagent-shaped
  `AgentActivityId` for this rpc, so the subagent-specific shape the
  deleted e2e test named is untested at every layer.
