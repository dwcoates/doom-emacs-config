# E2E seam audit (2026-11-12)

Owner ask: work out what `e2e` and `e2e-emacs` cover, convert what one system alone can prove into that system's tiers, and keep only one wiring test per real seam (AGENTS.md, "An e2e test proves a seam between real systems, and nothing else").

## Why

- The two suites take about 20 to 23% of a full run's CPU: `e2e` about 1.8 CPU-minutes recorded, `e2e-emacs` up to about 3.4 inside its Docker VM, which the unit metrics do not see.
- Neither can be reused, so they bound a no-change rerun at about 3 to 4 minutes.
- `e2e-emacs` is the only reason Docker is a requirement (`e2e/requirements.json`).
- Of 363 full gate runs since Oct 25, 22 (6.1%) failed only in these suites, 21 of them before Nov 1.

## Findings

- No e2e test names its seam: 0 of 139 top-level tests carry a `Seam:` line in their doc comment, against AGENTS.md's rule. A few steps name one inside their step comment.
- 114 scenario steps, by proposed home:
  - keep: real seam: 45
  - convert: ERT with the fake daemon: 26
  - convert: daemon integration (fakeshim frames → projection): 17
  - convert: shim integration (fake SDK → frames): 14
  - move: harness unit tests, out of the e2e package: 6
  - Doom tier: nightly container run: 6
- 127 further top-level tests in the package test the harness itself (teardown, backtraces, artifact tails, preflight bounds, shared world, scenario plumbing, perf budgets). They are unit tests, but run inside a suite that is never reused, so every full run pays for them.

## Plan

1. Lint: every top-level e2e test and every scenario step names its seam (`Seam: <producer> -> <consumer>, <what crosses>`), so a test that cannot name one fails the repo scan instead of landing.
2. Move the harness's own tests into a package of their own, keyed for reuse.
3. Convert each step marked daemon, shim or ERT into that system's integration tier, deleting the e2e step only once its replacement passes and covers its assertion.
4. Keep the steps marked keep, each given its `Seam:` line.
5. Move the Doom tier, the one thing that needs Docker after step 3, to a nightly container run, so Docker stops gating merges.

## Every step

| Scenario | Step | Proposed | Why |
|---|---|---|---|
| `TestTurns` | `testTurnStartToCompletion` | keep: real seam | the one prompt-to-feed wiring test across daemon, shim, store and sidecar |
| `TestTurns` | `testInterruptAfterTextDelta` | keep: real seam | Interrupt crosses daemon rpc → shim → vendor mid-tool |
| `TestTurns` | `testCompactionDirected` | keep: real seam | declares Seam: compaction (daemon ↔ shim) |
| `TestTurns` | `testAWorkspaceOpenReplaysItsFirstPageOnce` | keep: real seam | store → daemon → feed replay ordering across processes |
| `TestTurns` | `testContextUsage` | convert: daemon integration (fakeshim frames → projection) | what the daemon projects from frames it is handed (feed rows, footer chips, refusal arms): one system's behavior, given fakeshim frames |
| `TestTurns` | `testNoFooterPushBetweenFeedItemsCarriesAFeedItemDerivedLine` | convert: daemon integration (fakeshim frames → projection) | what the daemon projects from frames it is handed (feed rows, footer chips, refusal arms): one system's behavior, given fakeshim frames |
| `TestTurns` | `testAnOpenQuestionIsNamedAndGreenOnTheStripAndTheRail` | convert: daemon integration (fakeshim frames → projection) | what the daemon projects from frames it is handed (feed rows, footer chips, refusal arms): one system's behavior, given fakeshim frames |
| `TestTurns` | `testCronCreateSetsTheFootersCronsChip` | convert: daemon integration (fakeshim frames → projection) | what the daemon projects from frames it is handed (feed rows, footer chips, refusal arms): one system's behavior, given fakeshim frames |
| `TestTurns` | `testPlanModeCoalescesOntoOneBubble` | convert: daemon integration (fakeshim frames → projection) | what the daemon projects from frames it is handed (feed rows, footer chips, refusal arms): one system's behavior, given fakeshim frames |
| `TestTurns` | `testPlanModeSettlesPlanned` | convert: daemon integration (fakeshim frames → projection) | what the daemon projects from frames it is handed (feed rows, footer chips, refusal arms): one system's behavior, given fakeshim frames |
| `TestTurns` | `testChessBoard` | convert: daemon integration (fakeshim frames → projection) | what the daemon projects from frames it is handed (feed rows, footer chips, refusal arms): one system's behavior, given fakeshim frames |
| `TestTurns` | `testSlashShapeAUnnamed` | convert: shim integration (fake SDK → frames) | how the shim converts a fake-SDK scenario into frames: one system's behavior; the daemon half is the daemon tier's |
| `TestTurns` | `testSkillInvocation` | convert: shim integration (fake SDK → frames) | how the shim converts a fake-SDK scenario into frames: one system's behavior; the daemon half is the daemon tier's |
| `TestTurns` | `testAChatThreadLivesItsWholeLife` | keep: real seam | a chat thread's whole life crosses daemon, shim and store |
| `TestTurns` | `testMcpToolCall` | convert: shim integration (fake SDK → frames) | how the shim converts a fake-SDK scenario into frames: one system's behavior; the daemon half is the daemon tier's |
| `TestTurns` | `testIdeDiagnosticsAfterWrite` | convert: shim integration (fake SDK → frames) | how the shim converts a fake-SDK scenario into frames: one system's behavior; the daemon half is the daemon tier's |
| `TestTurns` | `testRateLimitSevenDayWindowFeedsTheWeeklyAllowance` | convert: daemon integration (fakeshim frames → projection) | what the daemon projects from frames it is handed (feed rows, footer chips, refusal arms): one system's behavior, given fakeshim frames |
| `TestTurns` | `testClearRotatesIdentity` | keep: real seam | identity rotation must agree across shim, store and daemon |
| `TestTurns` | `testSecondRotateUnderRotatedIdentity` | keep: real seam | identity rotation must agree across shim, store and daemon |
| `TestFaults` | `testQueryEofEndsTheTurnAsQueryDied` | convert: shim integration (fake SDK → frames) | how the shim converts a fake-SDK scenario into frames: one system's behavior; the daemon half is the daemon tier's |
| `TestFaults` | `testQueryEofMidAskDeniesTheOpenAsk` | convert: shim integration (fake SDK → frames) | how the shim converts a fake-SDK scenario into frames: one system's behavior; the daemon half is the daemon tier's |
| `TestFaults` | `testConverterRecoveryClosesTheDegradedWindow` | convert: shim integration (fake SDK → frames) | how the shim converts a fake-SDK scenario into frames: one system's behavior; the daemon half is the daemon tier's |
| `TestFaults` | `testResidueAttachmentsDrawNoRow` | convert: daemon integration (fakeshim frames → projection) | what the daemon projects from frames it is handed (feed rows, footer chips, refusal arms): one system's behavior, given fakeshim frames |
| `TestFaults` | `testContextTipDrawsNoRow` | convert: daemon integration (fakeshim frames → projection) | what the daemon projects from frames it is handed (feed rows, footer chips, refusal arms): one system's behavior, given fakeshim frames |
| `TestFaults` | `testTokensReminderDrawsNoRow` | convert: daemon integration (fakeshim frames → projection) | what the daemon projects from frames it is handed (feed rows, footer chips, refusal arms): one system's behavior, given fakeshim frames |
| `TestFaults` | `testBubbleRefusedNotDeliverable` | convert: daemon integration (fakeshim frames → projection) | what the daemon projects from frames it is handed (feed rows, footer chips, refusal arms): one system's behavior, given fakeshim frames |
| `TestFaults` | `testBubbleRefusedAgentBusy` | convert: daemon integration (fakeshim frames → projection) | what the daemon projects from frames it is handed (feed rows, footer chips, refusal arms): one system's behavior, given fakeshim frames |
| `TestFaults` | `testUnknownAgentOnUpdateAgent` | convert: daemon integration (fakeshim frames → projection) | what the daemon projects from frames it is handed (feed rows, footer chips, refusal arms): one system's behavior, given fakeshim frames |
| `TestFaults` | `testStartSessionVendorStartFailedRecovers` | convert: daemon integration (fakeshim frames → projection) | the daemon's recovery from a refused start, given the shim's refusal |
| `TestFaults` | `testApiRateLimitedCarriesTheVendorWait` | convert: daemon integration (fakeshim frames → projection) | what the daemon projects from frames it is handed (feed rows, footer chips, refusal arms): one system's behavior, given fakeshim frames |
| `TestFaults` | `testPermissionAskAnsweredArms` | convert: shim integration (fake SDK → frames) | how the shim converts a fake-SDK scenario into frames: one system's behavior; the daemon half is the daemon tier's |
| `TestFaults` | `testHookBlocked` | convert: shim integration (fake SDK → frames) | how the shim converts a fake-SDK scenario into frames: one system's behavior; the daemon half is the daemon tier's |
| `TestFaults` | `testHookFailed` | convert: shim integration (fake SDK → frames) | how the shim converts a fake-SDK scenario into frames: one system's behavior; the daemon half is the daemon tier's |
| `TestSubagents` | `testSubagentSyncNestedActivity` | convert: shim integration (fake SDK → frames) | how the shim converts a fake-SDK scenario into frames: one system's behavior; the daemon half is the daemon tier's |
| `TestSubagents` | `testSubagentDetached` | keep: real seam | detached work and its feeds across shim, store and a cold-booted daemon |
| `TestSubagents` | `testNestedSubagentHistoricalUsage` | convert: shim integration (fake SDK → frames) | how the shim converts a fake-SDK scenario into frames: one system's behavior; the daemon half is the daemon tier's |
| `TestSubagents` | `testSubagentBubbleFromAReplayIsStillAddressable` | keep: real seam | detached work and its feeds across shim, store and a cold-booted daemon |
| `TestSubagents` | `testSubagentDetachedSurvivesAnInterjection` | keep: real seam | detached work and its feeds across shim, store and a cold-booted daemon |
| `TestSubagents` | `testSubagentInterleavedResponsesStayOnTheirOwnFeeds` | keep: real seam | detached work and its feeds across shim, store and a cold-booted daemon |
| `TestSubagents` | `testADetachedSubagentWhoseProcessDiedIsNotLiveAfterAColdBoot` | keep: real seam | detached work and its feeds across shim, store and a cold-booted daemon |
| `TestSubagents` | `testBashDetachedStartAndComplete` | keep: real seam | declares Seam: shell-spool (shim ↔ sidecar) |
| `TestSubagents` | `testBashDetachExplicitPoll` | convert: shim integration (fake SDK → frames) | how the shim converts a fake-SDK scenario into frames: one system's behavior; the daemon half is the daemon tier's |
| `TestSubagents` | `testBashForegroundCompleted` | convert: shim integration (fake SDK → frames) | how the shim converts a fake-SDK scenario into frames: one system's behavior; the daemon half is the daemon tier's |
| `TestSubagents` | `testDetachedLostWentSilent` | keep: real seam | declares Seam: shell-spool (shim ↔ sidecar) |
| `TestKeepAlive` | `testKeepAliveAnswerAfterVendorTurnNeverServed` | keep: real seam | keep-alive and rewind ordering between shim, vendor, store and sidecar |
| `TestKeepAlive` | `testKeepAliveStoresNothingOnEitherPlane` | keep: real seam | keep-alive and rewind ordering between shim, vendor, store and sidecar |
| `TestKeepAlive` | `testPromptDuringKeepAliveTurnsOnce` | keep: real seam | keep-alive and rewind ordering between shim, vendor, store and sidecar |
| `TestKeepAlive` | `testRevivalAfterHibernate` | keep: real seam | keep-alive and rewind ordering between shim, vendor, store and sidecar |
| `TestKeepAlive` | `testRevivalRunsInTheSessionsModeNotTheSummarizers` | keep: real seam | keep-alive and rewind ordering between shim, vendor, store and sidecar |
| `TestKeepAlive` | `testPromptDuringKeepAliveRewindsPastTheKeepAliveAlone` | keep: real seam | keep-alive and rewind ordering between shim, vendor, store and sidecar |
| `TestKeepAlive` | `testStopReplayedByEveryRewindNeverLoops` | keep: real seam | keep-alive and rewind ordering between shim, vendor, store and sidecar |
| `TestKeepAlive` | `testRestartedShimCarriesItsKeepAlivesUntilARealReply` | keep: real seam | keep-alive and rewind ordering between shim, vendor, store and sidecar |
| `TestDaemonLifecycle` | `testHostRequestedStopLeavesNoProcessBehind` | keep: real seam | stop, relaunch, cold bring-up and store restart: whole-process lifecycle |
| `TestDaemonLifecycle` | `testAStopAfterACompletedTurnLeavesOnTheStop` | keep: real seam | stop, relaunch, cold bring-up and store restart: whole-process lifecycle |
| `TestDaemonLifecycle` | `testACompletedTurnAndTeardownLeaveNoWatchTokenOutstanding` | keep: real seam | stop, relaunch, cold bring-up and store restart: whole-process lifecycle |
| `TestDaemonLifecycle` | `testARelaunchedDaemonKeepsTheConversationItStoodDown` | keep: real seam | stop, relaunch, cold bring-up and store restart: whole-process lifecycle |
| `TestDaemonLifecycle` | `testColdGate` | keep: real seam | stop, relaunch, cold bring-up and store restart: whole-process lifecycle |
| `TestDaemonLifecycle` | `testColdBringUpEarnsNoStoreRefusal` | keep: real seam | stop, relaunch, cold bring-up and store restart: whole-process lifecycle |
| `TestDaemonLifecycle` | `testColdBootReadsReplayFromStore` | keep: real seam | stop, relaunch, cold bring-up and store restart: whole-process lifecycle |
| `TestDaemonLifecycle` | `testASessionlessWorkspaceHandedOverDrawsItsFeedWithNoPrompt` | keep: real seam | stop, relaunch, cold bring-up and store restart: whole-process lifecycle |
| `TestDaemonLifecycle` | `testRecoveryAfterStoreRestart` | keep: real seam | stop, relaunch, cold bring-up and store restart: whole-process lifecycle |
| `TestLanding` | `testADeployThatFindsTheDaemonStaleHandsOverAFreeWorkspace` | keep: real seam | deploys and handovers between two real daemons |
| `TestLanding` | `testADeployOverARunningTurnHandsItOverAndEndsNothing` | keep: real seam | deploys and handovers between two real daemons |
| `TestLanding` | `testASuccessorThatFinishedJoiningAcceptsADeploy` | keep: real seam | deploys and handovers between two real daemons |
| `TestLanding` | `testAWorkspaceWhoseMergeFailedTakesTheNextPromptAsAnOrdinaryTurn` | convert: daemon integration (fakeshim frames → projection) | the daemon's prompt routing after a failed merge, given its own state |
| `TestLanding` | `testDisplacedTurnCapturedEndedThenResubmittedExactlyOnce` | keep: real seam | deploys and handovers between two real daemons |
| `TestLanding` | `testFanWideCancel` | keep: real seam | deploys and handovers between two real daemons |
| `TestLanding` | `testFailMarkerFailsAPreDeployHooksDeploy` | keep: real seam | deploys and handovers between two real daemons |
| `TestWebappLayer` | `testWebappLayerCards` | move: harness unit tests, out of the e2e package | 2 lines: delegates to the webapp layer's own cards test |
| `TestWebappLayer` | `testWebappLayerClientLog` | keep: real seam | browser → daemon log join, and the page across a daemon handover |
| `TestWebappLayer` | `testWebappLayerRestartHandover` | keep: real seam | browser → daemon log join, and the page across a daemon handover |
| `TestHarness` | `testTheSharedWorldGivesEachTestItsOwnViewOfOneDaemon` | move: harness unit tests, out of the e2e package | tests of the e2e harness itself |
| `TestHarness` | `testNewRepoWorkspaceRegistersTheRepositoryItMinted` | move: harness unit tests, out of the e2e package | tests of the e2e harness itself |
| `TestHarness` | `testNewWorkspaceMintsAFreshWorkspaceEachTime` | move: harness unit tests, out of the e2e package | tests of the e2e harness itself |
| `TestHarness` | `testRequireOpenOpensARegisteredWorkspace` | move: harness unit tests, out of the e2e package | tests of the e2e harness itself |
| `TestHarness` | `testRequireOpenFailsTheTestForAWorkspaceTheDaemonDoesNotKnow` | move: harness unit tests, out of the e2e package | tests of the e2e harness itself |
| `TestEmacsLayout` | `testEmacsPlainCloseTogglesThePanelsBack` | convert: ERT with the fake daemon | Emacs-side window, composer and popup behavior: the fake daemon answers what the real one would |
| `TestEmacsLayout` | `testEmacsKillCloseLeavesTheTabOrderAlone` | convert: ERT with the fake daemon | Emacs-side window, composer and popup behavior: the fake daemon answers what the real one would |
| `TestEmacsLayout` | `testEmacsFocusInputSelectsTheComposer` | convert: ERT with the fake daemon | Emacs-side window, composer and popup behavior: the fake daemon answers what the real one would |
| `TestEmacsLayout` | `testEmacsFullscreenTogglesAndRestores` | convert: ERT with the fake daemon | Emacs-side window, composer and popup behavior: the fake daemon answers what the real one would |
| `TestEmacsLayout` | `testEmacsDeleteOtherWindowsOverPanelsKeepsTheWorkLayout` | convert: ERT with the fake daemon | Emacs-side window, composer and popup behavior: the fake daemon answers what the real one would |
| `TestEmacsLayout` | `testEmacsOpenProgressLadderReachesRendered` | convert: ERT with the fake daemon | Emacs-side window, composer and popup behavior: the fake daemon answers what the real one would |
| `TestEmacsLayout` | `testEmacsDiscardInputClearsTheComposer` | convert: ERT with the fake daemon | Emacs-side window, composer and popup behavior: the fake daemon answers what the real one would |
| `TestEmacsLayout` | `testEmacsHistoryRecallRestoresTheLastPrompt` | convert: ERT with the fake daemon | Emacs-side window, composer and popup behavior: the fake daemon answers what the real one would |
| `TestEmacsLayout` | `testEmacsAnUnroutableFileRecordsARefusal` | convert: ERT with the fake daemon | Emacs-side window, composer and popup behavior: the fake daemon answers what the real one would |
| `TestEmacsLayout` | `testEmacsPendingPlacementResolvesAfterRosterReconcile` | convert: ERT with the fake daemon | Emacs-side window, composer and popup behavior: the fake daemon answers what the real one would |
| `TestEmacsLayout` | `testEmacsPopupOpensADirectoryInDired` | convert: ERT with the fake daemon | Emacs-side window, composer and popup behavior: the fake daemon answers what the real one would |
| `TestEmacsWorkspaces` | `testEmacsProofOfLife` | keep: real seam | the one Emacs → daemon → shim → page wiring test |
| `TestEmacsWorkspaces` | `testEmacsCreateWorkspaceAppearsOnTheRoster` | convert: ERT with the fake daemon | what Emacs does with a roster push or a reply: the fake daemon can push both |
| `TestEmacsWorkspaces` | `testEmacsRegisterAnExistingDirectory` | convert: ERT with the fake daemon | what Emacs does with a roster push or a reply: the fake daemon can push both |
| `TestEmacsWorkspaces` | `testEmacsCloseWithAHeldPromptDoesNotTearTheTabDown` | convert: ERT with the fake daemon | what Emacs does with a roster push or a reply: the fake daemon can push both |
| `TestEmacsWorkspaces` | `testEmacsCloseThenKillDoesNotWedgeEmacs` | convert: ERT with the fake daemon | what Emacs does with a roster push or a reply: the fake daemon can push both |
| `TestEmacsWorkspaces` | `testEmacsRosterArmsPaintTheTabInOrder` | convert: ERT with the fake daemon | what Emacs does with a roster push or a reply: the fake daemon can push both |
| `TestEmacsWorkspaces` | `testEmacsUnknownRosterArmIsRefusedLoudly` | convert: ERT with the fake daemon | what Emacs does with a roster push or a reply: the fake daemon can push both |
| `TestEmacsWorkspaces` | `testEmacsPermissionAskFiresTheAttentionMarker` | convert: ERT with the fake daemon | what Emacs does with a roster push or a reply: the fake daemon can push both |
| `TestEmacsWorkspaces` | `testEmacsUnfocusedTurnEndRaisesTheDaemonsBanner` | convert: ERT with the fake daemon | what Emacs does with a roster push or a reply: the fake daemon can push both |
| `TestEmacsWorkspaces` | `testEmacsDrainScheduleDrawsTheStandingBanner` | convert: ERT with the fake daemon | what Emacs does with a roster push or a reply: the fake daemon can push both |
| `TestEmacsWorkspaces` | `testEmacsReachesAChatThreadOffTheTabBar` | convert: ERT with the fake daemon | what Emacs does with a roster push or a reply: the fake daemon can push both |
| `TestEmacsWorkspaces` | `testEmacsGuiRegistryNamesOnlyLiveCapabilities` | convert: ERT with the fake daemon | what Emacs does with a roster push or a reply: the fake daemon can push both |
| `TestEmacsWorkspaces` | `testEmacsGuiDurableSessionIdNamesTheVendorConversation` | convert: ERT with the fake daemon | what Emacs does with a roster push or a reply: the fake daemon can push both |
| `TestEmacsDaemonLifecycle` | `testEmacsLauncherArgvCarriesBothAccountRoots` | convert: ERT with the fake daemon | Emacs's launcher argv and its transport handling, against the fake daemon |
| `TestEmacsDaemonLifecycle` | `testEmacsSubmitWithNoDaemonIsRefusedLoudly` | convert: ERT with the fake daemon | Emacs's launcher argv and its transport handling, against the fake daemon |
| `TestEmacsDaemonLifecycle` | `testEmacsTabsRehydrateFromTheRosterOnConnect` | keep: real seam | Emacs across a real daemon restart or handover |
| `TestEmacsDaemonLifecycle` | `testEmacsDaemonDownSurfacesAndReconnects` | convert: ERT with the fake daemon | Emacs's launcher argv and its transport handling, against the fake daemon |
| `TestEmacsDaemonLifecycle` | `testEmacsHandoverTransfersAtFreeness` | keep: real seam | Emacs across a real daemon restart or handover |
| `TestEmacsDaemonLifecycle` | `testEmacsRestartKeepsTheSelectedWorkspace` | keep: real seam | Emacs across a real daemon restart or handover |
| `TestEmacsDaemonLifecycle` | `testEmacsForcedRestartInterruptsTheTurn` | keep: real seam | Emacs across a real daemon restart or handover |
| `TestEmacsDaemonLifecycle` | `testEmacsRestartHoldsPromptsMeanwhile` | keep: real seam | Emacs across a real daemon restart or handover |
| `TestDoom` | `testDoomPopupOpensAFileRightSideHalfWidth` | Doom tier: nightly container run | behavior only a booted Doom shows |
| `TestDoom` | `testDoomRegisteringAWorkspaceSelectsIt` | Doom tier: nightly container run | behavior only a booted Doom shows |
| `TestDoom` | `testDoomWorkspaceNavigationFollowsTheDrawnTabBar` | Doom tier: nightly container run | behavior only a booted Doom shows |
| `TestDoom` | `testDoomANewProjectDoesNotRecycleTheWorkspaceBeingLeft` | Doom tier: nightly container run | behavior only a booted Doom shows |
| `TestDoom` | `testDoomPlainCloseHidesPanelsAndLeavesTheTabAlone` | Doom tier: nightly container run | behavior only a booted Doom shows |
| `TestDoom` | `testDoomSplashLoadsWithoutTheDashboardModule` | Doom tier: nightly container run | behavior only a booted Doom shows |

## Harness tests outside the scenarios (move to their own package)

`TestWebappLayerCoverageEnvHandsEachDriverADirectoryOfItsOwn`, `TestWebappLayerCoverageEnvIsNothingWithoutCoverage`, `TestWebappLayerCoverageEnvRefusesAnUnwritableRoot`, `TestCursorIngested`, `TestEveryCursorWaitGoesThroughAwaitCursors`, `TestConclusionBudgetRecords`, `TestAwaitDoomStamp`, `TestStatCPUTicks`, `TestEmacsCPUOfAnUnknownPidIsNothingSpent`, `TestServerSocketDirConfined`, `TestTheRegistryIsReadThroughOneForm`, `TestTheExcerptLeadsWithTheMainThread`, `TestTheExcerptKeepsEveryMainThreadFrame`, `TestTheExcerptQuotesTheMainThreadOnlyOnce`, `TestTheExcerptKeepsTheThreadIndex`, `TestTheExcerptTrimsHelperThreadsAndSaysSo`, `TestTheExcerptTrimsAHelperThreadWholeOrNotAtAll`, `TestAnUnthreadedCaptureIsQuotedWhole`, `TestNextXvfbDisplayNeverRepeatsANumber`, `TestXvfbArgvStartsTheServerOnItsHandedOutDisplay`, `TestSandboxRunCommandEntersTheE2EModule`, `TestTailArtifactsPrintsEveryCollectedLog`, `TestTailArtifactsPrintsEmacsMessagesFirst`, `TestTailArtifactsSkipsWhatIsNotALog`, `TestTailArtifactsBoundsOneLog`, `TestTailArtifactsNamesALogPastTheBudget`, `TestReadCgroupCPUStatReadsEveryCounter`, `TestReadCgroupCPUStatRefusesAMalformedCounter`, `TestCPUOverProbeSaysWhenTheBeforeReadFailed`, `TestEmacsTeardownWaitsForTheProcessToExit`, `TestEmacsTeardownWaitsForTheGroupOfAnEmacsAlreadyGone`, `TestEmacsTeardownEscalatesToTheProcessGroup`, `TestEmacsTeardownReportsAStrayAsAFailure`, `TestEmacsTeardownWaitsForTheStoppedDaemonToExit`, `TestEmacsTeardownDoesNotWaitOnADaemonNobodyAskedToStop`, `TestEmacsTeardownDoesNotCountASparedProcessAsAStray`, `TestEmacsTeardownDaemonExitDoesNotWaitOutTheBoundOnASparedProcess`, `TestTheEmacsWorldIsolatesItsDaemonFromTheOwnersZettel`, `TestEndsTurn`, `TestTurnEndedMatchIsShared`, `TestHookRowsAnswersEveryHookCardInFeedOrder`, `TestNoE2EWaitIsBoundOnWallTime`, `TestWallClockDeclaredReadsOnlyTheCommentBlockAboveTheBound`, `TestResolvedPathResolvesASymlinkedDirectory`, `TestResolvedPathLeavesAnUnsymlinkedDirectoryUnchanged`, `TestResolvedPathFailsLoudlyOnAMissingDirectory`, `TestWorldDaemonEnvLeavesTheCheckoutToTheHarness`, `TestShortStateRootKeepsTheLockDirectoryOffTheSharedTempRoot`, `TestWebappWritableAcceptsAWritablePackageDirectory`, `TestWebappWritableRejectsADirectoryItCannotCreateTheCacheIn`, `TestPreserveWorkspaceElispLogsFollowsTheCanonicalSymlink`, `TestPreserveWorkspaceElispLogsIgnoresAnEmacsDirectoryOutsideDotClaude`, `TestPreserveWorkspaceElispLogsFindsASinkBesideTheEmacsRoot`, `TestProbeServerAsksOnceUnderTheHeartbeatBound`, `TestEmacsclientChargesItsBoundOnlyGrantedTime`, `TestEveryServerCallRidesTheEmacsclientHelper`, `TestSweepRecordSinceIgnoresARecordBeforeTheMark`, `TestSweepRecordSinceFindsARecordAfterTheMark`, `TestSweepRecordSinceIgnoresAnotherOperationAtTheSameMessage`, `TestKeepaliveWorldIsTheOneCompressedCadence`, `TestLandingSettled`, `TestLandingStateKeepsBufferNamesWhole`, `TestPerfDeclineAreaSkipsBeforeTheChildStarts`, `TestPerfDeclineAreaRecordsOneDeclinedRowPerAssertion`, `TestPerfDeclineAreaNamesTheCalibrationInItsSkip`, `TestPerfWebappAreaNamesEveryShippedAssertion`, `TestPerfSubmitPromptAck`, `TestPerfSubmitPromptToRosterArm`, `TestPerfSelectWorkspaceAck`, `TestPerfFooterFlipOnParticipantLoss`, `TestPerfRosterSubscribeReplay`, `TestPerfWebappLayer`, `TestHostPreflightBoundReportsAScriptThatNeverAnswers`, `TestHostPreflightBoundNamesTheWedgedEngine`, `TestHostPreflightQuotesANonZeroExitVerbatim`, `TestHostPreflightIsSilentWhenTheSandboxIsUsable`, `TestPreflightScriptExitsFourteenWhenTheRuntimeNeverAnswers`, `TestPreflightScriptWedgeMessageSaysToRestartTheEngine`, `TestScenarioOverrideReadsEveryRedefinitionForm`, `TestThePrewarmCoversEveryPrimitiveTheScenariosRedefine`, `TestProcessExitAnswersEveryObserver`, `TestProcessExitAwaitWithinBoundsARunningProcess`, `TestProcessExitReportsARealChildsExit`, `TestProgramOnce`, `TestEveryProgramThisSuiteBuildsGoesThroughProgramOnce`, `TestEverySandboxValueOfOneTestSharesItsScratch`, `TestTwoTestsHaveTwoScratches`, `TestStepNameIsTheFunctionNameWithoutItsPrefix`, `TestStepNameRefusesAFunctionNotNamedAsAStep`, `TestAScenarioRunsItsStepsInOrderBesideTheOtherScenarios`, `TestAScenarioWithNoStepsIsRefused`, `TestEveryStepRunsInExactlyOneScenario`, `TestEveryWorldBuildingTestIsAScenario`, `TestScenarioMatrixMatchesReality`, `TestMarkerScenarioSelection`, `TestMarkerScenarioCellRoundTrips`, `TestASharedWorldWhoseBuildFailedFailsEveryTestThatAsks`, `TestASharedWorldWhoseBuildLackedAPreconditionSkipsEveryTestThatAsks`, `TestASharedWorldIsBuiltOnceForEveryTestThatAsks`, `TestStoppingASharedStoreIsRefused`, `TestStoppingASharedSidecarIsRefused`, `TestRestartingASharedServiceIsRefused`, `TestEveryWorldChecksItsServicesThroughOneHelper`, `TestLogTailOfNamesTheChildsLastRecords`, `TestReadAgentBook`, `TestEveryBookReadGoesThroughReadAgentBook`, `TestStoreDumpOnStall`, `TestStoreDumpOnStallOfAStoreAlreadyGone`, `TestWebappLayerParticipantHoldOutlivesTheWaitBound`, `TestWlBootBudgetMirrorsTheChildsBootBudget`, `TestWlParseLiveGroupMembersLeavesOutZombiesAndOtherGroups`, `TestWlParseLiveGroupMembersRefusesAMalformedLine`, `TestWlKillTreeReapsAGrandchild`, `TestKillProcessGroup`, `TestTheLayerDriverIsKilledOnlyThroughKillGroup`, `TestWlServerRunRestartsTheBoundOnEveryFinishedTest`, `TestWlServerRunThatFinishesNoTestWithinTheBoundIsBroken`, `TestWlServerRunRestartsTheBoundOnceTheFileIsCollected`, `TestWlServerRunThatTimesOutNamesWhetherTheFileWasCollected`, `TestWlServerRunBoundsEachStretchByItsOwnMeasure`, `TestWlServerRunThatIsNeverCollectedNamesTheCollectBound`, `TestWebappLayerCollectBoundOutlivesTheSilenceBound`, `TestAServiceThatDiedOnItsOwnFailsTheTest`, `TestAServiceTheTestStoppedFailsNothing`, `TestAZettelThatDiedOnItsOwnFailsTheTest`, `TestNoTestMintsAWorkspaceByHand`, `TestNoTestRequiresAnOpenByHand`
