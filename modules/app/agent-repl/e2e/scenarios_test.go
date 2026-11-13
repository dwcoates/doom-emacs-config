package e2e

import "testing"

// THE E2E SCENARIOS: every top-level test in this package that builds a
// world (World, the shared World, EmacsWorld, DoomWorld) is one of these.
//
// Each scenario runs its steps in order over the worlds they build, and each
// step is a former top-level test kept whole: it builds or takes the same
// world it always did, owns its own workspaces, and reports pass or fail on
// its own (scenario, scenario_test.go). A step that must change its fixture
// still builds its own; the rule is AGENTS.md "A case may share an expensive
// fixture, never a slice another case reads".
//
// New e2e coverage is a new step of the scenario whose subject it extends. A
// new scenario is only for an entirely new, large feature
// (skills/agent-repl-tests/SKILL.md, "E2E coverage is a step of a
// scenario"). TestEveryWorldBuildingTestIsAScenario holds the line.

// TestTurns is a turn from prompt to end, and every feed row, footer chip and fact it draws.
//
// Seam: daemon -> shim -> store and sidecar, a turn from its prompt to its terminal and every row it draws
func TestTurns(t *testing.T) {
	scenario(t,
		testTurnStartToCompletion,
		testInterruptAfterTextDelta,
		testCompactionDirected,
		testAWorkspaceOpenReplaysItsFirstPageOnce,
		testPlanModeCoalescesOntoOneBubble,
		testPlanModeSettlesPlanned,
		testRateLimitSevenDayWindowFeedsTheWeeklyAllowance,
		testSkillInvocation,
		testAChatThreadLivesItsWholeLife,
		testClearRotatesIdentity,
		testSecondRotateUnderRotatedIdentity,
	)
}

// TestFaults is what the vendor, the producer and the hooks get wrong, and the refusals and failure arms they end in.
//
// Seam: shim -> daemon, the vendor's and the producer's faults and the arms they end in
func TestFaults(t *testing.T) {
	scenario(t,
		testQueryEofEndsTheTurnAsQueryDied,
		testConverterRecoveryClosesTheDegradedWindow,
		testResidueAttachmentsDrawNoRow,
		testContextTipDrawsNoRow,
		testTokensReminderDrawsNoRow,
		testApiRateLimitedCarriesTheVendorWait,
		testPermissionAskAnsweredArms,
	)
}

// TestSubagents is subagents, their feeds, and detached work that outlives its turn.
//
// Seam: shim and sidecar -> daemon, subagents' frames and detached work across the stream and file planes
func TestSubagents(t *testing.T) {
	scenario(t,
		testSubagentSyncNestedActivity,
		testSubagentDetached,
		testNestedSubagentHistoricalUsage,
		testSubagentBubbleFromAReplayIsStillAddressable,
		testSubagentDetachedSurvivesAnInterjection,
		testSubagentInterleavedResponsesStayOnTheirOwnFeeds,
		testADetachedSubagentWhoseProcessDiedIsNotLiveAfterAColdBoot,
		testBashDetachedStartAndComplete,
		testBashDetachExplicitPoll,
		testBashForegroundCompleted,
		testDetachedLostWentSilent,
	)
}

// TestKeepAlive is keep-alive turns, hibernation and revival.
//
// Seam: shim -> vendor -> store and sidecar, keep-alive turns, hibernation and the rewinds past them
func TestKeepAlive(t *testing.T) {
	scenario(t,
		testKeepAliveAnswerAfterVendorTurnNeverServed,
		testKeepAliveStoresNothingOnEitherPlane,
		testPromptDuringKeepAliveTurnsOnce,
		testRevivalAfterHibernate,
		testRevivalRunsInTheSessionsModeNotTheSummarizers,
		testPromptDuringKeepAliveRewindsPastTheKeepAliveAlone,
		testStopReplayedByEveryRewindNeverLoops,
		testRestartedShimCarriesItsKeepAlivesUntilARealReply,
	)
}

// TestDaemonLifecycle is the daemon's stops, cold boots, relaunches, cold gate and store outages.
//
// Seam: daemon -> store, shim and sidecar, the whole process tree across stops, relaunches, cold boots and store outages
func TestDaemonLifecycle(t *testing.T) {
	scenario(t,
		testHostRequestedStopLeavesNoProcessBehind,
		testAStopAfterACompletedTurnLeavesOnTheStop,
		testACompletedTurnAndTeardownLeaveNoWatchTokenOutstanding,
		testARelaunchedDaemonKeepsTheConversationItStoodDown,
		testColdGate,
		testColdBringUpEarnsNoStoreRefusal,
		testColdBootReadsReplayFromStore,
		testASessionlessWorkspaceHandedOverDrawsItsFeedWithNoPrompt,
		testRecoveryAfterStoreRestart,
	)
}

// TestLanding is deploys and the merge queue.
//
// Seam: incumbent daemon -> successor daemon, deploys and merge-queue handovers
func TestLanding(t *testing.T) {
	scenario(t,
		testADeployThatFindsTheDaemonStaleHandsOverAFreeWorkspace,
		testADeployOverARunningTurnHandsItOverAndEndsNothing,
		testASuccessorThatFinishedJoiningAcceptsADeploy,
		testDisplacedTurnCapturedEndedThenResubmittedExactlyOnce,
		testFanWideCancel,
		testFailMarkerFailsAPreDeployHooksDeploy,
	)
}

// TestWebappLayer is the real webapp's vitest areas driven against a real daemon.
//
// Seam: webapp -> daemon, the real page's rpcs, streams and client log against a real daemon
func TestWebappLayer(t *testing.T) {
	scenario(t,
		testWebappLayerCards,
		testWebappLayerClientLog,
		testWebappLayerRestartHandover,
	)
}

// TestHarness is the e2e harness's own World machinery: the shared World and minting workspaces.
func TestHarness(t *testing.T) {
	scenario(t,
		testTheSharedWorldGivesEachTestItsOwnViewOfOneDaemon,
		testNewRepoWorkspaceRegistersTheRepositoryItMinted,
		testNewWorkspaceMintsAFreshWorkspaceEachTime,
		testRequireOpenOpensARegisteredWorkspace,
		testRequireOpenFailsTheTestForAWorkspaceTheDaemonDoesNotKnow,
	)
}

// TestEmacsWorkspaces is Emacs's workspaces, tabs, roster arms, banners and chat threads.
//
// Seam: Emacs -> daemon -> shim -> page, a workspace from Emacs's command to its drawn reply
func TestEmacsWorkspaces(t *testing.T) {
	scenario(t,
		testEmacsProofOfLife,
		testEmacsReachesAChatThreadOffTheTabBar,
	)
}

// TestEmacsDaemonLifecycle is Emacs's daemon: cold start, restarts, handovers, reconnects and a daemon that is not there.
//
// Seam: daemon -> Emacs, the link across a real daemon restart or handover
func TestEmacsDaemonLifecycle(t *testing.T) {
	scenario(t,
		testEmacsTabsRehydrateFromTheRosterOnConnect,
		testEmacsHandoverTransfersAtFreeness,
		testEmacsRestartKeepsTheSelectedWorkspace,
		testEmacsForcedRestartInterruptsTheTurn,
		testEmacsRestartHoldsPromptsMeanwhile,
	)
}

// TestDoom is the Doom tier: agent-repl inside a real Doom.
func TestDoom(t *testing.T) {
	scenario(t,
		testDoomPopupOpensAFileRightSideHalfWidth,
		testDoomRegisteringAWorkspaceSelectsIt,
		testDoomWorkspaceNavigationFollowsTheDrawnTabBar,
		testDoomANewProjectDoesNotRecycleTheWorkspaceBeingLeft,
		testDoomPlainCloseHidesPanelsAndLeavesTheTabAlone,
		testDoomSplashLoadsWithoutTheDashboardModule,
	)
}
