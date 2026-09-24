package footer

import (
	"slices"
	"sort"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/vocab"
)

// connected puts a serving link under the workspace so the disconnected arm —
// which outranks everything — does not mask the arm under test.
// connected puts ALL THREE hops of connectivity truth up: the daemon-to-shim
// link and both client streams (daemon.md invariant 11). A test that wants one
// hop down states that hop itself.
func connected(h *harness) {
	h.r.SetParticipants(testWS, true, true)
	h.r.OnLink(testWS, shimclient.LinkConnected)
}

// permissionStart is one open consent ask.
func permissionStart(id, title string) *conversationv1.AgentPermission {
	return &conversationv1.AgentPermission{
		Id: &conversationv1.AgentPermissionId{Value: id},
		Result: &conversationv1.AgentPermission_Start{
			Start: &conversationv1.AgentPermissionStart{
				Prompt: &conversationv1.AgentPermissionPrompt{Title: title, DisplayName: "Bash"},
			},
		},
	}
}

// questionStart is one open question batch.
func questionStart(id string, texts ...string) *conversationv1.AgentQuestion {
	batch := &conversationv1.AgentQuestionBatch{}
	for _, text := range texts {
		batch.Questions = append(batch.Questions, &conversationv1.AgentQuestionAsked{
			Question: &conversationv1.AgentQuestionText{Text: text},
		})
	}
	return &conversationv1.AgentQuestion{
		Id:     &conversationv1.AgentQuestionId{Value: id},
		Result: &conversationv1.AgentQuestion_Start{Start: &conversationv1.AgentQuestionStart{Batch: batch}},
	}
}

func TestIdleIsReadyBeforeAnyTurnRan(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	connected(h)

	// Assert
	idle := h.view(t).GetStrip().GetStatus().GetIdle()
	if idle.GetReady() == nil {
		t.Fatalf("substatus = %+v, want ready", idle.GetSubstatus())
	}
}

func TestIdleIsDoneAfterATurnConcluded(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, completed(), nil)

	// Assert
	idle := h.view(t).GetStrip().GetStatus().GetIdle()
	if idle.GetDone() == nil {
		t.Fatalf("substatus = %+v, want done", idle.GetSubstatus())
	}
}

func TestTheArmIsThinkingFromSubmitBeforeAnyActivity(t *testing.T) {
	// The status word's footer wave gates on the `thinking` ARM. The daemon
	// publishes the turn on receipt (deliver.go), before the shim answers, so the
	// arm must already read `thinking` during submitting — the wave starts on
	// submit, not once the first activity or response arrives.

	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

	// Assert
	if got := h.status(t); got != "thinking" {
		t.Fatalf("status = %q, want thinking during the submitting phase", got)
	}
}

func TestThinkingIsSubmittingUntilTheFirstActivity(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

	// Assert
	thinking := h.view(t).GetStrip().GetStatus().GetThinking()
	if thinking.GetSubmitting() == nil {
		t.Fatalf("substatus = %+v, want submitting", thinking.GetSubstatus())
	}
}

func TestTheFirstActivityMovesSubmittingToThinking(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

	// Act
	h.r.OnActivity(testWS, mainAgent, thinkingActivity("unit-1"))

	// Assert
	thinking := h.view(t).GetStrip().GetStatus().GetThinking()
	if thinking.GetThinking() == nil {
		t.Fatalf("substatus = %+v, want thinking", thinking.GetSubstatus())
	}
}

func TestAClearActIsDrawnAsClearing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActClear})

	// Assert
	if h.view(t).GetStrip().GetStatus().GetThinking().GetClearing() == nil {
		t.Fatalf("want thinking · clearing for a /clear act")
	}
}

func TestASessionCompactingArmStartsCompacting(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

	// Act
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Compacting{Compacting: &conversationv1.SessionCompacting{}},
	})

	// Assert
	if h.view(t).GetStrip().GetStatus().GetThinking().GetCompacting() == nil {
		t.Fatalf("want thinking · compacting from the vendor's compacting signal")
	}
}

func TestWaitingOnPermissionCarriesTheGatedCallLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnPermission(testWS, mainAgent, permissionStart("ask-1", "Claude wants to run rm -rf build"))

	// Assert
	waiting := h.view(t).GetStrip().GetStatus().GetWaiting()
	if waiting.GetPermission() == nil {
		t.Fatalf("substatus = %+v, want permission", waiting.GetSubstatus())
	}
	if got := waiting.GetActivity().GetGatedCall().GetText(); got == "" {
		t.Fatalf("the required waiting activity carries no gated-call line")
	}
}

func TestADecidedPermissionLeavesWaiting(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnPermission(testWS, mainAgent, permissionStart("ask-1", "Claude wants to read foo"))

	// Act
	h.r.OnPermission(testWS, mainAgent, &conversationv1.AgentPermission{
		Id: &conversationv1.AgentPermissionId{Value: "ask-1"},
		Result: &conversationv1.AgentPermission_Success{
			Success: &conversationv1.AgentPermissionSuccess{},
		},
	})

	// Assert
	if got := h.status(t); got != "idle" {
		t.Fatalf("status = %q, want idle once the ask is decided", got)
	}
}

func TestWaitingOnQuestionComposesTheBatchLead(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnQuestion(testWS, mainAgent, questionStart("q-1", "Which approach?", "Ship it?"))

	// Assert
	waiting := h.view(t).GetStrip().GetStatus().GetWaiting()
	got := waiting.GetActivity().GetQuestionLead().GetText()
	if got != "2 questions · Which approach?" {
		t.Fatalf("lead = %q, want the count and the first question", got)
	}
}

func TestPermissionOutranksQuestion(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnQuestion(testWS, mainAgent, questionStart("q-1", "Which approach?"))

	// Act
	h.r.OnPermission(testWS, mainAgent, permissionStart("ask-1", "Claude wants to run make"))

	// Assert
	if h.view(t).GetStrip().GetStatus().GetWaiting().GetPermission() == nil {
		t.Fatalf("want permission to outrank an open question batch")
	}
}

func TestInterruptingOutranksEveryOtherWaitingStep(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnPermission(testWS, mainAgent, permissionStart("ask-1", "Claude wants to run make"))

	// Act
	h.r.SetInterrupting(testWS, true)

	// Assert
	waiting := h.view(t).GetStrip().GetStatus().GetWaiting()
	if waiting.GetInterrupting() == nil {
		t.Fatalf("substatus = %+v, want interrupting", waiting.GetSubstatus())
	}
	if waiting.GetActivity().GetInterrupting().GetText() == "" {
		t.Fatalf("the interrupting status carries no composed line")
	}
}

func TestTheColdGateIsAWaitingStep(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetColdGate(testWS, ColdGate{Standing: true, Detail: "context cold — 182k tokens to re-read"})

	// Assert
	waiting := h.view(t).GetStrip().GetStatus().GetWaiting()
	if waiting.GetColdGate() == nil {
		t.Fatalf("substatus = %+v, want cold_gate", waiting.GetSubstatus())
	}
	if waiting.GetActivity().GetColdGateCost().GetText() == "" {
		t.Fatalf("the cold gate's composed cost line is missing")
	}
}

func TestTheWakeupFallbackLosesToEveryRealStatus(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnActivity(testWS, mainAgent, wakeupScheduled(instant.Add(300*1000*1000*1000)))

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Assert
	if got := h.status(t); got != "thinking" {
		t.Fatalf("status = %q, want thinking: the wakeup fallback shows only where the footer reads idle", got)
	}
}

func TestTheWakeupFallbackStandsWhereTheFooterWouldReadIdle(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnActivity(testWS, mainAgent, wakeupScheduled(instant.Add(300*1000*1000*1000)))

	// Assert
	waiting := h.view(t).GetStrip().GetStatus().GetWaiting()
	if waiting.GetWakeup() == nil {
		t.Fatalf("substatus = %+v, want the wakeup fallback", waiting.GetSubstatus())
	}
	if waiting.GetActivity().GetWakeup().GetWakeAtMs() == 0 {
		t.Fatalf("the wakeup countdown carries no deadline")
	}
}

func TestBackgroundStandsWhileDetachedWorkRunsWithNoTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnDetachedWork(testWS, mainAgent, createdShell("work-1", "npm test"))

	// Assert
	if got := h.status(t); got != "background" {
		t.Fatalf("status = %q, want background", got)
	}
}

func TestATurnOutranksBackground(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnDetachedWork(testWS, mainAgent, createdShell("work-1", "npm test"))

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Assert
	if got := h.status(t); got != "thinking" {
		t.Fatalf("status = %q, want thinking", got)
	}
}

func TestAQueuedMergeCarriesItsPlace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetMerge(testWS, MergeFacts{State: "queued", QueuePosition: 3, QueueDepth: 7})

	// Assert
	queued := h.view(t).GetStrip().GetStatus().GetMerging().GetQueued()
	if queued.GetPosition() != 3 || queued.GetDepth() != 7 {
		t.Fatalf("queued = %+v, want position 3 of 7", queued)
	}
}

func TestAParkedMergeDrawsTheOrchestratorsComposedLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetMerge(testWS, MergeFacts{State: "parked", ParkedLine: "conflict in api.go needs you"})

	// Assert
	got := h.view(t).GetStrip().GetStatus().GetMerging().GetParked().GetLine()
	if got != "conflict in api.go needs you" {
		t.Fatalf("parked line = %q, want the orchestrator's own sentence", got)
	}
}

func TestTheActiveTabRefinesTheMergingPhase(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetMerge(testWS, MergeFacts{State: "merging", ActiveTab: "testing"})

	// Assert
	if h.view(t).GetStrip().GetStatus().GetMerging().GetTesting() == nil {
		t.Fatalf("want the testing phase from the front entry's active tab")
	}
}

func TestAMergingStateWithNoTabFallsToTheMergePhase(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetMerge(testWS, MergeFacts{State: "merging"})

	// Assert
	if h.view(t).GetStrip().GetStatus().GetMerging().GetMerge() == nil {
		t.Fatalf("want the merge phase when no active tab was stated")
	}
}

func TestAMergeOutranksWaiting(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnQuestion(testWS, mainAgent, questionStart("q-1", "which?"))

	// Act
	h.r.SetMerge(testWS, MergeFacts{State: "merging", ActiveTab: "merge"})

	// Assert
	if got := h.status(t); got != "merging" {
		t.Fatalf("status = %q, want merging", got)
	}
}

func TestAQueryDeathBlocksTheSession(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_QueryDied{
			QueryDied: &conversationv1.SessionQueryDied{},
		},
	})

	// Assert
	blocked := h.view(t).GetStrip().GetStatus().GetBlocked()
	if blocked.GetQueryDied() == nil {
		t.Fatalf("substatus = %+v, want query_died", blocked.GetSubstatus())
	}
	if blocked.GetActivity().GetQueryDied().GetText() == "" {
		t.Fatalf("the dead-query line is missing")
	}
}

func TestAQueryDeathKeepsItsLineUnderTheTurnsFailure(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_QueryDied{QueryDied: &conversationv1.SessionQueryDied{}},
	})

	// Act
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ExecutionError{
			ExecutionError: &conversationv1.AgentExecutionError{},
		},
	})

	// Assert
	blocked := h.view(t).GetStrip().GetStatus().GetBlocked()
	if blocked.GetActivity().GetQueryDied().GetText() == "" {
		t.Fatalf("the dead-query line is missing: activity = %+v", blocked.GetActivity())
	}
}

// queryDiedFailure is the terminal the shim owes the open turn when its query
// dies: AgentFailure's own query_died arm.
func queryDiedFailure() *conversationv1.AgentFailure {
	return &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_QueryDied{QueryDied: &conversationv1.SessionQueryDied{}},
	}
}

// TestAQueryDeathBlocksOnQueryDiedWhicheverStatementArrivesFirst: the session's
// query_died push and the turn's query_died terminal travel by independent
// channels, so the substatus must be query_died in either order.
func TestAQueryDeathBlocksOnQueryDiedWhicheverStatementArrivesFirst(t *testing.T) {
	died := &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_QueryDied{QueryDied: &conversationv1.SessionQueryDied{}},
	}
	cases := []struct {
		name string
		act  func(h *harness, turn *ids.TurnID)
	}{
		{name: "the terminal first, then the push", act: func(h *harness, turn *ids.TurnID) {
			h.r.OnAgentTerminal(testWS, mainAgent, turn, nil, queryDiedFailure())
			h.r.OnSessionUpdate(testWS, died)
		}},
		{name: "the push first, then the terminal", act: func(h *harness, turn *ids.TurnID) {
			h.r.OnSessionUpdate(testWS, died)
			h.r.OnAgentTerminal(testWS, mainAgent, turn, nil, queryDiedFailure())
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			turn := testTurnID
			h.r.SetTurn(testWS, &TurnStarted{At: instant})

			// Act
			tc.act(h, &turn)

			// Assert
			blocked := h.view(t).GetStrip().GetStatus().GetBlocked()
			if blocked.GetQueryDied() == nil {
				t.Fatalf("substatus = %+v, want query_died", blocked.GetSubstatus())
			}
		})
	}
}

// TestAQueryDiedTerminalStandsTheDeadQueryLine: the terminal is the death too,
// so the strip carries the dead-query line before the push lands.
func TestAQueryDiedTerminalStandsTheDeadQueryLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, nil, queryDiedFailure())

	// Assert
	blocked := h.view(t).GetStrip().GetStatus().GetBlocked()
	if blocked.GetActivity().GetQueryDied().GetText() == "" {
		t.Fatalf("the dead-query line is missing: activity = %+v", blocked.GetActivity())
	}
}

func TestAnAuthFailureBlocksOnAuth(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ApiRequestFailed{
			ApiRequestFailed: &conversationv1.ApiRequestFailed{
				Message: "credential rejected",
				Kind: &conversationv1.ApiRequestFailed_AuthenticationFailed{
					AuthenticationFailed: &conversationv1.ApiAuthenticationFailed{},
				},
			},
		},
	})

	// Assert
	if h.view(t).GetStrip().GetStatus().GetBlocked().GetAuth() == nil {
		t.Fatalf("want blocked · auth from an authentication failure")
	}
}

func TestABillingFailureBlocksOnBilling(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ApiRequestFailed{
			ApiRequestFailed: &conversationv1.ApiRequestFailed{
				Kind: &conversationv1.ApiRequestFailed_BillingError{
					BillingError: &conversationv1.ApiBillingError{},
				},
			},
		},
	})

	// Assert
	if h.view(t).GetStrip().GetStatus().GetBlocked().GetBilling() == nil {
		t.Fatalf("want blocked · billing")
	}
}

func TestABlockingLimitBlocksOnUsage(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_BlockingLimit{
			BlockingLimit: &conversationv1.AgentStoppedAtBlockingLimit{},
		},
	})

	// Assert
	if h.view(t).GetStrip().GetStatus().GetBlocked().GetUsageLimit() == nil {
		t.Fatalf("want blocked · usage_limit")
	}
}

func TestAnUnclassifiedFailureBlocksOnVendorError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ExecutionError{
			ExecutionError: &conversationv1.AgentExecutionError{},
		},
	})

	// Assert
	if h.view(t).GetStrip().GetStatus().GetBlocked().GetVendorError() == nil {
		t.Fatalf("want blocked · vendor_error")
	}
}

func TestANewTurnClearsAStandingBlock(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_QueryDied{QueryDied: &conversationv1.SessionQueryDied{}},
	})

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Assert
	if got := h.status(t); got != "thinking" {
		t.Fatalf("status = %q, want thinking: the next prompt restarts the query", got)
	}
}

func TestADialingLinkIsDisconnectedStarting(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.r.OnLink(testWS, shimclient.LinkDialing)

	// Assert
	if h.view(t).GetStrip().GetStatus().GetDisconnected().GetStarting() == nil {
		t.Fatalf("want disconnected · starting")
	}
}

func TestARedialingLinkIsSevered(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.r.OnLink(testWS, shimclient.LinkRedialing)

	// Assert
	if h.view(t).GetStrip().GetStatus().GetDisconnected().GetSevered() == nil {
		t.Fatalf("want disconnected · severed")
	}
}

func TestADeadLinkThatNeverConnectedIsAStartFailure(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.r.OnLink(testWS, shimclient.LinkDead)

	// Assert
	if h.view(t).GetStrip().GetStatus().GetDisconnected().GetStartFailed() == nil {
		t.Fatalf("want disconnected · start_failed for a shim that never served")
	}
}

func TestADeadLinkThatOnceConnectedIsDead(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnLink(testWS, shimclient.LinkDead)

	// Assert
	if h.view(t).GetStrip().GetStatus().GetDisconnected().GetDead() == nil {
		t.Fatalf("want disconnected · dead for a shim that had served")
	}
}

func TestAnOpenDegradedWindowDrawsAServingLinkAsDegraded(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Diagnostics{
			Diagnostics: &conversationv1.SessionDiagnostics{
				DegradedWindows: []*conversationv1.SessionDegradedWindow{{
					Component: "converter",
					Extent:    &conversationv1.SessionDegradedWindow_Open{Open: &conversationv1.SessionDegradedOpen{}},
				}},
			},
		},
	})

	// Assert
	if h.view(t).GetStrip().GetStatus().GetDisconnected().GetDegraded() == nil {
		t.Fatalf("want disconnected · degraded while a window is open")
	}
}

func TestDisconnectedOutranksEveryOtherStatus(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.SetMerge(testWS, MergeFacts{State: "merging"})

	// Act
	h.r.OnLink(testWS, shimclient.LinkDead)

	// Assert
	if got := h.status(t); got != "disconnected" {
		t.Fatalf("status = %q, want disconnected", got)
	}
}

func TestABlockedCloseComposesItsReasons(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetClosing(testWS, &CloseBlocked{
		Reason: "live_work",
		Detail: "a turn is in flight; 2 subagents and a shell are running",
	})

	// Assert
	closing := h.view(t).GetStrip().GetStatus().GetClosing()
	if closing.GetBlocked() == nil {
		t.Fatalf("substatus = %+v, want blocked", closing.GetSubstatus())
	}
	got := closing.GetActivity().GetCloseBlocked().GetText()
	if got != "a turn is in flight; 2 subagents and a shell are running" {
		t.Fatalf("close-blocked text = %q, want the composed reasons", got)
	}
}

func TestClearingTheCloseRefusalLeavesClosing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetClosing(testWS, &CloseBlocked{Reason: "live_work", Detail: "work is live"})

	// Act
	h.r.SetClosing(testWS, nil)

	// Assert
	if got := h.status(t); got == "closing" {
		t.Fatalf("status = %q, want the close refusal cleared", got)
	}
}

func TestEveryFooterStatusArmIsPaintedByTheVocabulary(t *testing.T) {
	// Arrange: the arm names this resolver can emit, which the render-colors
	// footer_status table must cover row for row.
	arms := []string{
		"idle", "thinking", "waiting", "interrupted", "merging",
		"background", "blocked", "disconnected", "closing", "loading",
	}
	emitted := map[string]bool{}
	for _, arm := range arms {
		emitted[arm] = true
	}

	// Act
	var missing []string
	for _, arm := range statusArmsFromProto() {
		if !emitted[arm] {
			missing = append(missing, arm)
		}
	}

	// Assert
	if len(missing) > 0 {
		t.Fatalf("arms %v exist in the contract but this resolver never emits them", missing)
	}
}

// statusArmsFromProto lists the FooterStatus arms the generated code declares,
// so an arm landing without a resolver branch fails here rather than drawing
// nothing.
func statusArmsFromProto() []string {
	probes := []*frontendv1.FooterStatus{
		{Status: &frontendv1.FooterStatus_Idle{}},
		{Status: &frontendv1.FooterStatus_Thinking{}},
		{Status: &frontendv1.FooterStatus_Waiting{}},
		{Status: &frontendv1.FooterStatus_Interrupted{}},
		{Status: &frontendv1.FooterStatus_Merging{}},
		{Status: &frontendv1.FooterStatus_Background{}},
		{Status: &frontendv1.FooterStatus_Blocked{}},
		{Status: &frontendv1.FooterStatus_Disconnected{}},
		{Status: &frontendv1.FooterStatus_Closing{}},
		{Status: &frontendv1.FooterStatus_Loading{}},
	}
	out := make([]string, 0, len(probes))
	for _, probe := range probes {
		out = append(out, statusName(probe))
	}
	return out
}

func TestAServingLinkWithNoWebStreamIsDisconnected(t *testing.T) {
	// Arrange: the daemon-to-shim hop serves, the web hop does not.
	h := newHarness(t)
	h.r.SetParticipants(testWS, true, false)

	// Act
	h.r.OnLink(testWS, shimclient.LinkConnected)

	// Assert
	if got := h.status(t); got != "disconnected" {
		t.Fatalf("status = %q, want disconnected: the workspace is connected only while all three hops are live", got)
	}
}

func TestAServingLinkWithNoHostStreamIsDisconnected(t *testing.T) {
	// Arrange: the daemon-to-shim hop serves, the host hop does not.
	h := newHarness(t)
	h.r.SetParticipants(testWS, false, true)

	// Act
	h.r.OnLink(testWS, shimclient.LinkConnected)

	// Assert
	if got := h.status(t); got != "disconnected" {
		t.Fatalf("status = %q, want disconnected: the workspace is connected only while all three hops are live", got)
	}
}

func TestADownPeerHopIsDrawnAsSevered(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetParticipants(testWS, true, false)

	// Act
	h.r.OnLink(testWS, shimclient.LinkConnected)

	// Assert
	arm := h.view(t).GetStrip().GetStatus().GetDisconnected()
	if arm.GetSevered() == nil {
		t.Fatalf("substatus = %+v, want severed: the route to a reader is broken", arm.GetSubstatus())
	}
}

func TestTheLastPeerHopComingUpMakesTheWorkspaceConnected(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetParticipants(testWS, true, false)
	h.r.OnLink(testWS, shimclient.LinkConnected)

	// Act
	h.r.SetParticipants(testWS, true, true)

	// Assert
	if got := h.status(t); got == "disconnected" {
		t.Fatal("status = disconnected after every hop came up, want a connected status")
	}
}

func TestAPeerHopGoingDownAgainDisconnectsTheWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetParticipants(testWS, true, false)

	// Assert
	if got := h.status(t); got != "disconnected" {
		t.Fatalf("status = %q, want disconnected once a hop went back down", got)
	}
}

func TestAPeerHopDownDoesNotOutrankTheShimLinksOwnStep(t *testing.T) {
	// Arrange: a dialing shim link is a more specific truth than a peer hop.
	h := newHarness(t)
	h.r.SetParticipants(testWS, false, false)

	// Act
	h.r.OnLink(testWS, shimclient.LinkDialing)

	// Assert
	arm := h.view(t).GetStrip().GetStatus().GetDisconnected()
	if arm.GetStarting() == nil {
		t.Fatalf("substatus = %+v, want starting", arm.GetSubstatus())
	}
}

func TestAPeerHopDownBeforeAnyLinkIsObservedIsNotDisconnected(t *testing.T) {
	// Arrange: no session has been asked for, so there is no route to report.
	h := newHarness(t)

	// Act
	h.r.SetParticipants(testWS, false, false)

	// Assert
	if got := h.status(t); got == "disconnected" {
		t.Fatal("status = disconnected with no link ever observed, want the no-session statuses")
	}
}

// TestStatusArmsCoverTheProtoOneof pins the hardcoded arm list against the
// FooterStatus.status oneof itself, so an arm landing in the proto cannot be
// left out of the list and silently escape the render-colors assertion.
func TestStatusArmsCoverTheProtoOneof(t *testing.T) {
	// Arrange.
	arms, err := vocab.OneofArmNames((&frontendv1.FooterStatus{}).ProtoReflect().Descriptor(), "status")
	if err != nil {
		t.Fatalf("OneofArmNames: %v", err)
	}

	// Act.
	got := append([]string(nil), statusArms...)
	sort.Strings(got)
	sort.Strings(arms)

	// Assert.
	if !slices.Equal(got, arms) {
		t.Fatalf("statusArms = %v, want the FooterStatus.status arms %v", got, arms)
	}
}

// TestAllowanceArmsCoverTheProtoOneof pins the same for the allowance list.
func TestAllowanceArmsCoverTheProtoOneof(t *testing.T) {
	// Arrange.
	arms, err := vocab.OneofArmNames((&frontendv1.FooterAllowance{}).ProtoReflect().Descriptor(), "status")
	if err != nil {
		t.Fatalf("OneofArmNames: %v", err)
	}

	// Act.
	got := append([]string(nil), allowanceArms...)
	sort.Strings(got)
	sort.Strings(arms)

	// Assert.
	if !slices.Equal(got, arms) {
		t.Fatalf("allowanceArms = %v, want the FooterAllowance.status arms %v", got, arms)
	}
}

// THE VENDOR ANNOUNCES A COMPACTION BEFORE THE TURN THAT RUNS IT. The
// compacting signal and the turn-open edge arrive within a millisecond of each
// other and the signal wins the race, so a turn opening must not wipe it.
func TestCompactingAnnouncedBeforeTheTurnOpensSurvivesTheTurnOpen(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Compacting{Compacting: &conversationv1.SessionCompacting{}},
	})

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

	// Assert
	if h.view(t).GetStrip().GetStatus().GetThinking().GetCompacting() == nil {
		t.Fatalf("want thinking · compacting: the turn-open edge must not wipe the vendor's announcement")
	}
}

// The context cut is the compaction's ONLY end signal.
func TestTheContextCutClearsCompacting(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Compacting{Compacting: &conversationv1.SessionCompacting{}},
	})
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

	// Act
	h.r.OnContextCut(testWS, mainAgent, &conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_Compacted{Compacted: &conversationv1.ContextCompacted{}},
	})
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

	// Assert
	if h.view(t).GetStrip().GetStatus().GetThinking().GetCompacting() != nil {
		t.Fatalf("want no compacting sub-status once the cut ended the compaction")
	}
}

// TestAParkedSessionWithADeadLinkIsIdleAndNeverDisconnected is the footer half
// of "A PARKED SESSION IS IDLE, NOT BROKEN" (resolve/sidebar/status.go), whose
// `linkArm` promises to mirror the disconnected step below fact for fact.
//
// Measured in a headless run of the real editor (05-tab-arms-lifecycle/
// 15-arm-hibernated): the strip read `disconnected · dead` after the idle
// sweep parked a settled session, and because the webapp's composer gate IS
// that word (webapp/src/main.ts — a `disconnected` status closes the composer)
// the page could not submit the prompt that revives the session.
func TestAParkedSessionWithADeadLinkIsIdleAndNeverDisconnected(t *testing.T) {
	// Arrange: a session that served, then the sweep's stand-down — the link
	// dies inside KillSession and the park record lands after it.
	h := newHarness(t)
	connected(h)
	h.r.OnLink(testWS, shimclient.LinkDead)

	// Act
	h.r.SetParked(testWS, true)

	// Assert
	if got := h.status(t); got != "idle" {
		t.Fatalf("status = %q, want idle: the daemon put this route down itself", got)
	}
}

// TestAParkThatWasRevivedNoLongerMasksARealDeath is the park's release. The
// session watcher latches a dead link and publishes nothing further on it, so
// the next link state a workspace sees belongs to the shim the reviving prompt
// spawned — after which an ordinary death must read `dead` again rather than
// hiding behind a park nothing lifted.
func TestAParkThatWasRevivedNoLongerMasksARealDeath(t *testing.T) {
	// Arrange: parked, then revived — the revival's own link attaches.
	h := newHarness(t)
	connected(h)
	h.r.OnLink(testWS, shimclient.LinkDead)
	h.r.SetParked(testWS, true)
	connected(h)

	// Act: the revived shim dies for real.
	h.r.OnLink(testWS, shimclient.LinkDead)

	// Assert
	if h.view(t).GetStrip().GetStatus().GetDisconnected().GetDead() == nil {
		t.Fatalf("want disconnected · dead: the park was lifted by the revival")
	}
}

// TestAShimsDeathEndsTheTurnTheStripDrew covers the turn a dead shim leaves
// behind: no terminal is coming for it, so the link coming back (the revived
// session) is not drawn as that turn still thinking. A severed link whose shim
// lives on keeps its turn.
func TestAShimsDeathEndsTheTurnTheStripDrew(t *testing.T) {
	tests := []struct {
		name         string
		lost         shimclient.LinkState
		wantThinking bool
	}{
		{name: "a dead link ends the turn", lost: shimclient.LinkDead},
		{name: "a severed link keeps the turn", lost: shimclient.LinkRedialing, wantThinking: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
			h.r.OnLink(testWS, tt.lost)

			// Act
			h.r.OnLink(testWS, shimclient.LinkConnected)

			// Assert
			if got := h.view(t).GetStrip().GetStatus().GetThinking() != nil; got != tt.wantThinking {
				t.Fatalf("thinking = %v, want %v; status %v", got, tt.wantThinking, h.view(t).GetStrip().GetStatus())
			}
		})
	}
}
