package footer

import (
	"slices"
	"sort"
	"testing"
	"time"

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
	got := h.view(t).GetStrip().GetStatus().GetMergeConflict().GetParked().GetLine()
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

// A DEAD QUERY IS A FAILED TURN, NOT A BLOCK (owner ruling, 2026-09-28):
// `blocked` is only for the vendor or the account, and the next prompt
// restarts a dead query.

func TestAQueryDeathFailsTheTurnItCut(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_QueryDied{
			QueryDied: &conversationv1.SessionQueryDied{},
		},
	})

	// Assert
	if h.view(t).GetStrip().GetStatus().GetIdle().GetTurnFailed() == nil {
		t.Fatalf("status = %q, want idle · turn_failed", h.status(t))
	}
}

func TestAQueryDeathWithNoTurnDoesNotBlock(t *testing.T) {
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
	if got := h.status(t); got != "idle" {
		t.Fatalf("status = %q, want idle", got)
	}
}

func TestAQueryDeathStandsItsLineUnderTheFailedTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_QueryDied{
			QueryDied: &conversationv1.SessionQueryDied{},
		},
	})

	// Assert
	if h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetQueryDied().GetText() == "" {
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
	idle := h.view(t).GetStrip().GetStatus().GetIdle()
	if idle.GetActivity().GetQueryDied().GetText() == "" {
		t.Fatalf("the dead-query line is missing: activity = %+v", idle.GetActivity())
	}
}

// queryDiedFailure is the terminal the shim owes the open turn when its query
// dies: AgentFailure's own query_died arm.
func queryDiedFailure() *conversationv1.AgentFailure {
	return &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_QueryDied{QueryDied: &conversationv1.SessionQueryDied{}},
	}
}

// TestAQueryDeathFailsTheTurnWhicheverStatementArrivesFirst: the session's
// query_died push and the turn's query_died terminal travel by independent
// channels, so the turn reads failed in either order.
func TestAQueryDeathFailsTheTurnWhicheverStatementArrivesFirst(t *testing.T) {
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
			if h.view(t).GetStrip().GetStatus().GetIdle().GetTurnFailed() == nil {
				t.Fatalf("status = %q, want idle · turn_failed", h.status(t))
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
	idle := h.view(t).GetStrip().GetStatus().GetIdle()
	if idle.GetActivity().GetQueryDied().GetText() == "" {
		t.Fatalf("the dead-query line is missing: activity = %+v", idle.GetActivity())
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

func TestAModelErrorBlocksOnVendorError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ModelError{ModelError: &conversationv1.AgentModelError{}},
	})

	// Assert
	if h.view(t).GetStrip().GetStatus().GetBlocked().GetVendorError() == nil {
		t.Fatalf("want blocked · vendor_error")
	}
}

// TestEveryAgentFailureArmTakesItsClassifiedStatus walks every AgentFailure
// arm (owner ruling, 2026-09-28): the vendor's or the account's block, the
// turn's own failure, or an expected stop that reads as a completion.
func TestEveryAgentFailureArmTakesItsClassifiedStatus(t *testing.T) {
	cases := []struct {
		name    string
		failure *conversationv1.AgentFailure
		want    string
	}{
		{name: "api_request_failed", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: &conversationv1.ApiRequestFailed{}}}, want: "blocked"},
		{name: "blocking_limit", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_BlockingLimit{BlockingLimit: &conversationv1.AgentStoppedAtBlockingLimit{}}}, want: "blocked"},
		{name: "rapid_refill_breaker", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_RapidRefillBreaker{RapidRefillBreaker: &conversationv1.AgentStoppedByRapidRefillBreaker{}}}, want: "blocked"},
		{name: "model_error", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ModelError{ModelError: &conversationv1.AgentModelError{}}}, want: "blocked"},
		{name: "prompt_too_long", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_PromptTooLong{PromptTooLong: &conversationv1.AgentPromptTooLong{}}}, want: "turn_failed"},
		{name: "image_error", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ImageError{ImageError: &conversationv1.AgentImageRejected{}}}, want: "turn_failed"},
		{name: "malformed_tool_use_exhausted", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_MalformedToolUseExhausted{MalformedToolUseExhausted: &conversationv1.AgentMalformedToolUseExhausted{}}}, want: "turn_failed"},
		{name: "stop_hook_prevented", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_StopHookPrevented{StopHookPrevented: &conversationv1.AgentStoppedByStopHook{}}}, want: "done"},
		{name: "hook_stopped", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_HookStopped{HookStopped: &conversationv1.AgentStoppedByHook{}}}, want: "turn_failed"},
		{name: "tool_deferred", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ToolDeferred{ToolDeferred: &conversationv1.AgentToolDeferred{}}}, want: "done"},
		{name: "tool_deferred_unavailable", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ToolDeferredUnavailable{ToolDeferredUnavailable: &conversationv1.AgentToolDeferredUnavailable{}}}, want: "turn_failed"},
		{name: "max_turns", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_MaxTurns{MaxTurns: &conversationv1.AgentMaxTurnsReached{}}}, want: "turn_failed"},
		{name: "budget_exhausted", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_BudgetExhausted{BudgetExhausted: &conversationv1.AgentBudgetExhausted{}}}, want: "turn_failed"},
		{name: "structured_output_retry_exhausted", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_StructuredOutputRetryExhausted{StructuredOutputRetryExhausted: &conversationv1.AgentStructuredOutputRetriesExhausted{}}}, want: "turn_failed"},
		{name: "turn_setup_failed", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_TurnSetupFailed{TurnSetupFailed: &conversationv1.AgentTurnSetupFailed{}}}, want: "turn_failed"},
		{name: "execution_error", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ExecutionError{ExecutionError: &conversationv1.AgentExecutionError{}}}, want: "turn_failed"},
		{name: "continuation_prevented", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ContinuationPrevented{ContinuationPrevented: &conversationv1.AgentContinuationPrevented{}}}, want: "turn_failed"},
		{name: "lost", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_Lost{Lost: &conversationv1.DetachedLost{}}}, want: "turn_failed"},
		{name: "query_died", failure: queryDiedFailure(), want: "turn_failed"},
		{name: "an unset arm", failure: &conversationv1.AgentFailure{}, want: "turn_failed"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			turn := testTurnID
			h.r.SetTurn(testWS, &TurnStarted{At: instant})

			// Act
			h.r.OnAgentTerminal(testWS, mainAgent, &turn, nil, tc.failure)

			// Assert
			status := h.view(t).GetStrip().GetStatus()
			got := h.status(t)
			switch {
			case status.GetIdle().GetTurnFailed() != nil:
				got = "turn_failed"
			case status.GetIdle().GetDone() != nil:
				got = "done"
			}
			if got != tc.want {
				t.Fatalf("status = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestARejectedRateLimitBlocksTheSession(t *testing.T) {
	// Arrange: a rejected verdict is the account refusing the session, which
	// the roster draws vendor_blocked on the same event.
	h := newHarness(t)
	connected(h)
	update := rateLimitStatus(fiveHourWindow(), 100, 5*time.Hour)
	update.GetRateLimitStatus().Status = &conversationv1.SessionRateLimitStatus_Rejected{
		Rejected: &conversationv1.SessionRateLimitRejected{},
	}

	// Act
	h.r.OnSessionUpdate(testWS, update)

	// Assert
	if h.view(t).GetStrip().GetStatus().GetBlocked().GetUsageLimit() == nil {
		t.Fatalf("status = %q, want blocked · usage_limit", h.status(t))
	}
}

func TestAnAllowedRateLimitLiftsTheBlock(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	rejected := rateLimitStatus(fiveHourWindow(), 100, 5*time.Hour)
	rejected.GetRateLimitStatus().Status = &conversationv1.SessionRateLimitStatus_Rejected{
		Rejected: &conversationv1.SessionRateLimitRejected{},
	}
	h.r.OnSessionUpdate(testWS, rejected)

	// Act
	h.r.OnSessionUpdate(testWS, rateLimitStatus(fiveHourWindow(), 10, 5*time.Hour))

	// Assert
	if got := h.status(t); got != "idle" {
		t.Fatalf("status = %q, want idle", got)
	}
}

func TestASessionStartLiftsAVendorBlock(t *testing.T) {
	// Arrange: the roster lifts vendor_blocked on the same event.
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, nil, authFailure())

	// Act
	h.r.OnSessionStarted(testWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-2"})

	// Assert
	if got := h.status(t); got != "idle" {
		t.Fatalf("status = %q, want idle", got)
	}
}

func TestANewTurnClearsAStandingBlock(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, nil, authFailure())

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Assert
	if got := h.status(t); got != "thinking" {
		t.Fatalf("status = %q, want thinking: a new turn lifts the block", got)
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

func TestDisconnectedOutranksATerminalMergeAndATurn(t *testing.T) {
	// Arrange: a failed merge is over, so a broken route outranks it (the one
	// ladder, resolve/ladder).
	h := newHarness(t)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.SetMerge(testWS, MergeFacts{State: "failed"})

	// Act
	h.r.OnLink(testWS, shimclient.LinkDead)

	// Assert
	if got := h.status(t); got != "disconnected" {
		t.Fatalf("status = %q, want disconnected", got)
	}
}

func TestATurnAcceptedBeforeAnyLinkAwaitsTheBringUp(t *testing.T) {
	// Arrange: no link state has ever been seen.
	h := newHarness(t)
	h.r.SetParticipants(testWS, true, true)

	// Act: the daemon accepts a prompt before the session is spawned.
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

	// Assert: the route is coming up, which the roster draws `init` from the
	// same two facts (ladder.AwaitingBringUp).
	if h.view(t).GetStrip().GetStatus().GetDisconnected().GetStarting() == nil {
		t.Fatalf("status = %q, want disconnected · starting", h.status(t))
	}
}

func TestASessionAnnouncedBeforeAnyLinkAwaitsTheBringUp(t *testing.T) {
	// Arrange: no link state has ever been seen.
	h := newHarness(t)
	h.r.SetParticipants(testWS, true, true)

	// Act
	h.r.OnSessionStarted(testWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-1"})

	// Assert: the roster draws this window `init` from its own started fact.
	if h.view(t).GetStrip().GetStatus().GetDisconnected().GetStarting() == nil {
		t.Fatalf("status = %q, want disconnected · starting", h.status(t))
	}
}

func TestAMergeInFlightOutranksDisconnected(t *testing.T) {
	// Arrange: a merge in flight is the daemon's own fact, knowable whatever
	// the route does, so it outranks the link (the one ladder, resolve/ladder).
	h := newHarness(t)
	h.r.SetMerge(testWS, MergeFacts{State: "merging"})

	// Act
	h.r.OnLink(testWS, shimclient.LinkDead)

	// Assert
	if got := h.status(t); got != "merging" {
		t.Fatalf("status = %q, want merging", got)
	}
}

// TestAStoppedMergeIsNeverMerging pins the owner's ruling of 2026-09-28: a
// merge that stopped is its own arm, never a `merging` step, so it can close
// no composer.
func TestAStoppedMergeIsNeverMerging(t *testing.T) {
	cases := []struct {
		state string
		want  string
	}{
		{state: "conflict", want: "merge_conflict"},
		{state: "parked", want: "merge_conflict"},
		{state: "failed", want: "merge_failed"},
		{state: "merged", want: "merged"},
	}
	for _, tc := range cases {
		t.Run(tc.state, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			h.r.SetMerge(testWS, MergeFacts{State: tc.state})

			// Assert
			if got := h.status(t); got != tc.want {
				t.Fatalf("status = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestAMergeStoppedOnAConflictHasNoFinerStep(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetMerge(testWS, MergeFacts{State: "conflict"})

	// Assert
	if sub := h.view(t).GetStrip().GetStatus().GetMergeConflict().GetSubstatus(); sub != nil {
		t.Fatalf("substatus = %+v, want unset: the conflict's name is the whole fact", sub)
	}
}

func TestAFailedMergeOutranksATurnInFlight(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.SetMerge(testWS, MergeFacts{State: "failed"})

	// Assert
	if got := h.status(t); got != "merge_failed" {
		t.Fatalf("status = %q, want merge_failed", got)
	}
}

func TestAMergeInFlightOutranksAVendorBlock(t *testing.T) {
	// Arrange: the roster always ranked the merge above vendor_blocked, and
	// the one ladder keeps that order.
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, nil, authFailure())

	// Act
	h.r.SetMerge(testWS, MergeFacts{State: "queued", QueuePosition: 1, QueueDepth: 1})

	// Assert
	if got := h.status(t); got != "merging" {
		t.Fatalf("status = %q, want merging", got)
	}
}

func TestAMergeInFlightOutranksTheMomentaryInterrupted(t *testing.T) {
	// Arrange: the momentary interrupted is a turn END, so it sits in the idle
	// rung under every claim above it.
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.SetMerge(testWS, MergeFacts{State: "enqueuing"})

	// Act
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, interruptedByUserStop(), nil)

	// Assert
	if got := h.status(t); got != "merging" {
		t.Fatalf("status = %q, want merging", got)
	}
}

func TestAParkedSessionIsNeverDisconnectedWhateverItsLink(t *testing.T) {
	// Arrange: the ladder skips the whole link rung for a parked session.
	h := newHarness(t)
	h.r.SetParticipants(testWS, true, true)
	h.r.OnLink(testWS, shimclient.LinkRedialing)

	// Act
	h.r.SetParked(testWS, true)

	// Assert
	if got := h.status(t); got != "idle" {
		t.Fatalf("status = %q, want idle", got)
	}
}

func TestAVendorCompactionBeforeItsTurnIsThinking(t *testing.T) {
	// Arrange: the roster draws `compacting` from this same fact the moment it
	// lands, so the strip must not read idle beside it.
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Compacting{Compacting: &conversationv1.SessionCompacting{}},
	})

	// Assert
	if h.view(t).GetStrip().GetStatus().GetThinking().GetCompacting() == nil {
		t.Fatalf("status = %q, want thinking · compacting", h.status(t))
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
		"merge_conflict", "merge_failed", "merged",
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
		{Status: &frontendv1.FooterStatus_MergeConflict{}},
		{Status: &frontendv1.FooterStatus_MergeFailed{}},
		{Status: &frontendv1.FooterStatus_Merged{}},
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

// authFailure is a vendor refusal: the account's credentials were refused.
func authFailure() *conversationv1.AgentFailure {
	return &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ApiRequestFailed{
			ApiRequestFailed: &conversationv1.ApiRequestFailed{
				Kind: &conversationv1.ApiRequestFailed_AuthenticationFailed{
					AuthenticationFailed: &conversationv1.ApiAuthenticationFailed{}},
			},
		},
	}
}
