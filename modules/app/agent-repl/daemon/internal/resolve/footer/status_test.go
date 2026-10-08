package footer

import (
	"slices"
	"sort"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/turnfault"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/vocab"
	"claude-repld/internal/wsm"
)

// connected puts a serving link under the workspace so the disconnected arm —
// which outranks everything — does not mask the arm under test.
// connected puts ALL THREE hops of connectivity truth up: the daemon-to-shim
// link and both client streams (daemon.md invariant 11). A test that wants one
// hop down states that hop itself.
func connected(h *harness) {
	h.r.SetParticipants(testWS, true, true)
	h.r.OnLink(testWS, shimclient.LinkConnected)
	h.r.OnMainAgent(testWS, mainAgent)
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
	if got := h.status(t); got != "working" {
		t.Fatalf("status = %q, want working during the submitting phase", got)
	}
}

func TestThinkingIsSubmittingUntilTheFirstActivity(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

	// Assert
	thinking := h.view(t).GetStrip().GetStatus().GetWorking()
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
	thinking := h.view(t).GetStrip().GetStatus().GetWorking()
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
	if h.view(t).GetStrip().GetStatus().GetWorking().GetClearing() == nil {
		t.Fatalf("want working · clearing for a /clear act")
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
	if h.view(t).GetStrip().GetStatus().GetWorking().GetCompacting() == nil {
		t.Fatalf("want working · compacting from the vendor's compacting signal")
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
	if got := waiting.GetActivity().GetSalient().GetGatedCall().GetText(); got == "" {
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
	got := waiting.GetActivity().GetSalient().GetQuestionLead().GetText()
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
	if waiting.GetActivity().GetSalient().GetInterrupting().GetText() == "" {
		t.Fatalf("the interrupting status carries no composed line")
	}
}

func TestTheColdGateIsAWaitingStep(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetColdGate(testWS, ColdGate{Standing: true, Cost: ColdGateCost{Lead: "context cold — 182k tokens to re-read"}})

	// Assert
	waiting := h.view(t).GetStrip().GetStatus().GetWaiting()
	if waiting.GetColdGate() == nil {
		t.Fatalf("substatus = %+v, want cold_gate", waiting.GetSubstatus())
	}
	if waiting.GetActivity().GetSalient().GetColdGateCost().GetText() == "" {
		t.Fatalf("the cold gate's composed cost line is missing")
	}
}

func TestTheColdGatesCostLineCarriesItsParts(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	cost := ColdGateCost{Lead: "the conversation is cold at ", Figure: "409,051", Tail: " context tokens", WindowFill: 0.41}

	// Act
	h.r.SetColdGate(testWS, ColdGate{Standing: true, Cost: cost})

	// Assert
	line := h.view(t).GetStrip().GetStatus().GetWaiting().GetActivity().GetSalient().GetColdGateCost()
	got := []any{line.GetLead(), line.GetFigure().GetText(), line.GetFigure().GetWindowFill(), line.GetTail()}
	want := []any{"the conversation is cold at ", "409,051", 0.41, " context tokens"}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("cost line parts = %v, want %v", got, want)
		}
	}
}

func TestTheColdGatesCostLineIsExactlyItsParts(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	cost := ColdGateCost{Lead: "the conversation is cold at ", Figure: "409,051", Tail: " context tokens", WindowFill: 0.41}

	// Act
	h.r.SetColdGate(testWS, ColdGate{Standing: true, Cost: cost})

	// Assert
	line := h.view(t).GetStrip().GetStatus().GetWaiting().GetActivity().GetSalient().GetColdGateCost()
	if parts := line.GetLead() + line.GetFigure().GetText() + line.GetTail(); line.GetText() != parts {
		t.Fatalf("text = %q, want lead+figure+tail %q", line.GetText(), parts)
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
	if got := h.status(t); got != "working" {
		t.Fatalf("status = %q, want working: the wakeup fallback shows only where the footer reads idle", got)
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
	if waiting.GetActivity().GetSalient().GetWakeup().GetWakeAtMs() == 0 {
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
	if got := h.status(t); got != "working" {
		t.Fatalf("status = %q, want working", got)
	}
}

// A DEAD QUERY IS AGENT-REPL'S FAULT, NOT A BLOCK (owner rulings, 2026-09-28
// and 2026-10-06): the turn it cut raises `agent_repl_fault · turn_died` from
// its close, and the next prompt restarts a dead query.

// queryDied is the session's query_died push.
func queryDied() *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_QueryDied{QueryDied: &conversationv1.SessionQueryDied{}},
	}
}

// failTurn ends the turn in flight with its terminal's failure, then with the
// close the prompt queue's door reports for it.
func failTurn(h *harness, failure *conversationv1.AgentFailure) {
	turn := testTurnID
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, nil, failure)
	h.r.SetTurnEnded(testWS, wsm.CloseFailed)
}

func TestAQueryDeathRaisesTheTurnDiedFaultFromItsClose(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnSessionUpdate(testWS, queryDied())

	// Act
	h.r.SetTurnEnded(testWS, wsm.CloseFailed)

	// Assert
	if h.view(t).GetStrip().GetStatus().GetAgentReplFault().GetTurnDied() == nil {
		t.Fatalf("status = %q, want agent_repl_fault · turn_died", h.status(t))
	}
}

func TestAQueryDeathRaisesNoFaultBeforeTheTurnsClose(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnSessionUpdate(testWS, queryDied())

	// Assert
	if h.view(t).GetStrip().GetStatus().GetAgentReplFault() != nil {
		t.Fatalf("status = %q, want no fault until the close raises it", h.status(t))
	}
}

func TestAQueryDeathWithNoTurnDoesNotBlock(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnSessionUpdate(testWS, queryDied())

	// Assert
	if got := h.status(t); got != "idle" {
		t.Fatalf("status = %q, want idle", got)
	}
}

func TestAQueryDeathWithNoTurnStandsTheDeadQueryLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnSessionUpdate(testWS, queryDied())

	// Assert
	if h.view(t).GetStrip().GetStatus().GetIdle().GetActivity().GetSalient().GetQueryDied().GetText() == "" {
		t.Fatalf("the dead-query line is missing")
	}
}

func TestATurnDiedFaultCarriesTheQueryDeathsSentence(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnSessionUpdate(testWS, queryDied())

	// Act
	h.r.SetTurnEnded(testWS, wsm.CloseFailed)

	// Assert
	got := h.view(t).GetStrip().GetStatus().GetAgentReplFault().GetActivity().GetSalient().GetTurnEnded().GetText()
	if want := turnfault.OfQueryDeath(&conversationv1.SessionQueryDied{}).Sentence; got != want {
		t.Fatalf("turn_ended line = %q, want %q", got, want)
	}
}

// queryDiedFailure is the terminal the shim owes the open turn when its query
// dies: AgentFailure's own query_died arm.
func queryDiedFailure() *conversationv1.AgentFailure {
	return &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_QueryDied{QueryDied: &conversationv1.SessionQueryDied{}},
	}
}

// TestAQueryDeathRaisesTurnDiedWhicheverStatementArrivesFirst: the session's
// query_died push and the turn's query_died terminal travel by independent
// channels, so the turn's close raises the same fault in either order.
func TestAQueryDeathRaisesTurnDiedWhicheverStatementArrivesFirst(t *testing.T) {
	cases := []struct {
		name string
		act  func(h *harness, turn *ids.TurnID)
	}{
		{name: "the terminal first, then the push", act: func(h *harness, turn *ids.TurnID) {
			h.r.OnAgentTerminal(testWS, mainAgent, turn, nil, queryDiedFailure())
			h.r.OnSessionUpdate(testWS, queryDied())
		}},
		{name: "the push first, then the terminal", act: func(h *harness, turn *ids.TurnID) {
			h.r.OnSessionUpdate(testWS, queryDied())
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
			tc.act(h, &turn)

			// Act
			h.r.SetTurnEnded(testWS, wsm.CloseFailed)

			// Assert
			if h.view(t).GetStrip().GetStatus().GetAgentReplFault().GetTurnDied() == nil {
				t.Fatalf("status = %q, want agent_repl_fault · turn_died", h.status(t))
			}
		})
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
	if h.view(t).GetStrip().GetStatus().GetVendorFault().GetAuth() == nil {
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
	if h.view(t).GetStrip().GetStatus().GetVendorFault().GetBilling() == nil {
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
	if h.view(t).GetStrip().GetStatus().GetVendorFault().GetUsageLimit() == nil {
		t.Fatalf("want blocked · usage_limit")
	}
}

// A model error is TRANSIENT (owner ruling, 2026-09-28): the workspace stays
// usable, so it raises no block, only the vendor turn fault (owner ruling,
// 2026-10-06).
func TestAModelErrorRaisesTheVendorErrorTurnFault(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	failTurn(h, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ModelError{ModelError: &conversationv1.AgentModelError{}},
	})

	// Assert
	if h.view(t).GetStrip().GetStatus().GetVendorFault().GetVendorError() == nil {
		t.Fatalf("status = %q, want vendor_fault · vendor_error", h.status(t))
	}
}

// TestEveryAgentFailureArmTakesItsFaultsStatus walks every AgentFailure arm
// through the turn's close (owner ruling, 2026-10-06): every cause the vendor
// ended or refused is a vendor fault (its own block's step when it blocks the
// session, `vendor_error` otherwise), the query dying is `agent_repl_fault ·
// turn_died`, and an expected stop reads as a completion.
func TestEveryAgentFailureArmTakesItsFaultsStatus(t *testing.T) {
	api := func(kind any) *conversationv1.AgentFailure {
		failed := &conversationv1.ApiRequestFailed{}
		switch k := kind.(type) {
		case *conversationv1.ApiRequestFailed_Overloaded:
			failed.Kind = k
		}
		return &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: failed}}
	}
	cases := []struct {
		name    string
		failure *conversationv1.AgentFailure
		want    string
	}{
		{name: "api_request_failed of an unstated kind", failure: api(nil), want: "vendor_fault·vendor_error"},
		{name: "api_request_failed: authentication", failure: authFailure(), want: "vendor_fault·auth"},
		{name: "api_request_failed: overloaded", failure: api(&conversationv1.ApiRequestFailed_Overloaded{Overloaded: &conversationv1.ApiOverloaded{}}), want: "vendor_fault·vendor_error"},
		{name: "blocking_limit", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_BlockingLimit{BlockingLimit: &conversationv1.AgentStoppedAtBlockingLimit{}}}, want: "vendor_fault·usage_limit"},
		{name: "rapid_refill_breaker", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_RapidRefillBreaker{RapidRefillBreaker: &conversationv1.AgentStoppedByRapidRefillBreaker{}}}, want: "vendor_fault·usage_limit"},
		{name: "model_error", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ModelError{ModelError: &conversationv1.AgentModelError{}}}, want: "vendor_fault·vendor_error"},
		{name: "prompt_too_long", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_PromptTooLong{PromptTooLong: &conversationv1.AgentPromptTooLong{}}}, want: "vendor_fault·vendor_error"},
		{name: "image_error", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ImageError{ImageError: &conversationv1.AgentImageRejected{}}}, want: "vendor_fault·vendor_error"},
		{name: "malformed_tool_use_exhausted", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_MalformedToolUseExhausted{MalformedToolUseExhausted: &conversationv1.AgentMalformedToolUseExhausted{}}}, want: "vendor_fault·vendor_error"},
		{name: "stop_hook_prevented", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_StopHookPrevented{StopHookPrevented: &conversationv1.AgentStoppedByStopHook{}}}, want: "idle·done"},
		{name: "hook_stopped", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_HookStopped{HookStopped: &conversationv1.AgentStoppedByHook{}}}, want: "vendor_fault·vendor_error"},
		{name: "tool_deferred", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ToolDeferred{ToolDeferred: &conversationv1.AgentToolDeferred{}}}, want: "idle·done"},
		{name: "tool_deferred_unavailable", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ToolDeferredUnavailable{ToolDeferredUnavailable: &conversationv1.AgentToolDeferredUnavailable{}}}, want: "vendor_fault·vendor_error"},
		{name: "max_turns", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_MaxTurns{MaxTurns: &conversationv1.AgentMaxTurnsReached{}}}, want: "vendor_fault·vendor_error"},
		{name: "budget_exhausted", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_BudgetExhausted{BudgetExhausted: &conversationv1.AgentBudgetExhausted{}}}, want: "vendor_fault·vendor_error"},
		{name: "structured_output_retry_exhausted", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_StructuredOutputRetryExhausted{StructuredOutputRetryExhausted: &conversationv1.AgentStructuredOutputRetriesExhausted{}}}, want: "vendor_fault·vendor_error"},
		{name: "turn_setup_failed", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_TurnSetupFailed{TurnSetupFailed: &conversationv1.AgentTurnSetupFailed{}}}, want: "vendor_fault·vendor_error"},
		{name: "execution_error", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ExecutionError{ExecutionError: &conversationv1.AgentExecutionError{}}}, want: "vendor_fault·vendor_error"},
		{name: "continuation_prevented", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ContinuationPrevented{ContinuationPrevented: &conversationv1.AgentContinuationPrevented{}}}, want: "vendor_fault·vendor_error"},
		{name: "lost", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_Lost{Lost: &conversationv1.DetachedLost{}}}, want: "vendor_fault·vendor_error"},
		{name: "query_died", failure: queryDiedFailure(), want: "agent_repl_fault·turn_died"},
		{name: "an unset arm", failure: &conversationv1.AgentFailure{}, want: "vendor_fault·vendor_error"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			h.r.SetTurn(testWS, &TurnStarted{At: instant})

			// Act
			failTurn(h, tc.failure)

			// Assert
			if got := statusAndStep(h.view(t).GetStrip().GetStatus()); got != tc.want {
				t.Fatalf("status = %q, want %q", got, tc.want)
			}
		})
	}
}

// statusAndStep names a status arm and its substatus arm, "status·step".
func statusAndStep(status *frontendv1.FooterStatus) string {
	m := status.ProtoReflect()
	arm := m.WhichOneof(m.Descriptor().Oneofs().ByName("status"))
	if arm == nil {
		return "unset"
	}
	inner := m.Get(arm).Message()
	sub := inner.Descriptor().Oneofs().ByName("substatus")
	if sub == nil {
		return string(arm.Name())
	}
	step := inner.WhichOneof(sub)
	if step == nil {
		return string(arm.Name())
	}
	return string(arm.Name()) + "·" + string(step.Name())
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
	if h.view(t).GetStrip().GetStatus().GetVendorFault().GetUsageLimit() == nil {
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
	h.r.SetTurnEnded(testWS, wsm.CloseFailed)

	// Act
	h.r.OnSessionStarted(testWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-2"})

	// Assert: the block lifts, and the vendor turn fault the failed turn
	// raised still stands until the next turn, as the roster's does.
	if got := statusAndStep(h.view(t).GetStrip().GetStatus()); got != "vendor_fault·vendor_error" {
		t.Fatalf("status = %q, want vendor_fault·vendor_error", got)
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
	if got := h.status(t); got != "working" {
		t.Fatalf("status = %q, want working: a new turn lifts the block", got)
	}
}

func TestADialingLinkIsDisconnectedStarting(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.r.OnLink(testWS, shimclient.LinkDialing)

	// Assert
	if h.view(t).GetStrip().GetStatus().GetAgentReplFault().GetStarting() == nil {
		t.Fatalf("want disconnected · starting")
	}
}

func TestARedialingLinkIsSevered(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.r.OnLink(testWS, shimclient.LinkRedialing)

	// Assert
	if h.view(t).GetStrip().GetStatus().GetAgentReplFault().GetSevered() == nil {
		t.Fatalf("want disconnected · severed")
	}
}

func TestADeadLinkThatNeverConnectedIsAStartFailure(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.r.OnLink(testWS, shimclient.LinkDead)

	// Assert
	if h.view(t).GetStrip().GetStatus().GetAgentReplFault().GetStartFailed() == nil {
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
	if h.view(t).GetStrip().GetStatus().GetAgentReplFault().GetDead() == nil {
		t.Fatalf("want disconnected · dead for a shim that had served")
	}
}

// A serving link with an open degraded window is USABLE (owner ruling,
// 2026-09-28): the degraded arm, never disconnected.
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
	if h.view(t).GetStrip().GetStatus().GetDegraded().GetObservation() == nil {
		t.Fatalf("status = %q, want degraded · observation while a window is open", h.status(t))
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
	if got := h.status(t); got != "agent_repl_fault" {
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
	if h.view(t).GetStrip().GetStatus().GetAgentReplFault().GetStarting() == nil {
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
	if h.view(t).GetStrip().GetStatus().GetAgentReplFault().GetStarting() == nil {
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
	if h.view(t).GetStrip().GetStatus().GetWorking().GetCompacting() == nil {
		t.Fatalf("status = %q, want working · compacting", h.status(t))
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
	got := closing.GetActivity().GetSalient().GetCloseBlocked().GetText()
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

// statusArmsFromProto lists the FooterStatus arms the generated code declares,
// so an arm landing without a resolver branch fails here rather than drawing
// nothing.
func statusArmsFromProto() []string {
	probes := []*frontendv1.FooterStatus{
		{Status: &frontendv1.FooterStatus_Idle{}},
		{Status: &frontendv1.FooterStatus_Working{}},
		{Status: &frontendv1.FooterStatus_Waiting{}},
		{Status: &frontendv1.FooterStatus_Interrupted{}},
		{Status: &frontendv1.FooterStatus_Merging{}},
		{Status: &frontendv1.FooterStatus_Background{}},
		{Status: &frontendv1.FooterStatus_VendorFault{}},
		{Status: &frontendv1.FooterStatus_AgentReplFault{}},
		{Status: &frontendv1.FooterStatus_Closing{}},
		{Status: &frontendv1.FooterStatus_Loading{}},
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
	if got := h.status(t); got != "agent_repl_fault" {
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
	if got := h.status(t); got != "agent_repl_fault" {
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
	arm := h.view(t).GetStrip().GetStatus().GetAgentReplFault()
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
	if got := h.status(t); got == "agent_repl_fault" {
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
	if got := h.status(t); got != "agent_repl_fault" {
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
	arm := h.view(t).GetStrip().GetStatus().GetAgentReplFault()
	if arm.GetStarting() == nil {
		t.Fatalf("substatus = %+v, want starting", arm.GetSubstatus())
	}
}

func TestNoClientAttachedToAServingLinkIsNotDisconnected(t *testing.T) {
	// Arrange: the editor released both of the workspace's streams.
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetParticipants(testWS, false, false)

	// Assert
	if got := h.status(t); got == "agent_repl_fault" {
		t.Fatal("status = agent_repl_fault with no client attached, want the session's own status")
	}
}

func TestAPeerHopDownBeforeAnyLinkIsObservedIsNotDisconnected(t *testing.T) {
	// Arrange: no session has been asked for, so there is no route to report.
	h := newHarness(t)

	// Act
	h.r.SetParticipants(testWS, false, false)

	// Assert
	if got := h.status(t); got == "agent_repl_fault" {
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
	if h.view(t).GetStrip().GetStatus().GetWorking().GetCompacting() == nil {
		t.Fatalf("want working · compacting: the turn-open edge must not wipe the vendor's announcement")
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
	if h.view(t).GetStrip().GetStatus().GetWorking().GetCompacting() != nil {
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
	if h.view(t).GetStrip().GetStatus().GetAgentReplFault().GetDead() == nil {
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
			if got := h.view(t).GetStrip().GetStatus().GetWorking() != nil; got != tt.wantThinking {
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

func TestATakenBackShimsUnreportedStateDrawsDegraded(t *testing.T) {
	tests := []struct {
		name    string
		act     func(h *harness)
		want    string
		wantArm bool
	}{
		{name: "the unreported state stands", act: func(h *harness) {
			h.r.SetStateUnreported(testWS, true)
		}, want: "degraded", wantArm: true},
		{name: "a session start lifts it", act: func(h *harness) {
			h.r.SetStateUnreported(testWS, true)
			h.r.OnSessionStarted(testWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-2"})
		}, want: "idle"},
		{name: "an explicit lift lifts it", act: func(h *harness) {
			h.r.SetStateUnreported(testWS, true)
			h.r.SetStateUnreported(testWS, false)
		}, want: "idle"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			tc.act(h)

			// Assert
			if got := h.status(t); got != tc.want {
				t.Fatalf("status = %q, want %q", got, tc.want)
			}
			if got := h.view(t).GetStrip().GetStatus().GetDegraded().GetStateUnreported() != nil; got != tc.wantArm {
				t.Fatalf("degraded · state_unreported = %v, want %v", got, tc.wantArm)
			}
		})
	}
}

func TestAnUnreportedStateOutranksAnObservationWindow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
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

	// Act
	h.r.SetStateUnreported(testWS, true)

	// Assert
	if h.view(t).GetStrip().GetStatus().GetDegraded().GetStateUnreported() == nil {
		t.Fatalf("status = %q, want degraded · state_unreported", h.status(t))
	}
}

// TestEveryWaitingLineStandsFromWhenItsConditionOpened: the salient line's
// instant is the condition's, so a later render does not re-stamp it.
func TestEveryWaitingLineStandsFromWhenItsConditionOpened(t *testing.T) {
	tests := []struct {
		name string
		open func(h *harness)
	}{
		{name: "a question batch", open: func(h *harness) { h.r.OnQuestion(testWS, mainAgent, questionStart("q-1", "Which?")) }},
		{name: "a cold gate", open: func(h *harness) { h.r.SetColdGate(testWS, ColdGate{Standing: true, Cost: ColdGateCost{Lead: "cold"}}) }},
		{name: "an interrupt", open: func(h *harness) { h.r.SetInterrupting(testWS, true) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			tt.open(h)
			h.clock.Advance(time.Minute)

			// Act
			h.r.SetParked(testWS, false)

			// Assert
			at := h.view(t).GetStrip().GetStatus().GetWaiting().GetActivity().GetSalient().GetAt()
			if at.GetAtMs() != instant.UnixMilli() {
				t.Fatalf("at = %d, want the instant the condition opened", at.GetAtMs())
			}
		})
	}
}

func TestARestatedColdGateKeepsItsInstant(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetColdGate(testWS, ColdGate{Standing: true, Cost: ColdGateCost{Lead: "cold"}})
	h.clock.Advance(time.Minute)

	// Act
	h.r.SetColdGate(testWS, ColdGate{Standing: true, Cost: ColdGateCost{Lead: "still cold"}})

	// Assert
	at := h.view(t).GetStrip().GetStatus().GetWaiting().GetActivity().GetSalient().GetAt()
	if at.GetAtMs() != instant.UnixMilli() {
		t.Fatalf("at = %d, want the instant the gate first opened", at.GetAtMs())
	}
}

func TestTheCloseRefusalStandsFromWhenItWasRefused(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetClosing(testWS, &CloseBlocked{Reason: "turn_in_flight", Detail: "a turn is in flight"})
	h.clock.Advance(time.Minute)

	// Act
	h.r.SetParked(testWS, false)

	// Assert
	at := h.view(t).GetStrip().GetStatus().GetClosing().GetActivity().GetSalient().GetAt()
	if at.GetAtMs() != instant.UnixMilli() {
		t.Fatalf("at = %d, want the instant the close was refused", at.GetAtMs())
	}
}

func TestEveryFooterStatusArmTheContractDeclaresIsOneThisResolverEmits(t *testing.T) {
	// Arrange: the arm names this resolver can emit.
	emitted := map[string]bool{}
	for _, arm := range statusArms {
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

func TestAQueuedMergeOutranksAVendorBlock(t *testing.T) {
	// Arrange: the roster always ranked the merge above vendor_blocked, and
	// the one ladder keeps that order.
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, nil, authFailure())

	// Act
	h.r.SetMerge(testWS, MergeFacts{State: "queued", Step: StepEnqueued, QueuePlace: 1, QueueWaiting: 1})

	// Assert
	if got := h.status(t); got != "merging" {
		t.Fatalf("status = %q, want merging", got)
	}
}

func TestAQueuedMergeOutranksTheMomentaryInterrupted(t *testing.T) {
	// Arrange: the momentary interrupted is a turn END, so it sits in the idle
	// rung under every claim above it.
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.SetMerge(testWS, MergeFacts{State: "queued", Step: StepEnqueued, QueuePlace: 1, QueueWaiting: 1})

	// Act
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, interruptedByUserStop(), nil)

	// Assert
	if got := h.status(t); got != "merging" {
		t.Fatalf("status = %q, want merging", got)
	}
}

func TestAMergeOnAStepOutranksWaiting(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnQuestion(testWS, mainAgent, questionStart("q-1", "which?"))

	// Act
	h.r.SetMerge(testWS, MergeFacts{State: "merging", Step: StepRebasing, Replayed: 1, Total: 2})

	// Assert
	if got := h.status(t); got != "merging" {
		t.Fatalf("status = %q, want merging", got)
	}
}

func TestEachMergeStepDrawsItsOwnSubstatus(t *testing.T) {
	tests := []struct {
		name  string
		facts MergeFacts
		check func(*frontendv1.FooterStatusMerging) bool
	}{
		{"enqueued carries its place among the waiting", MergeFacts{State: "queued", Step: StepEnqueued, QueuePlace: 2, QueueWaiting: 5},
			func(m *frontendv1.FooterStatusMerging) bool {
				return m.GetEnqueued().GetPlace() == 2 && m.GetEnqueued().GetWaiting() == 5
			}},
		{"preprocessing", MergeFacts{State: "merging", Step: StepPreprocessing},
			func(m *frontendv1.FooterStatusMerging) bool { return m.GetPreprocessing() != nil }},
		{"rebasing carries its progress", MergeFacts{State: "merging", Step: StepRebasing, Replayed: 3, Total: 7},
			func(m *frontendv1.FooterStatusMerging) bool {
				return m.GetRebasing().GetReplayed() == 3 && m.GetRebasing().GetTotal() == 7
			}},
		{"conflict resolution", MergeFacts{State: "merging", Step: StepConflictResolution},
			func(m *frontendv1.FooterStatusMerging) bool { return m.GetConflictResolution() != nil }},
		{"testing", MergeFacts{State: "merging", Step: StepTesting},
			func(m *frontendv1.FooterStatusMerging) bool { return m.GetTesting() != nil }},
		{"fixing carries its attempt and the bound", MergeFacts{State: "merging", Step: StepFixing, Attempt: 2, MaxAttempts: 3},
			func(m *frontendv1.FooterStatusMerging) bool {
				return m.GetFixing().GetAttempt() == 2 && m.GetFixing().GetMaxAttempts() == 3
			}},
		{"committing", MergeFacts{State: "merging", Step: StepCommitting},
			func(m *frontendv1.FooterStatusMerging) bool { return m.GetCommitting() != nil }},
		{"updating main", MergeFacts{State: "merging", Step: StepUpdatingMain},
			func(m *frontendv1.FooterStatusMerging) bool { return m.GetUpdatingMain() != nil }},
		{"postprocessing", MergeFacts{State: "merging", Step: StepPostprocessing},
			func(m *frontendv1.FooterStatusMerging) bool { return m.GetPostprocessing() != nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			h.r.SetMerge(testWS, tt.facts)

			// Assert
			merging := h.view(t).GetStrip().GetStatus().GetMerging()
			if !tt.check(merging) {
				t.Fatalf("merging = %+v, want the %s substatus", merging, tt.name)
			}
		})
	}
}

// TestAMergeHeldOnTheUserIsWaitingOnUser covers the merging substatus raised
// when the merge's own agent asks the user: only conflict resolution and
// fixing hand the session a turn that can ask.
func TestAMergeHeldOnTheUserIsWaitingOnUser(t *testing.T) {
	tests := []struct {
		name string
		step MergeStep
		ask  func(h *harness)
		want bool
	}{
		{"a permission during conflict resolution", StepConflictResolution,
			func(h *harness) { h.r.OnPermission(testWS, mainAgent, permissionStart("p-1", "rm -rf build")) }, true},
		{"a question during fixing", StepFixing,
			func(h *harness) { h.r.OnQuestion(testWS, mainAgent, questionStart("q-1", "Which approach?")) }, true},
		{"a permission during testing", StepTesting,
			func(h *harness) { h.r.OnPermission(testWS, mainAgent, permissionStart("p-1", "rm -rf build")) }, false},
		{"no ask during fixing", StepFixing, func(*harness) {}, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			h.r.SetMerge(testWS, MergeFacts{State: "merging", Step: tt.step, Attempt: 1, MaxAttempts: 3})

			// Act
			tt.ask(h)

			// Assert
			got := h.view(t).GetStrip().GetStatus().GetMerging().GetWaitingOnUser() != nil
			if got != tt.want {
				t.Fatalf("waiting on user = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestAMergeWaitingOnAPermissionDrawsTheGatedCall(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetMerge(testWS, MergeFacts{State: "merging", Step: StepConflictResolution})

	// Act
	h.r.OnPermission(testWS, mainAgent, permissionStart("p-1", "rm -rf build"))

	// Assert
	line := h.view(t).GetStrip().GetStatus().GetMerging().GetActivity().GetSalient().GetGatedCall().GetText()
	if line != "Bash: rm -rf build" {
		t.Fatalf("activity = %q, want the gated call", line)
	}
}

func TestAMergeWaitingOnAQuestionDrawsItsLead(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetMerge(testWS, MergeFacts{State: "merging", Step: StepFixing, Attempt: 1, MaxAttempts: 3})

	// Act
	h.r.OnQuestion(testWS, mainAgent, questionStart("q-1", "Which approach?"))

	// Assert
	line := h.view(t).GetStrip().GetStatus().GetMerging().GetActivity().GetSalient().GetQuestionLead().GetText()
	if line != "1 question · Which approach?" {
		t.Fatalf("activity = %q, want the batch's lead", line)
	}
}

func TestAnAnsweredAskReturnsTheMergeToItsStep(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetMerge(testWS, MergeFacts{State: "merging", Step: StepFixing, Attempt: 2, MaxAttempts: 3})
	h.r.OnPermission(testWS, mainAgent, permissionStart("p-1", "rm -rf build"))

	// Act
	h.r.OnPermission(testWS, mainAgent, &conversationv1.AgentPermission{
		Id:     &conversationv1.AgentPermissionId{Value: "p-1"},
		Result: &conversationv1.AgentPermission_Success{Success: &conversationv1.AgentPermissionSuccess{}},
	})

	// Assert
	if got := h.view(t).GetStrip().GetStatus().GetMerging().GetFixing().GetAttempt(); got != 2 {
		t.Fatalf("fixing attempt = %d, want the fixing substatus back at attempt 2", got)
	}
}

func TestAMergeWaitingOnUserKeepsTheMergingClaim(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetMerge(testWS, MergeFacts{State: "merging", Step: StepConflictResolution})

	// Act
	h.r.OnPermission(testWS, mainAgent, permissionStart("p-1", "rm -rf build"))

	// Assert
	if got := h.status(t); got != "merging" {
		t.Fatalf("status = %q, want merging: the ask is a substatus of the merge", got)
	}
}

func TestAMergeInFlightWithNoStepIsRecordedAsAnInvariantViolation(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetMerge(testWS, MergeFacts{State: "merging"})

	// Assert
	if sub := h.view(t).GetStrip().GetStatus().GetMerging().GetSubstatus(); sub != nil {
		t.Fatalf("substatus = %+v, want none drawn for a step nobody named", sub)
	}
	records := recordsOf(h.log.Records(), "daemon.footer.merge_step")
	if len(records) == 0 || records[0].Level != dlog.LevelError || records[0].Context["invariant_violation"] == nil {
		t.Fatalf("merge_step records = %+v, want an ERROR naming the invariant", records)
	}
}

func TestAConcludedMergeIsNeverMerging(t *testing.T) {
	cases := []struct {
		facts MergeFacts
		want  string
	}{
		{facts: MergeFacts{State: "failed", FailedArea: FailedTests}, want: "merge_failed"},
		{facts: MergeFacts{State: "merged"}, want: "merged"},
	}
	for _, tc := range cases {
		t.Run(tc.facts.State, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			h.r.SetMerge(testWS, tc.facts)

			// Assert
			if got := h.status(t); got != tc.want {
				t.Fatalf("status = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestAFailedMergeDrawsTheAreaItFailedIn(t *testing.T) {
	tests := []struct {
		area  MergeFailedArea
		check func(*frontendv1.FooterStatusMergeFailed) bool
	}{
		{FailedConflicts, func(m *frontendv1.FooterStatusMergeFailed) bool { return m.GetConflicts() != nil }},
		{FailedTests, func(m *frontendv1.FooterStatusMergeFailed) bool { return m.GetTests() != nil }},
		{FailedOther, func(m *frontendv1.FooterStatusMergeFailed) bool { return m.GetOther() != nil }},
	}
	for _, tt := range tests {
		t.Run(string(tt.area), func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			h.r.SetMerge(testWS, MergeFacts{State: "failed", FailedArea: tt.area})

			// Assert
			failed := h.view(t).GetStrip().GetStatus().GetMergeFailed()
			if !tt.check(failed) {
				t.Fatalf("merge_failed = %+v, want the %s area", failed, tt.area)
			}
		})
	}
}

func TestAFailedMergeWithNoAreaIsRecordedAsAnInvariantViolation(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetMerge(testWS, MergeFacts{State: "failed"})

	// Assert
	if sub := h.view(t).GetStrip().GetStatus().GetMergeFailed().GetSubstatus(); sub != nil {
		t.Fatalf("substatus = %+v, want none drawn for an area nobody named", sub)
	}
	records := recordsOf(h.log.Records(), "daemon.footer.merge_failed_area")
	if len(records) == 0 || records[0].Level != dlog.LevelError || records[0].Context["invariant_violation"] == nil {
		t.Fatalf("merge_failed_area records = %+v, want an ERROR naming the invariant", records)
	}
}

func TestMergeStepWordsAreThePlainWordsOfEachStep(t *testing.T) {
	tests := []struct {
		step MergeStep
		want string
	}{
		{StepEnqueued, "enqueued"},
		{StepPreprocessing, "preprocessing"},
		{StepRebasing, "rebasing"},
		{StepConflictResolution, "conflict resolution"},
		{StepTesting, "testing"},
		{StepFixing, "fixing"},
		{StepCommitting, "committing"},
		{StepUpdatingMain, "updating main"},
		{StepPostprocessing, "postprocessing"},
	}
	for _, tt := range tests {
		t.Run(string(tt.step), func(t *testing.T) {
			// Act
			got := tt.step.Words()

			// Assert
			if got != tt.want {
				t.Fatalf("Words() = %q, want %q", got, tt.want)
			}
		})
	}
}

// ---- the API-retry block ------------------------------------------------

// enotfound is the vendor's report that it is retrying a call it could not
// send.
func enotfound() *conversationv1.ApiRequestFailed {
	return &conversationv1.ApiRequestFailed{Message: "Can't reach the API server (ENOTFOUND)"}
}

func TestARetriedCallBlocksTheTurnAsApiRetrying(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnApiError(testWS, mainAgent, enotfound())

	// Assert
	if h.view(t).GetStrip().GetStatus().GetVendorFault().GetApiRetrying() == nil {
		t.Fatalf("status = %q, want blocked · api_retrying", h.status(t))
	}
}

func TestARetryWithNoTurnInFlightDoesNotBlock(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnApiError(testWS, mainAgent, enotfound())

	// Assert
	if got := h.status(t); got == "vendor_fault" {
		t.Fatalf("status = blocked with no turn in flight")
	}
}

func TestAPromptOpeningATurnDuringARetryIsWorkingWithNoRetryLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnApiError(testWS, mainAgent, enotfound())

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

	// Assert: the whole strip is the working one — status, step and a
	// cell with no retry line.
	working := h.view(t).GetStrip().GetStatus().GetWorking()
	if working == nil {
		t.Fatalf("status = %q, want working for the prompt that opened the turn", h.status(t))
	}
	if working.GetSubmitting() == nil {
		t.Fatalf("working = %v, want the submitting step", working)
	}
	if line := lineName(t, h); line == "salient.retrying" {
		t.Fatalf("activity = %q, want no retry line while the new turn works", line)
	}
}

func TestTheTurnsTerminalEndsApiRetrying(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnApiError(testWS, mainAgent, enotfound())

	// Act
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: enotfound()},
	})

	// Assert
	if got := h.status(t); got == "vendor_fault" {
		t.Fatalf("status = blocked after the turn's terminal")
	}
}

func TestAVendorBlockOutranksApiRetrying(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnApiError(testWS, mainAgent, enotfound())
	update := rateLimitStatus(fiveHourWindow(), 100, 5*time.Hour)
	update.GetRateLimitStatus().Status = &conversationv1.SessionRateLimitStatus_Rejected{
		Rejected: &conversationv1.SessionRateLimitRejected{},
	}

	// Act
	h.r.OnSessionUpdate(testWS, update)

	// Assert
	if h.view(t).GetStrip().GetStatus().GetVendorFault().GetUsageLimit() == nil {
		t.Fatalf("status = %v, want blocked · usage_limit over the retry", h.view(t).GetStrip().GetStatus())
	}
}
