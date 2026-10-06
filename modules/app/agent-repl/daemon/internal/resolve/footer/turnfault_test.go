package footer

import (
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/resolve/turnfault"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
)

// THE TURN FAULT (owner ruling, 2026-10-06): a failed turn's cause puts its
// fault on the strip, raised from the turn's close, and the activity cell
// carries the per-cause sentence the feed's row carries.

// rateLimited is a turn-ending rate limit, with the vendor's wait when wait > 0.
func rateLimited(wait time.Duration) *conversationv1.AgentFailure {
	limited := &conversationv1.ApiRateLimited{}
	if wait > 0 {
		ms := wait.Milliseconds()
		limited.RetryAfterMs = &ms
	}
	return &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ApiRequestFailed{
		ApiRequestFailed: &conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_RateLimited{RateLimited: limited}},
	}}
}

// overloaded is a turn-ending overload, with the vendor's wait when wait > 0.
func overloaded(wait time.Duration) *conversationv1.AgentFailure {
	arm := &conversationv1.ApiOverloaded{}
	if wait > 0 {
		ms := wait.Milliseconds()
		arm.RetryAfterMs = &ms
	}
	return &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ApiRequestFailed{
		ApiRequestFailed: &conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_Overloaded{Overloaded: arm}},
	}}
}

// maxTurns is the run stopping at its turn ceiling.
func maxTurns() *conversationv1.AgentFailure {
	return &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_MaxTurns{MaxTurns: &conversationv1.AgentMaxTurnsReached{}}}
}

// vendorTurnLine is the vendor fault's turn_ended line, nil when none stands.
func vendorTurnLine(h *harness, t *testing.T) *frontendv1.FooterStatusActivityTurnEnded {
	t.Helper()
	return h.view(t).GetStrip().GetStatus().GetVendorFault().GetActivity().GetSalient().GetTurnEnded()
}

// agentReplTurnLine is the agent-repl fault's turn_ended line.
func agentReplTurnLine(h *harness, t *testing.T) *frontendv1.FooterStatusActivityTurnEnded {
	t.Helper()
	return h.view(t).GetStrip().GetStatus().GetAgentReplFault().GetActivity().GetSalient().GetTurnEnded()
}

func TestAVendorTurnFaultCarriesTheSharedPerCauseSentence(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	failTurn(h, maxTurns())

	// Assert
	got := vendorTurnLine(h, t).GetText()
	if want := turnfault.OfAgentFailure(maxTurns(), false).Sentence; got != want {
		t.Fatalf("turn_ended line = %q, want the shared sentence %q", got, want)
	}
}

func TestAWaitedCauseCountsDownToTheVendorsStatedWait(t *testing.T) {
	cases := []struct {
		name     string
		failure  *conversationv1.AgentFailure
		wantStep string
	}{
		{name: "a rate limit keeps its usage-limit block's step", failure: rateLimited(42 * time.Second), wantStep: "vendor_fault·usage_limit"},
		{name: "an overload is a vendor error", failure: overloaded(42 * time.Second), wantStep: "vendor_fault·vendor_error"},
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
			if got := statusAndStep(h.view(t).GetStrip().GetStatus()); got != tc.wantStep {
				t.Fatalf("status = %q, want %q", got, tc.wantStep)
			}
			if got, want := vendorTurnLine(h, t).GetRetryAt().GetAtMs(), instant.Add(42*time.Second).UnixMilli(); got != want {
				t.Fatalf("retry_at = %d, want %d", got, want)
			}
		})
	}
}

func TestAWaitedCauseThatStatedNoWaitCountsDownToNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	failTurn(h, overloaded(0))

	// Assert
	if line := vendorTurnLine(h, t); line == nil || line.RetryAt != nil {
		t.Fatalf("turn_ended line = %+v, want one with no retry instant", line)
	}
}

func TestAnUnwaitedCauseCarriesNoCountdown(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	failTurn(h, maxTurns())

	// Assert
	if vendorTurnLine(h, t).RetryAt != nil {
		t.Fatalf("a turn limit carries a retry countdown it has no wait for")
	}
}

func TestANewTurnEndsTheTurnFault(t *testing.T) {
	cases := []struct {
		name    string
		failure *conversationv1.AgentFailure
	}{
		{name: "a vendor turn fault", failure: maxTurns()},
		{name: "an agent-repl turn fault", failure: queryDiedFailure()},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			h.r.SetTurn(testWS, &TurnStarted{At: instant})
			failTurn(h, tc.failure)

			// Act
			h.r.SetTurn(testWS, &TurnStarted{At: instant})

			// Assert
			if got := h.status(t); got != "working" {
				t.Fatalf("status = %q, want working once the next turn starts", got)
			}
		})
	}
}

func TestATurnFaultOutlivesASessionStart(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	failTurn(h, queryDiedFailure())

	// Act
	h.r.OnSessionStarted(testWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-1"})

	// Assert
	if h.view(t).GetStrip().GetStatus().GetAgentReplFault().GetTurnDied() == nil {
		t.Fatalf("status = %q, want the turn_died fault standing until the next turn", h.status(t))
	}
}

func TestACloseNoTerminalExplainedRaisesItsOwnFault(t *testing.T) {
	cases := []struct {
		name     string
		how      wsm.TurnClose
		wantStep string
	}{
		{name: "the agent process died", how: wsm.CloseAgentDied, wantStep: "agent_repl_fault·turn_died"},
		{name: "an orphaned turn", how: wsm.CloseOrphaned, wantStep: "vendor_fault·vendor_error"},
		{name: "a failed close with no account", how: wsm.CloseFailed, wantStep: "vendor_fault·vendor_error"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			h.r.SetTurn(testWS, &TurnStarted{At: instant})

			// Act
			h.r.SetTurnEnded(testWS, tc.how)

			// Assert
			status := h.view(t).GetStrip().GetStatus()
			if got := statusAndStep(status); got != tc.wantStep {
				t.Fatalf("status = %q, want %q", got, tc.wantStep)
			}
			want, _ := turnfault.OfClose(tc.how)
			line := vendorTurnLine(h, t)
			if status.GetAgentReplFault() != nil {
				line = agentReplTurnLine(h, t)
			}
			if line.GetText() != want.Sentence {
				t.Fatalf("turn_ended line = %q, want %q", line.GetText(), want.Sentence)
			}
		})
	}
}

func TestACloseThatIsNoFailureRaisesNoFault(t *testing.T) {
	cases := []struct {
		name string
		how  wsm.TurnClose
	}{
		{name: "a completion", how: wsm.CloseCompleted},
		{name: "an interrupt", how: wsm.CloseKilled},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			h.r.SetTurn(testWS, &TurnStarted{At: instant})
			turn := testTurnID
			h.r.OnAgentTerminal(testWS, mainAgent, &turn, &conversationv1.AgentSuccess{}, nil)

			// Act
			h.r.SetTurnEnded(testWS, tc.how)

			// Assert
			if got := h.status(t); got != "idle" {
				t.Fatalf("status = %q, want idle", got)
			}
		})
	}
}

func TestAnExpectedStopRaisesNoFaultOnItsFailedClose(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	failTurn(h, &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_StopHookPrevented{StopHookPrevented: &conversationv1.AgentStoppedByStopHook{}}})

	// Assert
	if got := statusAndStep(h.view(t).GetStrip().GetStatus()); got != "idle·done" {
		t.Fatalf("status = %q, want idle·done", got)
	}
}

func TestACloseThisBuildDoesNotKnowIsAnInvariantViolationAndRaisesNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.SetTurnEnded(testWS, wsm.TurnClose(99))

	// Assert
	if got := h.status(t); got == "vendor_fault" || got == "agent_repl_fault" {
		t.Fatalf("status = %q, want no fault raised for an unknown close", got)
	}
	found := false
	for _, rec := range h.log.Records() {
		if rec.Operation == "daemon.footer.set_turn_ended" && rec.Level == "error" && rec.Context["invariant_violation"] != nil {
			found = true
		}
	}
	if !found {
		t.Fatalf("records = %+v, want the invariant violation recorded at error", h.log.Records())
	}
}

func TestARefusedResponseWordsTheModelErrorAsTheRefusal(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnActivity(testWS, mainAgent, &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "resp-1"},
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{
			Result: &conversationv1.AgentResponse_Failure{Failure: &conversationv1.AgentResponseFailure{
				Reason: &conversationv1.AgentResponseFailureReason{Reason: &conversationv1.AgentResponseFailureReason_Refused{Refused: &conversationv1.AgentResponseRefused{}}},
			}},
		}},
	})
	modelError := &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ModelError{ModelError: &conversationv1.AgentModelError{}}}

	// Act
	failTurn(h, modelError)

	// Assert
	if got, want := vendorTurnLine(h, t).GetText(), turnfault.OfAgentFailure(modelError, true).Sentence; got != want {
		t.Fatalf("turn_ended line = %q, want the refusal's %q", got, want)
	}
}

func TestARefusalDoesNotWordTheNextTurnsModelError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnActivity(testWS, mainAgent, &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "resp-1"},
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{
			Result: &conversationv1.AgentResponse_Failure{Failure: &conversationv1.AgentResponseFailure{
				Reason: &conversationv1.AgentResponseFailureReason{Reason: &conversationv1.AgentResponseFailureReason_Refused{Refused: &conversationv1.AgentResponseRefused{}}},
			}},
		}},
	})
	failTurn(h, maxTurns())
	modelError := &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ModelError{ModelError: &conversationv1.AgentModelError{}}}
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	failTurn(h, modelError)

	// Assert
	if got, want := vendorTurnLine(h, t).GetText(), turnfault.OfAgentFailure(modelError, false).Sentence; got != want {
		t.Fatalf("turn_ended line = %q, want the plain model error's %q", got, want)
	}
}

// A TURN FAULT IS NOT A VENDOR BLOCK: the prompt queue holds a prompt only
// while a block stands, and the next prompt is what ends a turn fault, so a
// turn fault that held prompts would never end.
func TestAVendorTurnFaultIsNoVendorBlock(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	failTurn(h, maxTurns())

	// Assert
	if block, ok := h.r.VendorBlock(testWS); ok {
		t.Fatalf("VendorBlock = %q, want none while only a turn fault stands", block)
	}
}

func TestATurnDiedFaultGivesWayToTheLinksOwnStep(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	failTurn(h, queryDiedFailure())

	// Act
	h.r.OnLink(testWS, shimclient.LinkRedialing)

	// Assert
	if got := statusAndStep(h.view(t).GetStrip().GetStatus()); got != "agent_repl_fault·severed" {
		t.Fatalf("status = %q, want the severed link's own step", got)
	}
}

func TestATurnDiedFaultCarriesItsLineUnderTheLinksStep(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	failTurn(h, queryDiedFailure())

	// Act
	h.r.OnLink(testWS, shimclient.LinkRedialing)

	// Assert
	if agentReplTurnLine(h, t).GetText() == "" {
		t.Fatalf("the turn's cause line is missing under the severed step")
	}
}
