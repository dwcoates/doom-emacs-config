package ssm

import (
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

func TestMarkPromptAcceptedIsIdempotentWhenTurnStartedWonTheRace(t *testing.T) {
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}

	if err := m.MarkPromptAccepted("ws1", "s1", "req-1", func(*frontendv1.WorkspaceState) {}); err != nil {
		t.Fatalf("MarkPromptAccepted: %v", err)
	}

	got := mustCurrent(t, m, "ws1")
	if got.GetCauseKind() != causeTurnStarted || got.GetCauseSeq() != 1 {
		t.Fatalf("cause = %q seq=%d, want durable turn_started/1 preserved", got.GetCauseKind(), got.GetCauseSeq())
	}
	if !cl.contains("prompt accepted IDEMPOTENT") {
		t.Fatalf("missing idempotent-race log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

func TestMarkPromptAcceptedRejectsAnotherSessionsTurn(t *testing.T) {
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1", "s2": "ws1"})
	if err := m.Apply(evTurnStarted("s2", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}

	err := m.MarkPromptAccepted("ws1", "s1", "req-1", func(*frontendv1.WorkspaceState) {})

	if err == nil || !strings.Contains(err.Error(), `session "s2" owns the active turn`) {
		t.Fatalf("err = %v, want active-owner rejection", err)
	}
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = %s, want standing THINKING untouched", got)
	}
	if !cl.contains("prompt accepted REJECTED") {
		t.Fatalf("missing rejection log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// The accepted edge lands on `submitting` and the shim ack advances it to
// `thinking`. The split exists so the phase word stops claiming the agent is
// working during a window in which it has not been handed anything.

func TestMarkPromptAcceptedLandsOnSubmitting(t *testing.T) {
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("session started: %v", err)
	}

	var published *frontendv1.WorkspaceState
	if err := m.MarkPromptAccepted("ws1", "s1", "req-1", func(state *frontendv1.WorkspaceState) {
		published = state
	}); err != nil {
		t.Fatalf("MarkPromptAccepted: %v", err)
	}

	got := mustCurrent(t, m, "ws1")
	if got.GetState() != frontendv1.RenderState_RENDER_STATE_SUBMITTING || !got.GetTurnActive() {
		t.Fatalf("state = %s turn_active=%v, want SUBMITTING/true", got.GetState(), got.GetTurnActive())
	}
	if got.GetCauseKind() != causePromptAccepted || got.GetCauseSeq() != 0 {
		t.Fatalf("cause = %q seq=%d, want %q/0", got.GetCauseKind(), got.GetCauseSeq(), causePromptAccepted)
	}
	if published == nil || published.GetState() != frontendv1.RenderState_RENDER_STATE_SUBMITTING || !published.GetTurnActive() {
		t.Fatalf("published = %+v, want synchronous SUBMITTING/active state", published)
	}
	if !cl.contains(`ssm: prompt accepted ws=ws1 session=s1 request_id="req-1"`) {
		t.Fatalf("missing accepted-edge log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

func TestMarkPromptDeliveredAdvancesSubmittingToThinking(t *testing.T) {
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.MarkPromptAccepted("ws1", "s1", "req-1", func(*frontendv1.WorkspaceState) {}); err != nil {
		t.Fatalf("MarkPromptAccepted: %v", err)
	}

	advanced, err := m.MarkPromptDelivered("ws1", "s1", "req-1")

	if err != nil {
		t.Fatalf("MarkPromptDelivered: %v", err)
	}
	if !advanced {
		t.Fatal("advanced = false, want true over this session's own submitting row")
	}
	got := mustCurrent(t, m, "ws1")
	if got.GetState() != frontendv1.RenderState_RENDER_STATE_THINKING || !got.GetTurnActive() {
		t.Fatalf("state = %s turn_active=%v, want THINKING/true", got.GetState(), got.GetTurnActive())
	}
	if got.GetCauseKind() != causePromptDelivered {
		t.Fatalf("cause = %q, want %q", got.GetCauseKind(), causePromptDelivered)
	}
	if !cl.contains("prompt delivered ws=ws1 session=s1") {
		t.Fatalf("missing delivered-edge log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

func TestMarkPromptDeliveredKeepsTheTurnActiveAcrossTheEdge(t *testing.T) {
	// A turn that reported inactive between the ack and the durable TurnStarted
	// would let a second prompt bypass the queue mid-turn.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.MarkPromptAccepted("ws1", "s1", "req-1", func(*frontendv1.WorkspaceState) {}); err != nil {
		t.Fatalf("MarkPromptAccepted: %v", err)
	}
	if _, err := m.MarkPromptDelivered("ws1", "s1", "req-1"); err != nil {
		t.Fatalf("MarkPromptDelivered: %v", err)
	}

	active, claimant, err := turnClaim(m.db, "ws1")

	if err != nil {
		t.Fatalf("turnClaim: %v", err)
	}
	if !active || claimant != "s1" {
		t.Fatalf("turn claim = (%v, %q), want an active claim held by s1", active, claimant)
	}
}

func TestSubmittingAloneClaimsTheTurn(t *testing.T) {
	// The claim must cover the FIRST half too, or a second prompt submitted
	// during it would be forwarded straight into a turn that is already starting.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.MarkPromptAccepted("ws1", "s1", "req-1", func(*frontendv1.WorkspaceState) {}); err != nil {
		t.Fatalf("MarkPromptAccepted: %v", err)
	}

	active, claimant, err := turnClaim(m.db, "ws1")

	if err != nil {
		t.Fatalf("turnClaim: %v", err)
	}
	if !active || claimant != "s1" {
		t.Fatalf("turn claim = (%v, %q), want submitting to claim the turn for s1", active, claimant)
	}
}

func TestMarkPromptDeliveredPreservesASupersededRow(t *testing.T) {
	// A durable TurnStarted landed first, so there is no submitting row left to
	// advance and overwriting would restate a row already more authoritative.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}

	advanced, err := m.MarkPromptDelivered("ws1", "s1", "req-1")

	if err != nil {
		t.Fatalf("MarkPromptDelivered: %v", err)
	}
	if advanced {
		t.Fatal("advanced = true, want false over a durable turn_started row")
	}
	if got := mustCurrent(t, m, "ws1").GetCauseKind(); got != causeTurnStarted {
		t.Fatalf("cause = %q, want the durable turn_started preserved", got)
	}
	if !cl.contains("decision=preserve state=thinking cause_kind=turn_started") {
		t.Fatalf("missing preservation log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

func TestMarkPromptDeliveredRefusesAnotherSessionsClaim(t *testing.T) {
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1", "s2": "ws1"})
	if err := m.Apply(evTurnStarted("s2", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}

	advanced, err := m.MarkPromptDelivered("ws1", "s1", "req-1")

	if err == nil || !strings.Contains(err.Error(), `session "s2"`) {
		t.Fatalf("err = %v, want foreign-claim refusal", err)
	}
	if advanced {
		t.Fatal("advanced = true, want false on refusal")
	}
	if !cl.contains("prompt delivered REJECTED") {
		t.Fatalf("missing refusal log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// The accepted edge is published BEFORE the shim takes the prompt, so these
// cover the other half of that trade: the retraction that keeps the optimistic
// claim honest, and the cases where retracting would itself be the lie.

func TestMarkPromptRejectedRetractsItsOwnAcceptedRow(t *testing.T) {
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	if err := m.MarkPromptAccepted("ws1", "s1", "req-1", func(*frontendv1.WorkspaceState) {}); err != nil {
		t.Fatalf("MarkPromptAccepted: %v", err)
	}

	var published *frontendv1.WorkspaceState
	retracted, err := m.MarkPromptRejected("ws1", "s1", "req-1", func(state *frontendv1.WorkspaceState) {
		published = state
	})

	if err != nil {
		t.Fatalf("MarkPromptRejected: %v", err)
	}
	if !retracted {
		t.Fatal("retracted = false, want true over this session's own accepted row")
	}
	got := mustCurrent(t, m, "ws1")
	if got.GetState() != frontendv1.RenderState_RENDER_STATE_IDLE || got.GetTurnActive() {
		t.Fatalf("state = %s turn_active=%v, want IDLE/false", got.GetState(), got.GetTurnActive())
	}
	if got.GetCauseKind() != causePromptRejected {
		t.Fatalf("cause = %q, want %q", got.GetCauseKind(), causePromptRejected)
	}
	if published == nil || published.GetTurnActive() {
		t.Fatalf("published = %+v, want a synchronous state claiming no turn", published)
	}
	if !cl.contains(`ssm: prompt rejected ws=ws1 session=s1 request_id="req-1"`) {
		t.Fatalf("missing retraction log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

func TestMarkPromptRejectedPreservesADurableTurnStarted(t *testing.T) {
	// A real turn began in the window between the accept and the submit
	// failure. Closing it would report an idle workspace over a working session.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.MarkPromptAccepted("ws1", "s1", "req-1", func(*frontendv1.WorkspaceState) {}); err != nil {
		t.Fatalf("MarkPromptAccepted: %v", err)
	}
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}

	retracted, err := m.MarkPromptRejected("ws1", "s1", "req-1", func(*frontendv1.WorkspaceState) {})

	if err != nil {
		t.Fatalf("MarkPromptRejected: %v", err)
	}
	if retracted {
		t.Fatal("retracted = true, want false over a durable turn_started row")
	}
	got := mustCurrent(t, m, "ws1")
	if got.GetState() != frontendv1.RenderState_RENDER_STATE_THINKING || !got.GetTurnActive() {
		t.Fatalf("state = %s turn_active=%v, want the live turn preserved", got.GetState(), got.GetTurnActive())
	}
	if !cl.contains("decision=preserve state=thinking cause_kind=turn_started") {
		t.Fatalf("missing preservation log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

func TestMarkPromptRejectedPreservesASettledOutcome(t *testing.T) {
	// The turn the accept claimed has already ENDED. Its outcome is the more
	// specific account and a retraction must not overwrite it.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnEnded("s1", 1, false)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}

	retracted, err := m.MarkPromptRejected("ws1", "s1", "req-1", func(*frontendv1.WorkspaceState) {})

	if err != nil {
		t.Fatalf("MarkPromptRejected: %v", err)
	}
	if retracted {
		t.Fatal("retracted = true, want false over a settled outcome")
	}
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("state = %s, want DONE preserved", got)
	}
}

func TestMarkPromptRejectedRefusesAnotherSessionsClaim(t *testing.T) {
	// Two submitters disagreeing about who owns the workspace's turn is a fault
	// to surface, never one to resolve by closing the other session's turn.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1", "s2": "ws1"})
	if err := m.Apply(evTurnStarted("s2", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}

	retracted, err := m.MarkPromptRejected("ws1", "s1", "req-1", func(*frontendv1.WorkspaceState) {})

	if err == nil || !strings.Contains(err.Error(), `session "s2"`) {
		t.Fatalf("err = %v, want foreign-claim refusal", err)
	}
	if retracted {
		t.Fatal("retracted = true, want false on refusal")
	}
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = %s, want THINKING untouched", got)
	}
	if !cl.contains("prompt rejected REJECTED") {
		t.Fatalf("missing refusal log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

func TestMarkPromptRejectedOnAWorkspaceWithNoAgentAxis(t *testing.T) {
	// The accept itself never landed a row, so there is nothing to withdraw.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})

	retracted, err := m.MarkPromptRejected("ws1", "s1", "req-1", func(*frontendv1.WorkspaceState) {})

	if err != nil {
		t.Fatalf("MarkPromptRejected: %v", err)
	}
	if retracted {
		t.Fatal("retracted = true, want false with no session-status row at all")
	}
	if !cl.contains("decision=no_agent_axis") {
		t.Fatalf("missing no-axis log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

func TestReconcileAlreadyCompleteClosesThinkingBeforeFooterWindow(t *testing.T) {
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}

	var published *frontendv1.WorkspaceState
	closed, err := m.ReconcileAlreadyComplete("ws1", "s1", func(state *frontendv1.WorkspaceState) { published = state })

	if err != nil {
		t.Fatalf("ReconcileAlreadyComplete: %v", err)
	}
	if !closed {
		t.Fatal("closed = false, want true")
	}
	got := mustCurrent(t, m, "ws1")
	if got.GetState() != frontendv1.RenderState_RENDER_STATE_IDLE || got.GetTurnActive() {
		t.Fatalf("state = %s turn_active=%v, want IDLE/false", got.GetState(), got.GetTurnActive())
	}
	if got.GetCauseKind() != causeInterruptAlreadyComplete {
		t.Fatalf("cause = %q, want %q", got.GetCauseKind(), causeInterruptAlreadyComplete)
	}
	if published == nil || published.GetTurnActive() || published.GetState() != frontendv1.RenderState_RENDER_STATE_IDLE {
		t.Fatalf("published = %+v, want synchronous IDLE/inactive state", published)
	}
	if !cl.contains("already-complete reconciliation CLOSED") {
		t.Fatalf("missing reconciliation log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

func TestReconcileAlreadyCompleteClosesSubmittingBeforeFooterWindow(t *testing.T) {
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.MarkPromptAccepted("ws1", "s1", "req-1", func(*frontendv1.WorkspaceState) {}); err != nil {
		t.Fatalf("MarkPromptAccepted: %v", err)
	}

	var published *frontendv1.WorkspaceState
	closed, err := m.ReconcileAlreadyComplete("ws1", "s1", func(state *frontendv1.WorkspaceState) { published = state })

	if err != nil {
		t.Fatalf("ReconcileAlreadyComplete: %v", err)
	}
	if !closed || published == nil || published.GetTurnActive() || published.GetState() != frontendv1.RenderState_RENDER_STATE_IDLE {
		t.Fatalf("closed=%v published=%+v, want synchronous IDLE/inactive state", closed, published)
	}
}

func TestReconcileAlreadyCompletePreservesSettledOutcome(t *testing.T) {
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnEnded("s1", 1, false)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}

	var published *frontendv1.WorkspaceState
	closed, err := m.ReconcileAlreadyComplete("ws1", "s1", func(state *frontendv1.WorkspaceState) { published = state })

	if err != nil {
		t.Fatalf("ReconcileAlreadyComplete: %v", err)
	}
	if closed {
		t.Fatal("closed = true, want false over an already-settled outcome")
	}
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("state = %s, want DONE preserved", got)
	}
	if published == nil || published.GetState() != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("published = %+v, want synchronous preserved DONE state", published)
	}
	if !cl.contains("decision=preserve_settled state=done") {
		t.Fatalf("missing preserved-outcome log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

func TestReconcileAlreadyCompleteClosesStalePermission(t *testing.T) {
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.ApplyPermission("ws1", true, "request"); err != nil {
		t.Fatalf("open permission: %v", err)
	}

	closed, err := m.ReconcileAlreadyComplete("ws1", "s1", func(*frontendv1.WorkspaceState) {})

	if err != nil {
		t.Fatalf("ReconcileAlreadyComplete: %v", err)
	}
	if !closed {
		t.Fatal("closed = false, want true")
	}
	if got := mustCurrent(t, m, "ws1"); got.GetState() != frontendv1.RenderState_RENDER_STATE_IDLE || got.GetTurnActive() {
		t.Fatalf("state = %s turn_active=%v, want IDLE/false", got.GetState(), got.GetTurnActive())
	}
}

func TestReconcileAlreadyCompleteRejectsAnotherSessionsClaim(t *testing.T) {
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1", "s2": "ws1"})
	if err := m.Apply(evTurnStarted("s2", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}

	closed, err := m.ReconcileAlreadyComplete("ws1", "s1", func(*frontendv1.WorkspaceState) {})

	if err == nil || !strings.Contains(err.Error(), `session "s2"`) {
		t.Fatalf("err = %v, want foreign-claim rejection", err)
	}
	if closed {
		t.Fatal("closed = true, want false on rejection")
	}
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = %s, want THINKING untouched", got)
	}
	if !cl.contains("already-complete reconciliation REJECTED") {
		t.Fatalf("missing rejection log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

func TestPromptStateMethodsRejectMissingIdentity(t *testing.T) {
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	tests := []struct {
		name string
		call func() error
	}{
		{"prompt workspace", func() error { return m.MarkPromptAccepted("", "s1", "r", func(*frontendv1.WorkspaceState) {}) }},
		{"prompt session", func() error { return m.MarkPromptAccepted("ws1", "", "r", func(*frontendv1.WorkspaceState) {}) }},
		{"prompt publisher", func() error { return m.MarkPromptAccepted("ws1", "s1", "r", nil) }},
		{"retraction workspace", func() error {
			_, err := m.MarkPromptRejected("", "s1", "r", func(*frontendv1.WorkspaceState) {})
			return err
		}},
		{"retraction session", func() error {
			_, err := m.MarkPromptRejected("ws1", "", "r", func(*frontendv1.WorkspaceState) {})
			return err
		}},
		{"retraction publisher", func() error {
			_, err := m.MarkPromptRejected("ws1", "s1", "r", nil)
			return err
		}},
		{"interrupt workspace", func() error {
			_, err := m.ReconcileAlreadyComplete("", "s1", func(*frontendv1.WorkspaceState) {})
			return err
		}},
		{"interrupt session", func() error {
			_, err := m.ReconcileAlreadyComplete("ws1", "", func(*frontendv1.WorkspaceState) {})
			return err
		}},
		{"interrupt publisher", func() error {
			_, err := m.ReconcileAlreadyComplete("ws1", "s1", nil)
			return err
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if err := tc.call(); err == nil {
				t.Fatal("err = nil, want validation failure")
			}
		})
	}
}
