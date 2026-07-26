package ssm

import (
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// evTurnEndedReason is evTurnEnded with an explicit stop reason, for the
// vendor-block classification.
func evTurnEndedReason(sid string, seq uint64, reason string, isErr bool) *corev1.Event {
	return &corev1.Event{
		SessionId: sid,
		Seq:       seq,
		Payload: &corev1.Event_TurnEnded{
			TurnEnded: &corev1.TurnEnded{StopReason: reason, IsError: isErr},
		},
	}
}

// ---------------------------------------------------------------------------
// Shim-asserted readiness and the no-regress guard
// ---------------------------------------------------------------------------

// A session that is never prompted must still reach green: that is the whole
// reason readiness moved off the vendor's first-prompt-only system:init.
func TestSessionStartedReachesReadyWithoutAnyPrompt(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	// Act — readiness only; no prompt is ever submitted.
	if err := m.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_READY {
		t.Fatalf("state = %s, want READY", renderName(got))
	}
}

// THE NO-REGRESS GUARD. A readiness assertion arriving over a live turn must
// not knock it back — the regression observed as a THINKING→IDLE flip.
func TestReadinessDuringActiveTurnDoesNotRegress(t *testing.T) {
	// Arrange — a turn is running.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	// Act — a shim relaunch/revive re-asserts readiness underneath it.
	if err := m.Apply(evSessionStarted("s1", 2)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	// Assert — the running turn is the stronger claim and stands.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = %s, want THINKING (readiness must not regress a live turn)", renderName(got))
	}
}

func TestSuppressedReadinessIsLoggedNotSilent(t *testing.T) {
	// Arrange.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	// Act.
	if err := m.Apply(evSessionStarted("s1", 2)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	// Assert — a dropped signal must never be invisible.
	if !cl.contains("readiness suppressed") {
		t.Fatal("a dropped readiness signal was not logged")
	}
}

// The guard is scoped to a LIVE turn, not to every prior state: readiness
// arriving after the turn ended must take effect normally.
func TestReadinessAfterTurnEndsApplies(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.Apply(evTurnEnded("s1", 2, false)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}
	// Act.
	if err := m.Apply(evSessionStarted("s1", 3)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_READY {
		t.Fatalf("state = %s, want READY", renderName(got))
	}
}

// ---------------------------------------------------------------------------
// Vendor/account blocking
// ---------------------------------------------------------------------------

func TestVendorBlockingTurnEnd(t *testing.T) {
	tests := []struct {
		name       string
		stopReason string
		isError    bool
		want       bool
	}{
		{name: "clean success does not block", stopReason: "end_turn", want: false},
		{name: "user interrupt does not block", stopReason: "aborted", want: false},
		{name: "aborted with the error flag still does not block", stopReason: "aborted", isError: true, want: false},
		{name: "max turns blocks", stopReason: "error_max_turns", want: true},
		{name: "max budget blocks", stopReason: "error_max_budget", want: true},
		{name: "execution error blocks", stopReason: "error_during_execution", want: true},
		{name: "model refusal blocks", stopReason: "refusal", want: true},
		{name: "auth failure blocks", stopReason: "authentication_failed", want: true},
		{name: "billing failure blocks", stopReason: "billing_error", want: true},
		{name: "a rejected request blocks", stopReason: "invalid_request", want: true},
		{name: "a server error blocks", stopReason: "server_error", want: true},
		{name: "an unrecognized erroring reason blocks", stopReason: "novel_failure", isError: true, want: true},
		{name: "an unrecognized clean reason does not block", stopReason: "novel_ending", want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := VendorBlockingTurnEnd(tc.stopReason, tc.isError)
			// Assert.
			if got != tc.want {
				t.Fatalf("VendorBlockingTurnEnd(%q, %v) = %v, want %v",
					tc.stopReason, tc.isError, got, tc.want)
			}
		})
	}
}

// An allowed_warning rides a request the API ALLOWED, so it must not claim
// the session has stopped.
func TestVendorBlockingRateLimit(t *testing.T) {
	tests := []struct {
		name   string
		status string
		want   bool
	}{
		{name: "allowed does not block", status: "allowed", want: false},
		{name: "an overage warning does not block", status: "allowed_warning", want: false},
		{name: "a rejection blocks", status: "rejected", want: true},
		{name: "credits required blocks", status: "credits_required", want: true},
		{name: "a status-less signal blocks", status: "", want: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := VendorBlockingRateLimit(tc.status)
			// Assert.
			if got != tc.want {
				t.Fatalf("VendorBlockingRateLimit(%q) = %v, want %v", tc.status, got, tc.want)
			}
		})
	}
}

func TestAbnormalConclusionResolvesVendorBlocked(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	// Act.
	if err := m.Apply(evTurnEndedReason("s1", 2, "error_max_turns", true)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED {
		t.Fatalf("state = %s, want VENDOR_BLOCKED", renderName(got))
	}
}

func TestUserInterruptConcludesGreen(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	// Act — the user asked for the turn to stop, and it stopped.
	if err := m.Apply(evTurnEndedReason("s1", 2, "aborted", true)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("state = %s, want DONE", renderName(got))
	}
}

func TestVendorClearedReleasesTheBlock(t *testing.T) {
	// Arrange — blocked.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnEndedReason("s1", 1, "authentication_failed", true)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}
	// Act — a clean auth report releases it.
	if err := m.ApplyVendorCleared("ws1", "auth_ok"); err != nil {
		t.Fatalf("vendor cleared: %v", err)
	}
	// Assert — the block is gone and the agent axis speaks again.
	if got := mustCurrent(t, m, "ws1").State; got == frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED {
		t.Fatalf("state = %s, want the block released", renderName(got))
	}
}

// A clean turn is the proof the vendor released whatever blocked the
// account — the only evidence the daemon accepts, since our own retries are
// not evidence of anything.
func TestCleanTurnAfterABlockReleasesIt(t *testing.T) {
	// Arrange — blocked by an auth failure.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnEndedReason("s1", 1, "authentication_failed", true)); err != nil {
		t.Fatalf("blocked turn: %v", err)
	}
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED {
		t.Fatalf("state = %s, want VENDOR_BLOCKED before the clean turn", renderName(got))
	}
	// Act — the next turn concludes cleanly.
	if err := m.Apply(evTurnEndedReason("s1", 2, "end_turn", false)); err != nil {
		t.Fatalf("clean turn: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("state = %s, want DONE after a clean conclusion", renderName(got))
	}
}

// An abnormal conclusion records BOTH axes: the turn ended AND the vendor
// blocked. Collapsing them into one row loses whichever is not written, and
// clearing the block would then leave the workspace with no state at all.
func TestAbnormalConclusionRecordsTheSettledTurnUnderneath(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnEndedReason("s1", 1, "error_max_turns", true)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}
	// Act — release the block out of band.
	if err := m.ApplyVendorCleared("ws1", "user_raised_the_limit"); err != nil {
		t.Fatalf("vendor cleared: %v", err)
	}
	// Assert — the settled turn is revealed, not an absent state.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("state = %s, want DONE underneath the released block", renderName(got))
	}
}

// Purple outranks red: a session the vendor stopped is not going to finish
// whatever it still looks busy doing.
func TestVendorBlockOutranksThinking(t *testing.T) {
	// Arrange.
	db := newTestDB(t)
	seedSignal(t, db, "ws", "s1", sigVendorBlocked, causeVendorBlocked, 1, 1)
	seedSignal(t, db, "ws", "s1", sigThinking, causeTurnStarted, 2, 2)
	// Act.
	got, err := resolve(db, "ws", nil)
	// Assert.
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if got.state != frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED {
		t.Fatalf("state = %s, want VENDOR_BLOCKED", renderName(got.state))
	}
}

// ---------------------------------------------------------------------------
// Paint attestation
// ---------------------------------------------------------------------------

func TestPaintAckAdvancesTheWatermark(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	// Act.
	if err := m.ApplyPaintAck("ws1", 7); err != nil {
		t.Fatalf("paint ack: %v", err)
	}
	// Assert — a ready, attested session is green.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_READY {
		t.Fatalf("state = %s, want READY", renderName(got))
	}
}

// Seq 0 is a REAL attestation of an empty history, which is what lets a
// never-prompted session reach green.
func TestPaintAckAtZeroAttestsAnEmptyHistory(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	if err := m.ApplyPaintLost("ws1", "fresh"); err != nil {
		t.Fatalf("paint lost: %v", err)
	}
	// Act.
	if err := m.ApplyPaintAck("ws1", 0); err != nil {
		t.Fatalf("paint ack: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_READY {
		t.Fatalf("state = %s, want READY after attesting an empty history", renderName(got))
	}
}

// An absent attestation leaves BLUE, whatever every other hop reports.
func TestUnpaintedRouteResolvesBlue(t *testing.T) {
	// Arrange — a fully ready session whose frontend has not attested.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	// Act.
	if err := m.ApplyPaintLost("ws1", "never_attested"); err != nil {
		t.Fatalf("paint lost: %v", err)
	}
	// Assert — INIT is blue's token.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_INIT {
		t.Fatalf("state = %s, want INIT (blue) for an unattested route", renderName(got))
	}
}

// Blue outranks a live turn: a turn running behind a route the user cannot
// see is broken, not working.
func TestUnpaintedOutranksThinking(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.ApplyPaintLost("ws1", "shim_died"); err != nil {
		t.Fatalf("paint lost: %v", err)
	}
	// Act.
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_INIT {
		t.Fatalf("state = %s, want INIT (blue beats red)", renderName(got))
	}
}

// Versioning: a stale ack cannot green a newer gap.
func TestStalePaintAckIsDropped(t *testing.T) {
	// Arrange.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.ApplyPaintAck("ws1", 9); err != nil {
		t.Fatalf("paint ack: %v", err)
	}
	// Act.
	if err := m.ApplyPaintAck("ws1", 4); err != nil {
		t.Fatalf("stale paint ack: %v", err)
	}
	// Assert — dropped, and loudly.
	if !cl.contains("paint ack superseded") {
		t.Fatal("a superseded paint ack was not logged")
	}
}

// A route break withdraws the watermark, so an ack replayed from before the
// break does not silently re-green the workspace at its old seq.
func TestPaintLostResetsTheWatermark(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	if err := m.ApplyPaintAck("ws1", 6); err != nil {
		t.Fatalf("paint ack: %v", err)
	}
	// Act.
	if err := m.ApplyPaintLost("ws1", "hibernated"); err != nil {
		t.Fatalf("paint lost: %v", err)
	}
	// Assert — blue again, and the SAME ack is accepted afresh.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_INIT {
		t.Fatalf("state = %s, want INIT after the route broke", renderName(got))
	}
	if err := m.ApplyPaintAck("ws1", 6); err != nil {
		t.Fatalf("re-attest: %v", err)
	}
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_READY {
		t.Fatalf("state = %s, want READY after re-attesting", renderName(got))
	}
}

// ---------------------------------------------------------------------------
// Yellow is carved out of every green state, not just idle
// ---------------------------------------------------------------------------

func TestGreenStatesPromoteToIdleAsync(t *testing.T) {
	tests := []struct {
		name  string
		token string
	}{
		{name: "idle with live work", token: sigIdle},
		{name: "ready with live work", token: sigReady},
		{name: "done with live work", token: sigDone},
		{name: "permission with live work", token: sigPermission},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			db := newTestDB(t)
			seedSignal(t, db, "ws", "s1", tc.token, causeSessionStarted, 1, 1)
			seedTaskSignal(t, db, "ws", "s1", sigTaskStarted, causeTaskStarted, 2, 2, "a1")
			// Act.
			got, err := resolve(db, "ws", nil)
			// Assert.
			if err != nil {
				t.Fatalf("resolve: %v", err)
			}
			if got.state != frontendv1.RenderState_RENDER_STATE_IDLE_ASYNC {
				t.Fatalf("state = %s, want IDLE_ASYNC", renderName(got.state))
			}
		})
	}
}

// Red is NOT promoted: a running turn is a stronger claim than background
// work, so thinking stays thinking.
func TestThinkingIsNotPromotedToIdleAsync(t *testing.T) {
	// Arrange.
	db := newTestDB(t)
	seedSignal(t, db, "ws", "s1", sigThinking, causeTurnStarted, 1, 1)
	seedTaskSignal(t, db, "ws", "s1", sigTaskStarted, causeTaskStarted, 2, 2, "a1")
	// Act.
	got, err := resolve(db, "ws", nil)
	// Assert.
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if got.state != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = %s, want THINKING", renderName(got.state))
	}
}

// ---------------------------------------------------------------------------
// Backfill composes into green
// ---------------------------------------------------------------------------

// THE REOPEN WEDGE, end to end at the SSM: a session whose transcript was
// already ingested reaches GREEN with no prompt ever submitted. If the
// backfill axis could not settle for such a session, this would sit blue
// forever on the most ordinary action there is — reopening a workspace.
func TestReopenedSessionWithIngestedHistoryReachesGreen(t *testing.T) {
	// Arrange — the reopen sequence, with NO transcript line arriving:
	// the daemon settles the backfill from the store high-water, the shim
	// asserts readiness, and the frontend attests it painted the replay.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.ApplyBackfillState("ws1", "done"); err != nil {
		t.Fatalf("backfill: %v", err)
	}
	if err := m.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	// Act — the frontend painted the replayed history.
	if err := m.ApplyPaintAck("ws1", 4200); err != nil {
		t.Fatalf("paint ack: %v", err)
	}
	// Assert
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_READY {
		t.Fatalf("state = %s, want READY for a reopened, fully-backfilled session", renderName(got))
	}
}

// A FAILED backfill is blue: the history is incomplete, so anything painted
// from it is a partial account of the conversation.
func TestFailedBackfillResolvesBlue(t *testing.T) {
	// Arrange — everything else healthy and attested.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	if err := m.ApplyPaintAck("ws1", 10); err != nil {
		t.Fatalf("paint ack: %v", err)
	}
	// Act.
	if err := m.ApplyBackfillState("ws1", "failed"); err != nil {
		t.Fatalf("backfill: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_INIT {
		t.Fatalf("state = %s, want INIT (blue) for a failed backfill", renderName(got))
	}
}

// "Nothing to backfill" is a real, correct answer — a genuinely fresh
// workspace — not an unknown, so it must not hold the workspace blue.
func TestEmptyWorkspaceBackfillDoesNotHoldBlue(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.ApplyBackfillState("ws1", "pending"); err != nil {
		t.Fatalf("backfill: %v", err)
	}
	if err := m.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	// Act — an empty history is attested at seq 0.
	if err := m.ApplyPaintAck("ws1", 0); err != nil {
		t.Fatalf("paint ack: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_READY {
		t.Fatalf("state = %s, want READY for a fresh empty workspace", renderName(got))
	}
}

// A failed backfill that later recovers releases the axis.
func TestBackfillRecoveryReleasesBlue(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	if err := m.ApplyPaintAck("ws1", 3); err != nil {
		t.Fatalf("paint ack: %v", err)
	}
	if err := m.ApplyBackfillState("ws1", "failed"); err != nil {
		t.Fatalf("backfill failed: %v", err)
	}
	// Act.
	if err := m.ApplyBackfillState("ws1", "done"); err != nil {
		t.Fatalf("backfill done: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_READY {
		t.Fatalf("state = %s, want READY once the backfill settled", renderName(got))
	}
}
