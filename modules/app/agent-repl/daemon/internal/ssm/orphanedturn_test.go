package ssm

import (
	"database/sql"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ---------------------------------------------------------------------------
// CloseOrphanedTurn — the reconciliation of a turn NOTHING CAN EVER END.
//
// THE OBSERVED WEDGE. CloseStaleTurn retires ONE claimant's claims while turn
// liveness is derived over the whole workspace, so a standing claim held by any
// other claimant left the close with `closed=[]`, the derivation still holding a
// turn open, and the axis untouched. The sweep re-ran every ~30s for hours,
// refusing hibernation and runtime restarts the whole time. These tests pin the
// wider close, the liveness it must NOT spend, and the convergence.
// ---------------------------------------------------------------------------

// arrangeOrphanUnderAnotherClaimant reproduces the exact wedge shape: the axis
// tops out in an UNATTRIBUTED `thinking` (the row a permission close restores)
// while the durable claim under it belongs to s2, so a close aimed at s1 finds
// nothing of its own to retire.
func arrangeOrphanUnderAnotherClaimant(t *testing.T) (*Manager, *capLog) {
	t.Helper()
	m, cl, _ := openTest(t, fakeResolver{"s2": "ws1"})
	if err := applyTest(m, evTurnStarted("s2", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.ApplyPermission("ws1", true, "ask"); err != nil {
		t.Fatalf("permission open: %v", err)
	}
	if err := m.ApplyPermission("ws1", false, "answered"); err != nil {
		t.Fatalf("permission close: %v", err)
	}
	if _, claimant, err := turnClaim(m.db, "ws1"); err != nil || claimant != "" {
		t.Fatalf("arrangement claimant = %q err = %v, want an unattributed `thinking`", claimant, err)
	}
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("arrangement state = %s, want THINKING", renderName(got))
	}
	return m, cl
}

// THE BUG. The orphan reconciliation must retire the claim whatever claimant
// holds it, and the workspace must stop resolving THINKING.
func TestCloseOrphanedTurnClosesAClaimHeldByAnotherClaimant(t *testing.T) {
	// Arrange.
	m, cl := arrangeOrphanUnderAnotherClaimant(t)
	// Act — the reconciliation names the resolved session, which is not the
	// claimant.
	closed, err := m.CloseOrphanedTurn("ws1", "s1", "orphaned_turn_no_live_shim")
	// Assert.
	if err != nil {
		t.Fatalf("CloseOrphanedTurn: %v", err)
	}
	if !closed {
		t.Fatalf("closed = false, want true — no party exists that could ever end this turn")
	}
	if got := mustCurrent(t, m, "ws1").State; got == frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = THINKING after the reconciliation, want anything else")
	}
	if !cl.contains("ssm: stale turn CLOSED ws=ws1 session=s1 reason=\"orphaned_turn_no_live_shim\"") {
		t.Fatalf("missing the canonical close record; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// The durable ledger goes with the axis: a retired axis over a standing claim is
// the same wedge wearing a different hat, because the hibernation lease folds
// open claims into TurnActive.
func TestCloseOrphanedTurnRetiresTheDurableClaimToo(t *testing.T) {
	// Arrange.
	m, _ := arrangeOrphanUnderAnotherClaimant(t)
	// Act.
	if _, err := m.CloseOrphanedTurn("ws1", "s1", "orphaned_turn_no_live_shim"); err != nil {
		t.Fatalf("CloseOrphanedTurn: %v", err)
	}
	// Assert.
	liveness, err := m.TurnLiveness("ws1")
	if err != nil {
		t.Fatalf("TurnLiveness: %v", err)
	}
	if liveness.Active() {
		t.Fatalf("liveness = %s, want no turn in flight — the ledger must be retired with the axis", liveness)
	}
}

// CONVERGENCE. The sweep runs again the moment nothing changed; a second pass
// over a reconciled workspace must find nothing to close and write nothing.
func TestCloseOrphanedTurnConvergesAfterOnePass(t *testing.T) {
	// Arrange — one reconciliation has already run.
	m, cl := arrangeOrphanUnderAnotherClaimant(t)
	if _, err := m.CloseOrphanedTurn("ws1", "s1", "orphaned_turn_no_live_shim"); err != nil {
		t.Fatalf("first CloseOrphanedTurn: %v", err)
	}
	before := mustCurrent(t, m, "ws1").State
	// Act — the next sweep.
	closed, err := m.CloseOrphanedTurn("ws1", "s1", "orphaned_turn_no_live_shim")
	// Assert.
	if err != nil {
		t.Fatalf("second CloseOrphanedTurn: %v", err)
	}
	if closed {
		t.Fatalf("closed = true on the second pass, want false — the first pass left nothing stale")
	}
	if got := mustCurrent(t, m, "ws1").State; got != before {
		t.Fatalf("state = %s after the second pass, want it unchanged at %s", renderName(got), renderName(before))
	}
	if cl.count("ssm: stale turn CLOSED ws=ws1") != 1 {
		t.Fatalf("the reconciliation wrote twice; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// THE LIVENESS THIS MUST NEVER SPEND, from the other side of the boundary. The
// caller declines while a shim is alive, and the ordinary teardown close still
// refuses another session's claim — the wider retirement is licensed by the
// orphan proof alone and must not leak into CloseStaleTurn.
func TestCloseStaleTurnStillDeclinesAnotherClaimantsLiveTurn(t *testing.T) {
	// Arrange — the same shape, reconciled through the TEARDOWN entry point.
	m, _ := arrangeOrphanUnderAnotherClaimant(t)
	// Act.
	closed, err := m.CloseStaleTurn("ws1", "s1", "hibernate_session", true)
	// Assert.
	if err != nil {
		t.Fatalf("CloseStaleTurn: %v", err)
	}
	if closed {
		t.Fatalf("closed = true, want false — a stop that killed only s1's shim may not spend s2's live turn")
	}
	liveness, err := m.TurnLiveness("ws1")
	if err != nil {
		t.Fatalf("TurnLiveness: %v", err)
	}
	if !liveness.Active() {
		t.Fatalf("liveness = %s, want s2's turn still in flight", liveness)
	}
}

// A workspace whose turn ended honestly is left exactly as it stands, so the
// reconciliation cannot paint over a real outcome.
func TestCloseOrphanedTurnWritesNothingOverASettledWorkspace(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := applyTest(m, evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := applyTest(m, evTurnEnded("s1", 2, false)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}
	// Act.
	closed, err := m.CloseOrphanedTurn("ws1", "s1", "orphaned_turn_no_live_shim")
	// Assert.
	if err != nil {
		t.Fatalf("CloseOrphanedTurn: %v", err)
	}
	if closed {
		t.Fatalf("closed = true, want false — the turn's own end already settled the workspace")
	}
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("state = %s, want DONE preserved", renderName(got))
	}
}

// A claim standing under a SETTLED axis is still a turn in flight to the
// hibernation lease, so the reconciliation retires it and says it did — that
// answer is what lets the caller retry the settledness test instead of sweeping
// again.
func TestCloseOrphanedTurnReportsARetiredClaimUnderASettledAxis(t *testing.T) {
	// Arrange — the axis settled while the ledger kept s2's claim, which is the
	// drift the hibernation lease reads as a turn in flight.
	m, _ := arrangeOrphanUnderAnotherClaimant(t)
	tx, err := m.db.Begin()
	if err != nil {
		t.Fatalf("arrangement begin: %v", err)
	}
	if err := appendRow(tx, "ws1", "", sigIdle, "test arrangement", sql.NullInt64{}, m.nextAt(), ""); err != nil {
		t.Fatalf("arrangement append: %v", err)
	}
	if err := tx.Commit(); err != nil {
		t.Fatalf("arrangement commit: %v", err)
	}
	if standing, _, err := turnClaim(m.db, "ws1"); err != nil || standing {
		t.Fatalf("arrangement standing = %v err = %v, want a settled axis", standing, err)
	}
	// Act.
	closed, err := m.CloseOrphanedTurn("ws1", "s1", "orphaned_turn_no_live_shim")
	// Assert.
	if err != nil {
		t.Fatalf("CloseOrphanedTurn: %v", err)
	}
	if !closed {
		t.Fatalf("closed = false, want true — the retired claim is a change the caller must re-test on")
	}
	liveness, err := m.TurnLiveness("ws1")
	if err != nil {
		t.Fatalf("TurnLiveness: %v", err)
	}
	if liveness.Active() {
		t.Fatalf("liveness = %s, want the orphaned claim retired", liveness)
	}
}

// Each missing input is rejected on its own. The session id is NOT among them:
// the close is the workspace's, and a workspace latched with no session on its
// resolved state is exactly one this must be able to unstick.
func TestCloseOrphanedTurnRejectsMissingInputs(t *testing.T) {
	tests := []struct {
		name      string
		workspace string
		reason    string
		wantErr   string
	}{
		{
			name:      "empty workspace",
			workspace: "", reason: "orphaned_turn_no_live_shim",
			wantErr: "CloseOrphanedTurn got an empty workspace",
		},
		{
			name:      "empty reason",
			workspace: "ws1", reason: "",
			wantErr: "got an empty reason",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
			// Act.
			closed, err := m.CloseOrphanedTurn(tc.workspace, "s1", tc.reason)
			// Assert.
			if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
				t.Fatalf("err = %v, want one containing %q", err, tc.wantErr)
			}
			if closed {
				t.Fatalf("closed = true, want false — a rejected call writes nothing")
			}
		})
	}
}

// A workspace whose resolved state carries NO session id is reconciled rather
// than refused forever; the row then names no session, as the rotation's does.
func TestCloseOrphanedTurnAcceptsAnEmptySessionID(t *testing.T) {
	// Arrange.
	m, _ := arrangeOrphanUnderAnotherClaimant(t)
	// Act.
	closed, err := m.CloseOrphanedTurn("ws1", "", "orphaned_turn_no_live_shim")
	// Assert.
	if err != nil {
		t.Fatalf("CloseOrphanedTurn: %v", err)
	}
	if !closed {
		t.Fatalf("closed = false, want true — an unnameable session is not a reason to stay latched")
	}
	if got := mustCurrent(t, m, "ws1").State; got == frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = THINKING, want the workspace reconciled out of it")
	}
}

// The database failing is surfaced, never absorbed into a false "nothing to
// reconcile".
func TestCloseOrphanedTurnSurfacesAStateReadFailure(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.db.Close(); err != nil {
		t.Fatalf("close db: %v", err)
	}
	// Act.
	closed, err := m.CloseOrphanedTurn("ws1", "s1", "orphaned_turn_no_live_shim")
	// Assert.
	if err == nil {
		t.Fatalf("err = nil, want the read failure surfaced")
	}
	if closed {
		t.Fatalf("closed = true, want false — nothing was written")
	}
}
