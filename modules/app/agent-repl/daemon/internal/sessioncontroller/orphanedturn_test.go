package sessioncontroller

import (
	"errors"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ---------------------------------------------------------------------------
// THE ORPHANED-TURN RECONCILIATION. `state=hibernated turn_active=true` is
// self-contradictory, and only a shim handshake clears the flag — so when the
// handshake is what keeps failing, the workspace can never hibernate again.
// See connectivitystate.go.
// ---------------------------------------------------------------------------

// wedgedState is the live wedge: a hibernated workspace still claiming a turn.
func wedgedState() *frontendv1.WorkspaceState {
	return &frontendv1.WorkspaceState{
		Workspace:  "ws",
		SessionId:  "s1",
		State:      frontendv1.RenderState_RENDER_STATE_HIBERNATED,
		TurnActive: true,
	}
}

// newWedgedHarness is a manager whose workspace reads the wedge and whose
// liveness probe is under the test's control.
func newWedgedHarness(t *testing.T, lock *fakeWorkspaceLock) *gateHarness {
	t.Helper()
	h := newGateHarness(t, lock)
	h.applier.setCurrent("ws", wedgedState())
	return h
}

// TestAProvablyStaleTurnIsClosedAndTheHibernationProceeds: no controller and no
// process hold the workspace, so nothing can ever report the turn's end.
func TestAProvablyStaleTurnIsClosedAndTheHibernationProceeds(t *testing.T) {
	// Arrange — the workspace lock is free, and the close settles the axis the
	// way a real closing row does.
	h := newWedgedHarness(t, &fakeWorkspaceLock{answers: []bool{false}})
	h.applier.orphanedTurnClosed = true
	h.applier.orphanedTurnCloseHook = func(workspace string) {
		settled := wedgedState()
		settled.TurnActive = false
		h.applier.setCurrent(workspace, settled)
	}

	// Act
	release, err := h.m.acquireSettledHibernationLease("ws")

	// Assert — the lease is granted, through the ONE stale-turn mechanism, under
	// a reason that names this cause rather than a teardown.
	if err != nil {
		t.Fatalf("acquireSettledHibernationLease = %v, want the reconciled workspace to settle", err)
	}
	release()
	closes := h.applier.orphanedTurnClosesApplied()
	if len(closes) != 1 {
		t.Fatalf("stale-turn closes = %d, want exactly 1", len(closes))
	}
	if closes[0].reason != staleTurnReasonOrphaned {
		t.Fatalf("close reason = %q, want %q", closes[0].reason, staleTurnReasonOrphaned)
	}
	if closes[0].sessionID != "s1" {
		t.Fatalf("close = %+v, want it made on behalf of the claiming session", closes[0])
	}
}

// TestASucceededReconciliationIsNotAttemptedAgain: the every-sweep churn the
// wedge produced. Once the reconciliation settles the workspace, the next
// hibernation must find nothing to reconcile and ask for no second close.
func TestASucceededReconciliationIsNotAttemptedAgain(t *testing.T) {
	// Arrange — the reconciliation settles the axis, as the real one does.
	h := newWedgedHarness(t, &fakeWorkspaceLock{answers: []bool{false, false}})
	h.applier.orphanedTurnClosed = true
	h.applier.orphanedTurnCloseHook = func(workspace string) {
		settled := wedgedState()
		settled.TurnActive = false
		h.applier.setCurrent(workspace, settled)
	}
	release, err := h.m.acquireSettledHibernationLease("ws")
	if err != nil {
		t.Fatalf("arrangement lease = %v, want the reconciled workspace to settle", err)
	}
	release()

	// Act — the sweep comes round again.
	release, err = h.m.acquireSettledHibernationLease("ws")

	// Assert — it settled on its own, so nothing was reconciled a second time.
	if err != nil {
		t.Fatalf("second acquireSettledHibernationLease = %v, want the settled workspace to grant", err)
	}
	release()
	if closes := h.applier.orphanedTurnClosesApplied(); len(closes) != 1 {
		t.Fatalf("orphan reconciliations = %d, want exactly 1 — the second sweep must find nothing to close", len(closes))
	}
}

// TestAnUnprovableTurnIsRefusedRatherThanReconciled covers every way liveness
// can fail to be disproved. Each must leave the claim standing and write
// nothing.
func TestAnUnprovableTurnIsRefusedRatherThanReconciled(t *testing.T) {
	probeErr := errors.New("permission denied")
	tests := []struct {
		name    string
		lock    *fakeWorkspaceLock
		arrange func(h *gateHarness)
		wantLog string
	}{
		{
			name:    "a live shim holds the workspace lock",
			lock:    &fakeWorkspaceLock{answers: []bool{true}},
			wantLog: "a live shim holds this workspace's lock",
		},
		{
			name:    "the probe cannot answer",
			lock:    &fakeWorkspaceLock{err: probeErr},
			wantLog: "cannot determine whether a shim is alive",
		},
		{
			name: "a controller still drives the workspace",
			lock: &fakeWorkspaceLock{answers: []bool{false}},
			arrange: func(h *gateHarness) {
				h.m.mu.Lock()
				h.m.byWS["ws"] = &sessionController{sessionID: "s1", workspace: "ws"}
				h.m.mu.Unlock()
			},
			wantLog: "a live controller drives this workspace",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newWedgedHarness(t, tc.lock)
			h.applier.orphanedTurnClosed = true
			if tc.arrange != nil {
				tc.arrange(h)
			}

			// Act
			_, err := h.m.acquireSettledHibernationLease("ws")

			// Assert — the refusal stands and no turn was guessed dead.
			if !errors.Is(err, ErrNotSettled) {
				t.Fatalf("acquireSettledHibernationLease = %v, want ErrNotSettled", err)
			}
			if closes := h.applier.orphanedTurnClosesApplied(); len(closes) != 0 {
				t.Fatalf("stale-turn closes = %+v, want none", closes)
			}
			if !h.log.contains(tc.wantLog) {
				t.Fatalf("the refusal did not say why; wanted a log naming %q", tc.wantLog)
			}
		})
	}
}

// TestTheReconciliationRunsAtMostOncePerRequest: a close that does not settle
// the workspace must not loop the lease acquisition.
func TestTheReconciliationRunsAtMostOncePerRequest(t *testing.T) {
	// Arrange — the close writes a row but the workspace still reads the wedge.
	h := newWedgedHarness(t, &fakeWorkspaceLock{answers: []bool{false}})
	h.applier.orphanedTurnClosed = true

	// Act
	_, err := h.m.acquireSettledHibernationLease("ws")

	// Assert
	if !errors.Is(err, ErrNotSettled) {
		t.Fatalf("acquireSettledHibernationLease = %v, want ErrNotSettled", err)
	}
	if closes := h.applier.orphanedTurnClosesApplied(); len(closes) != 1 {
		t.Fatalf("stale-turn closes = %d, want exactly 1", len(closes))
	}
}

// TestASettledWorkspaceIsNeverReconciled: the ordinary hibernation path must
// not acquire a probe or a write.
func TestASettledWorkspaceIsNeverReconciled(t *testing.T) {
	// Arrange
	lock := &fakeWorkspaceLock{answers: []bool{false}}
	h := newGateHarness(t, lock)
	h.applier.setCurrent("ws", &frontendv1.WorkspaceState{
		Workspace: "ws", SessionId: "s1", State: frontendv1.RenderState_RENDER_STATE_READY,
	})

	// Act
	release, err := h.m.acquireSettledHibernationLease("ws")

	// Assert
	if err != nil {
		t.Fatalf("acquireSettledHibernationLease over a settled workspace = %v", err)
	}
	release()
	if lock.calls != 0 {
		t.Fatalf("workspace lock probed %d times for a settled workspace", lock.calls)
	}
	if closes := h.applier.orphanedTurnClosesApplied(); len(closes) != 0 {
		t.Fatalf("stale-turn closes = %+v, want none", closes)
	}
}
