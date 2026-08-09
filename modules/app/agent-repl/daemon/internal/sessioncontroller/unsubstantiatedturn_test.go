package sessioncontroller

import (
	"errors"
	"testing"
)

// ---------------------------------------------------------------------------
// THE COMPOUNDING DEADLOCK. A shim that is CONNECTED BUT WEDGED satisfies both
// refusals forever: hibernation is refused while `turn_active` stands, and the
// orphaned-turn reconciliation declines while a controller drives the
// workspace. Observed live with deploys refused on a turn that had been over
// for hours. See unsubstantiatedturn.go.
// ---------------------------------------------------------------------------

// wedgedLiveHarness is the deadlock: the workspace reads the wedge, a
// controller is alive, and the workspace lock is irrelevant because the live
// branch is reached before it.
func wedgedLiveHarness(t *testing.T) *gateHarness {
	t.Helper()
	h := newWedgedHarness(t, &fakeWorkspaceLock{answers: []bool{false}})
	h.applier.orphanedTurnClosed = true
	h.m.mu.Lock()
	h.m.byWS["ws"] = &sessionController{sessionID: "s1", workspace: "ws"}
	h.m.mu.Unlock()
	return h
}

// setShimContradiction arms the two facts the narrowing requires: a stop the
// shim acked and never answered, and a workspace nothing has moved since.
func setShimContradiction(h *gateHarness, unansweredMs, quietMs int64) {
	h.applier.reconcMutex.Lock()
	defer h.applier.reconcMutex.Unlock()
	h.applier.unansweredInterrupt = true
	h.applier.unansweredInterruptMs = unansweredMs
	h.applier.lastActivityKnown = true
	h.applier.lastActivityMs = h.m.now() - quietMs
}

// windowMs is the narrowing's own window, so a test names the policy rather
// than a literal that would drift away from it.
func windowMs() int64 { return unansweredInterruptWindow.Milliseconds() }

// THE DEADLOCK, BROKEN. The shim acked a stop as INTERRUPTED — a positive claim
// that a turn was live and has been aborted — and then produced neither that
// turn's end nor anything else for the whole window. A live controller is not a
// shield against its own shim's contradiction.
func TestAShimThatNeverAnsweredItsOwnInterruptLosesTheClaim(t *testing.T) {
	// Arrange.
	h := wedgedLiveHarness(t)
	setShimContradiction(h, windowMs(), windowMs())
	h.applier.orphanedTurnCloseHook = func(workspace string) {
		settled := wedgedState()
		settled.TurnActive = false
		h.applier.setCurrent(workspace, settled)
	}

	// Act.
	release, err := h.m.acquireSettledHibernationLease("ws")

	// Assert.
	if err != nil {
		t.Fatalf("acquireSettledHibernationLease = %v, want the reconciled workspace to settle", err)
	}
	release()
	closes := h.applier.orphanedTurnClosesApplied()
	if len(closes) != 1 {
		t.Fatalf("reconciliations = %d, want exactly 1", len(closes))
	}
	if closes[0].reason != staleTurnReasonUnsubstantiated {
		t.Fatalf("close reason = %q, want %q; the proof here is a shim contradicting itself, not a vanished process",
			closes[0].reason, staleTurnReasonUnsubstantiated)
	}
}

// THE DECLINE IS NEVER DELETED, only narrowed. Each of these leaves the claim
// standing, because the evidence that would license closing it is absent.
func TestALiveControllerStillShieldsAClaimWithoutShimEvidence(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *gateHarness)
		wantLog string
	}{
		{
			name:    "no stop was ever acked here",
			arrange: func(*gateHarness) {},
			wantLog: "a live controller drives this workspace and its shim has not contradicted the claim",
		},
		{
			name: "the stop was acked recently enough that the turn may still be ending",
			arrange: func(h *gateHarness) {
				setShimContradiction(h, windowMs()-1, windowMs())
			},
			wantLog: "the shim acked a stop recently enough",
		},
		{
			name: "something moved on the workspace inside the window",
			arrange: func(h *gateHarness) {
				setShimContradiction(h, windowMs(), windowMs()-1)
			},
			wantLog: "something moved on this workspace inside the window",
		},
		{
			name: "the log carries no activity record to judge quiet against",
			arrange: func(h *gateHarness) {
				h.applier.reconcMutex.Lock()
				defer h.applier.reconcMutex.Unlock()
				h.applier.unansweredInterrupt = true
				h.applier.unansweredInterruptMs = windowMs()
			},
			wantLog: "no activity record at all",
		},
		{
			name: "the activity record cannot be read",
			arrange: func(h *gateHarness) {
				h.applier.reconcMutex.Lock()
				defer h.applier.reconcMutex.Unlock()
				h.applier.unansweredInterrupt = true
				h.applier.unansweredInterruptMs = windowMs()
				h.applier.lastActivityErr = errors.New("state log unreadable")
			},
			wantLog: "cannot read when anything last happened here",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := wedgedLiveHarness(t)
			tc.arrange(h)

			// Act.
			_, err := h.m.acquireSettledHibernationLease("ws")

			// Assert — the refusal stands and no turn was guessed dead.
			if !errors.Is(err, ErrNotSettled) {
				t.Fatalf("acquireSettledHibernationLease = %v, want ErrNotSettled", err)
			}
			if closes := h.applier.orphanedTurnClosesApplied(); len(closes) != 0 {
				t.Fatalf("reconciliations = %+v, want none; the shim has not contradicted anything", closes)
			}
			if !h.log.contains(tc.wantLog) {
				t.Fatalf("the refusal did not say why; wanted a log naming %q", tc.wantLog)
			}
		})
	}
}

// A FAILED CLOSE IS SURFACED AND THE CLAIM STANDS. The reconciliation reports
// what it could not do rather than answering as though it had.
func TestAnUnsubstantiatedCloseThatFailsLeavesTheClaimStanding(t *testing.T) {
	// Arrange.
	h := wedgedLiveHarness(t)
	setShimContradiction(h, windowMs(), windowMs())
	h.applier.orphanedTurnClosed = false
	h.applier.orphanedTurnErr = errors.New("state log refused the closing row")

	// Act.
	_, err := h.m.acquireSettledHibernationLease("ws")

	// Assert.
	if !errors.Is(err, ErrNotSettled) {
		t.Fatalf("acquireSettledHibernationLease = %v, want ErrNotSettled", err)
	}
	if !h.log.contains("unsubstantiated-turn reconciliation FAILED") {
		t.Fatal("a close that could not be written said nothing; the workspace stays latched with no record of why")
	}
}
