package rollout

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/wsm"
)

// TestARestartOverASuspendedMergesLeaseRelaunches pins the 2026-10-08 fix: a
// RestartWorkspace whose workspace's merge holds the lease (the verb suspended
// the merge first) relaunches without a hold of its own.
func TestARestartOverASuspendedMergesLeaseRelaunches(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws, _ := h.workspace(t)
	if _, err := h.db.AcquireLease(context.Background(), ws, wsm.HolderMerge, wsm.PolicyHold); err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act.
	err := runRelaunch(t, h, ws, ReasonRestartVerb)

	// Assert.
	if err != nil {
		t.Fatalf("bounce = %v, want the relaunch to run under the merge's lease", err)
	}
	if len(h.fleet.resumes) != 1 {
		t.Fatalf("resumes = %v, want the session resumed once", h.fleet.resumes)
	}
}

// TestARestartOverAMergesLeaseLeavesTheLeaseToTheMerge pins the lease's
// owner: the restart releases nothing it did not take.
func TestARestartOverAMergesLeaseLeavesTheLeaseToTheMerge(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws, _ := h.workspace(t)
	if _, err := h.db.AcquireLease(context.Background(), ws, wsm.HolderMerge, wsm.PolicyHold); err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act.
	_ = runRelaunch(t, h, ws, ReasonRestartVerb)

	// Assert.
	lease, held, err := h.db.Lease(context.Background(), ws)
	if err != nil || !held || lease.Holder != wsm.HolderMerge {
		t.Fatalf("lease = %+v held %v (%v), want the merge's lease still standing", lease, held, err)
	}
}

// TestAnyOtherBounceOverAMergesLeaseStillFails pins the scope: only the
// restart verb suspends the merge first, so every other bounce still refuses
// to replace a shim a merge drives.
func TestAnyOtherBounceOverAMergesLeaseStillFails(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws, _ := h.workspace(t)
	if _, err := h.db.AcquireLease(context.Background(), ws, wsm.HolderMerge, wsm.PolicyHold); err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act.
	err := runRelaunch(t, h, ws, ReasonBuildStale)

	// Assert.
	var held *wsm.LeaseHeldError
	if !errors.As(err, &held) {
		t.Fatalf("bounce = %v, want the merge's lease refusing the hold", err)
	}
}
