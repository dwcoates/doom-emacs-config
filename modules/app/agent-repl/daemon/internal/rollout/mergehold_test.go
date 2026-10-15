package rollout

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/bounce"
	"agent-repld/internal/wsm"
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

// TestAStaleBuildBounceOverAMergesLeaseIsDeferred pins the scope: only the
// restart verb suspends the merge first, so a stale-build bounce never
// replaces a shim a merge drives. It gives itself back unperformed, before
// any prelaunch, and the registry reruns it once the merge releases the lease
// (owner ruling 2026-10-12: a graceful restart that fails to restart a
// session is a defect; before this it failed at ERROR and was never retried).
func TestAStaleBuildBounceOverAMergesLeaseIsDeferred(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws, _ := h.workspace(t)
	if _, err := h.db.AcquireLease(context.Background(), ws, wsm.HolderMerge, wsm.PolicyHold); err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act.
	err := h.c.shimBounce(ReasonBuildStale, false)(context.Background(), ws)

	// Assert.
	if !errors.Is(err, bounce.ErrDeferredBehindLease) {
		t.Fatalf("bounce = %v, want it deferred behind the merge's lease", err)
	}
	if len(h.fleet.resumes) != 0 {
		t.Fatalf("resumes = %v, want nothing replaced under the merge", h.fleet.resumes)
	}
}

// TestAFailedStaleBuildBounceLeavesItsBuildToBeBouncedAgain pins the stamp: a
// bounce that failed replaced nothing, so the same build is bounced again at
// its next judgement rather than called already bounced.
func TestAFailedStaleBuildBounceLeavesItsBuildToBeBouncedAgain(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws, _ := h.workspace(t)
	if got := h.c.claimStaleBounce(ws, "old-build"); got != staleBounceClaimed {
		t.Fatalf("first claim = %v, want claimed", got)
	}

	// Act.
	h.c.settleStaleBounce(ws, errors.New("the bounce failed"))

	// Assert.
	if got := h.c.claimStaleBounce(ws, "old-build"); got != staleBounceClaimed {
		t.Fatalf("claim after a failed bounce = %v, want the build claimed again", got)
	}
}

// A shim's own refusal is a deferral behind its WORK, never behind a lease: a
// lease change must not re-judge it (promptqueue edgeSet).
func TestAStandDownRefusalIsNeverADeferralBehindALease(t *testing.T) {
	tests := []struct {
		name string
		err  error
	}{
		{name: "refused as live", err: ErrStandDownLive},
		{name: "unanswered", err: ErrStandDownUnanswered},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			behindLease := errors.Is(tc.err, bounce.ErrDeferredBehindLease)

			// Assert
			if behindLease || !errors.Is(tc.err, bounce.ErrDeferred) {
				t.Fatalf("%v: behind a lease = %v, want a deferral behind work only", tc.err, behindLease)
			}
		})
	}
}
