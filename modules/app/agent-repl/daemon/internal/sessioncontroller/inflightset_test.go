package sessioncontroller

import (
	"errors"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/inflight"
)

// inflightHarness is a wired manager whose SSM double can be posed in each of
// the evidence states the resolver must tell apart.
func inflightHarness(t *testing.T) *queueHarness {
	t.Helper()
	h := newQueueHarnessWithPusher(t, nil, nil, func(string, ...any) {})
	h.applier.setCurrent("ws", &frontendv1.WorkspaceState{
		Workspace: "ws",
		SessionId: "s1",
		State:     frontendv1.RenderState_RENDER_STATE_IDLE,
	})
	return h
}

// TestInFlightIsEmptyForAQuietWorkspace covers the one answer that licenses a
// teardown.
func TestInFlightIsEmptyForAQuietWorkspace(t *testing.T) {
	// Arrange
	h := inflightHarness(t)

	// Act
	got := h.m.InFlight("ws")

	// Assert
	blocked, why := got.Blocks()
	if blocked {
		t.Fatalf("Blocks() = true for a quiet workspace: %s", why)
	}
	if _, ok := got.Settled(); !ok {
		t.Fatal("a quiet workspace yielded no settledness proof")
	}
}

// TestInFlightNamesALiveBackgroundTask is the member every turn-shaped gate
// missed: a shim with no turn and a running detached task is NOT idle.
func TestInFlightNamesALiveBackgroundTask(t *testing.T) {
	// Arrange
	h := inflightHarness(t)
	h.applier.setLiveTasks(0, "task-1")

	// Act
	got := h.m.InFlight("ws")

	// Assert
	if !got.Has(inflight.Item{Kind: inflight.KindTask, ID: "task-1"}) {
		t.Fatalf("in-flight set = %s, want the live task named", got.Summary())
	}
	if _, ok := got.Settled(); ok {
		t.Fatal("a workspace running a background task yielded a settledness proof")
	}
}

// TestInFlightNamesAnOpenTurnClaim covers the turn plane.
func TestInFlightNamesAnOpenTurnClaim(t *testing.T) {
	// Arrange
	h := inflightHarness(t)
	h.applier.setDurableTurns("turn-1")

	// Act
	got := h.m.InFlight("ws")

	// Assert
	if !got.Has(inflight.Item{Kind: inflight.KindTurn, ID: "turn-1"}) {
		t.Fatalf("in-flight set = %s, want the open turn claim named", got.Summary())
	}
}

// TestInFlightIsUnknownWhenTheStateCannotBeRead covers an unreadable SSM: the
// failure becomes an UNKNOWN that blocks, never an error a caller might read as
// "not busy".
func TestInFlightIsUnknownWhenTheStateCannotBeRead(t *testing.T) {
	// Arrange
	h := inflightHarness(t)
	h.applier.setCurrentErr(errors.New("state log unreadable"))

	// Act
	got := h.m.InFlight("ws")

	// Assert
	if got.Known() {
		t.Fatalf("in-flight set = %s, want UNKNOWN when the state cannot be read", got.Summary())
	}
	if blocked, _ := got.Blocks(); !blocked {
		t.Fatal("an unknown in-flight set did not block")
	}
}

// TestInFlightIsUnknownForAWorkspaceWithNoResolvedState pins that an unknown
// workspace is not a quiet one — the ruling Server.sweepable already made for
// its own read, now inherited by every consumer.
func TestInFlightIsUnknownForAWorkspaceWithNoResolvedState(t *testing.T) {
	// Arrange
	h := inflightHarness(t)

	// Act
	got := h.m.InFlight("never-seen")

	// Assert
	if got.Known() {
		t.Fatalf("in-flight set = %s, want UNKNOWN for a workspace with no resolved state", got.Summary())
	}
}

// TestInFlightIsUnknownWhenALiveTaskHasNoIdentity covers the anonymous leg: a
// running task nobody can name makes the SET unknown rather than shorter.
func TestInFlightIsUnknownWhenALiveTaskHasNoIdentity(t *testing.T) {
	// Arrange
	h := inflightHarness(t)
	h.applier.setLiveTasks(1, "task-1")

	// Act
	got := h.m.InFlight("ws")

	// Assert
	if got.Known() {
		t.Fatalf("in-flight set = %s, want UNKNOWN while an unidentified task runs", got.Summary())
	}
	if !strings.Contains(got.Reason(), "without an identity") {
		t.Fatalf("reason = %q, want it to name the anonymous starts", got.Reason())
	}
}

// TestInFlightIsUnknownWhenTheTaskReadFails covers the read failure separately
// from the anonymous case: both understate what is running if folded in.
func TestInFlightIsUnknownWhenTheTaskReadFails(t *testing.T) {
	// Arrange
	h := inflightHarness(t)
	h.applier.setLiveTaskIDsErr(errors.New("task rows unreadable"))

	// Act
	got := h.m.InFlight("ws")

	// Assert
	if got.Known() {
		t.Fatalf("in-flight set = %s, want UNKNOWN when the task set cannot be read", got.Summary())
	}
}

// TestInFlightIsUnknownWhenATurnRunsUnderNoIdentity covers an adopted turn: the
// workspace reads turn_active and the ledger names nothing, so a running turn
// exists that cannot be named. Neither fabricating an id nor reporting an empty
// set is honest.
func TestInFlightIsUnknownWhenATurnRunsUnderNoIdentity(t *testing.T) {
	// Arrange
	h := inflightHarness(t)
	h.applier.setCurrent("ws", &frontendv1.WorkspaceState{
		Workspace:  "ws",
		SessionId:  "s1",
		State:      frontendv1.RenderState_RENDER_STATE_THINKING,
		TurnActive: true,
	})

	// Act
	got := h.m.InFlight("ws")

	// Assert
	if got.Known() {
		t.Fatalf("in-flight set = %s, want UNKNOWN for a turn with no resolvable identity", got.Summary())
	}
	if !strings.Contains(got.Reason(), "cannot be identified") {
		t.Fatalf("reason = %q, want it to say the turn cannot be identified", got.Reason())
	}
}

// TestInFlightIsUnknownWhenTheTurnClaimReadFails covers the turn plane's read
// failure.
func TestInFlightIsUnknownWhenTheTurnClaimReadFails(t *testing.T) {
	// Arrange
	h := inflightHarness(t)
	h.applier.setActiveTurnIDsErr(errors.New("claim ledger unreadable"))

	// Act
	got := h.m.InFlight("ws")

	// Assert
	if got.Known() {
		t.Fatalf("in-flight set = %s, want UNKNOWN when the claim ledger cannot be read", got.Summary())
	}
}

// TestInFlightAddsNoQueryToAnEmptySet pins the deliberate narrowing: a shim
// holds one query for its whole life, so admitting it unconditionally would
// make every wired workspace permanently non-empty and disable hibernation,
// the idle sweep and every close for the whole fleet.
func TestInFlightAddsNoQueryToAnEmptySet(t *testing.T) {
	// Arrange
	h := inflightHarness(t)

	// Act
	got := h.m.InFlight("ws")

	// Assert
	if len(got.OfKind(inflight.KindQuery)) != 0 {
		t.Fatalf("in-flight set = %s, want no query member on a workspace holding no work", got.Summary())
	}
}

// TestInFlightRefusesAnEmptyWorkspace covers the validation refusal, which is
// an UNKNOWN like every other missing evidence rather than a panic or a pass.
func TestInFlightRefusesAnEmptyWorkspace(t *testing.T) {
	// Arrange
	h := inflightHarness(t)

	// Act
	got := h.m.InFlight("")

	// Assert
	if got.Known() {
		t.Fatal("InFlight answered for an empty workspace")
	}
}

// settledHarness is a manager whose workspace resolves SETTLED — no turn, a
// green state — so the only thing that can refuse its hibernation lease is the
// in-flight set. It uses the gate harness because that one has no controller
// registration standing in the way of the lease.
func settledHarness(t *testing.T) *gateHarness {
	t.Helper()
	h := newGateHarness(t, &fakeWorkspaceLock{answers: []bool{false}})
	h.applier.setCurrent("ws", &frontendv1.WorkspaceState{
		Workspace: "ws",
		SessionId: "s1",
		State:     frontendv1.RenderState_RENDER_STATE_IDLE,
	})
	return h
}

// TestHibernationIsRefusedWhileAsyncWorkRuns is the settledness guard's own
// regression: the workspace reads a settled state and holds no turn, and only
// the in-flight set can see the detached work that would die with the shim.
func TestHibernationIsRefusedWhileAsyncWorkRuns(t *testing.T) {
	// Arrange
	h := settledHarness(t)
	h.applier.setLiveTasks(0, "task-1")

	// Act
	_, err := h.m.acquireSettledHibernationLease("ws")

	// Assert
	if !errors.Is(err, ErrNotSettled) {
		t.Fatalf("acquireSettledHibernationLease err = %v, want ErrNotSettled while a background task runs", err)
	}
	if !strings.Contains(err.Error(), "task:task-1") {
		t.Fatalf("refusal = %v, want it to name the work it is protecting", err)
	}
}

// TestHibernationIsRefusedWhenTheInFlightSetIsUnknown pins that silence is not
// a licence: an unreadable task set refuses the teardown exactly as a live task
// does.
func TestHibernationIsRefusedWhenTheInFlightSetIsUnknown(t *testing.T) {
	// Arrange
	h := settledHarness(t)
	h.applier.setLiveTaskIDsErr(errors.New("task rows unreadable"))

	// Act
	_, err := h.m.acquireSettledHibernationLease("ws")

	// Assert
	if !errors.Is(err, ErrNotSettled) {
		t.Fatalf("acquireSettledHibernationLease err = %v, want ErrNotSettled on an UNKNOWN in-flight set", err)
	}
}

// TestHibernationIsGrantedForAWorkspaceHoldingNothing pins that the new gate
// refuses only what it must — a gate that always refuses is a gate nobody keeps.
func TestHibernationIsGrantedForAWorkspaceHoldingNothing(t *testing.T) {
	// Arrange
	h := settledHarness(t)

	// Act
	release, err := h.m.acquireSettledHibernationLease("ws")

	// Assert
	if err != nil {
		t.Fatalf("acquireSettledHibernationLease err = %v, want the lease granted for a quiet workspace", err)
	}
	release()
}

// TestLiveTaskSetIsAnsweredEmptyForAQuietWorkspace covers the arm that licenses
// a drain to stop waiting on background work.
func TestLiveTaskSetIsAnsweredEmptyForAQuietWorkspace(t *testing.T) {
	// Arrange
	h := inflightHarness(t)

	// Act
	got := h.m.LiveTaskSet("ws")

	// Assert
	if !got.Known() || len(got.Items()) != 0 {
		t.Fatalf("LiveTaskSet = %s, want an answered empty set", got.Summary())
	}
}

// TestLiveTaskSetSeparatesAnUnknownWorkspaceFromAQuietOne is the miss
// distinction the old (count, ok) pair drew and the set must keep drawing: a
// workspace nothing has ever reported on is UNKNOWN, not empty.
func TestLiveTaskSetSeparatesAnUnknownWorkspaceFromAQuietOne(t *testing.T) {
	// Arrange
	h := inflightHarness(t)

	// Act
	got := h.m.LiveTaskSet("ws-never-seen")

	// Assert
	if got.Known() {
		t.Fatalf("LiveTaskSet = %s for an unheard-of workspace, want the UNKNOWN arm", got.Summary())
	}
}

// TestLiveTaskSetNamesTheLiveTasks covers the identities a hold reports.
func TestLiveTaskSetNamesTheLiveTasks(t *testing.T) {
	// Arrange
	h := inflightHarness(t)
	h.applier.setLiveTasks(0, "task-1")

	// Act
	got := h.m.LiveTaskSet("ws")

	// Assert
	if !got.Has(inflight.Item{Kind: inflight.KindTask, ID: "task-1"}) {
		t.Fatalf("LiveTaskSet = %s, want the live task named", got.Summary())
	}
}

// TestLiveTaskSetIsUnknownWhenALiveStartCarriedNoIdentity covers the poisoning
// rule: work is running that cannot be named, so the honest answer about the
// SET is that it is not known.
func TestLiveTaskSetIsUnknownWhenALiveStartCarriedNoIdentity(t *testing.T) {
	// Arrange
	h := inflightHarness(t)
	h.applier.setLiveTasks(1, "task-1")

	// Act
	got := h.m.LiveTaskSet("ws")

	// Assert
	if got.Known() {
		t.Fatalf("LiveTaskSet = %s under an anonymous live start, want the UNKNOWN arm", got.Summary())
	}
}

// TestLiveTaskSetIsUnknownWhenTheResolvedStateCannotBeRead covers the read
// failure, which must surface as UNKNOWN rather than as an empty set.
func TestLiveTaskSetIsUnknownWhenTheResolvedStateCannotBeRead(t *testing.T) {
	// Arrange
	h := inflightHarness(t)
	h.applier.setCurrentErr(errors.New("state store is gone"))

	// Act
	got := h.m.LiveTaskSet("ws")

	// Assert
	if got.Known() || !strings.Contains(got.Reason(), "state store is gone") {
		t.Fatalf("LiveTaskSet = %s, want UNKNOWN carrying the read failure", got.Summary())
	}
}
