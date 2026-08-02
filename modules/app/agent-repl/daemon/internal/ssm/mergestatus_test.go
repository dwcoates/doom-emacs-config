package ssm

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/workspace/merge"
)

// WorkspaceState.merge_status, the coarse projection of the merge axis. Each
// test covers ONE mapping arm, plus absence and the cause's routing prefix.
//
// Every case reads the status off mustCurrent rather than off the projection
// helper directly: the whole claim being made is that the ONE WorkspaceState
// construction funnel stamps it, so a test that called the mapper would pass
// with the funnel unwired.

// mustMergeStatus returns the workspace's stamped merge status, failing when
// the frame carries none.
func mustMergeStatus(t *testing.T, m *Manager, ws string) *frontendv1.MergeStatus {
	t.Helper()
	status := mustCurrent(t, m, ws).GetMergeStatus()
	if status == nil {
		t.Fatalf("WorkspaceState for %s carries no merge_status", ws)
	}
	return status
}

func TestMergeStatusIsAbsentWithoutAMergeAxis(t *testing.T) {
	// Arrange — a workspace with a session-status row and no merge history.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	// Act.
	got := mustCurrent(t, m, "ws1")

	// Assert — absence is the absence of a merge, never a zero-valued status.
	if got.GetMergeStatus() != nil {
		t.Fatalf("merge_status = %v, want none for a workspace that has never merged", got.GetMergeStatus())
	}
}

func TestMergeStatusIsAbsentOnAClearedMergeAxis(t *testing.T) {
	// Arrange — a merge attempt that was cleared off the axis again.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("Apply: %v", err)
	}
	applyPhases(t, m, "ws1", merge.PhaseMergeEnqueuing)
	if err := m.ApplyMergeTransition("ws1", sigMergeNone, "test arrangement"); err != nil {
		t.Fatalf("ApplyMergeTransition(merge_none): %v", err)
	}

	// Act.
	got := mustCurrent(t, m, "ws1")

	// Assert.
	if got.GetMergeStatus() != nil {
		t.Fatalf("merge_status = %v, want none for a cleared merge axis", got.GetMergeStatus())
	}
}

func TestMergeEnqueuingMapsToTheEnqueuedPhase(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	applyPhases(t, m, "ws1", merge.PhaseMergeEnqueuing)

	// Act.
	got := mustMergeStatus(t, m, "ws1")

	// Assert.
	if got.GetEnqueued() == nil {
		t.Fatalf("merge_status phase = %T, want enqueued", got.GetPhase())
	}
}

func TestMergeQueuedMapsToTheEnqueuedPhase(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	applyPhases(t, m, "ws1", merge.PhaseMergeQueued)

	// Act.
	got := mustMergeStatus(t, m, "ws1")

	// Assert.
	if got.GetEnqueued() == nil {
		t.Fatalf("merge_status phase = %T, want enqueued", got.GetPhase())
	}
}

func TestTheEnqueuedPhaseCarriesTheQueuePlace(t *testing.T) {
	// Arrange — the SAME queue snapshot that fills merge_queue_position/depth.
	m, _, q, _, _ := openLeaseTest(t, "ws1")
	q.snapshot = map[string][]merge.Request{
		"/repo": {
			{Workspace: "ws-ahead", Name: "ahead"},
			{Workspace: "ws1", Name: "one"},
		},
	}
	applyPhases(t, m, "ws1", merge.PhaseMergeQueued)

	// Act.
	got := mustMergeStatus(t, m, "ws1").GetEnqueued()

	// Assert — 1-based place, and the depth of the whole repository's queue.
	if got.GetPosition() != 2 || got.GetDepth() != 2 {
		t.Fatalf("enqueued = (position=%d depth=%d), want (2, 2)", got.GetPosition(), got.GetDepth())
	}
}

func TestMergingMapsToTheCherryPickingPhase(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	applyPhases(t, m, "ws1", merge.PhaseMerging)

	// Act.
	got := mustMergeStatus(t, m, "ws1")

	// Assert.
	if got.GetCherryPicking() == nil {
		t.Fatalf("merge_status phase = %T, want cherry_picking", got.GetPhase())
	}
}

func TestTheCherryPickingPhaseClaimsNoCommitCountsItCannotKnow(t *testing.T) {
	// The current pipeline picks commits without ever reporting how many there
	// are, so a non-zero figure here would be invented rather than observed.
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	applyPhases(t, m, "ws1", merge.PhaseMerging)

	// Act.
	got := mustMergeStatus(t, m, "ws1").GetCherryPicking()

	// Assert.
	if got.GetCommitsTotal() != 0 || got.GetCommitsLanded() != 0 {
		t.Fatalf("cherry_picking counts = (%d, %d), want (0, 0)", got.GetCommitsTotal(), got.GetCommitsLanded())
	}
}

func TestMergeConflictMapsToTheConflictPhase(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	applyPhases(t, m, "ws1", merge.PhaseMergeConflict)

	// Act.
	got := mustMergeStatus(t, m, "ws1")

	// Assert.
	if got.GetConflict() == nil {
		t.Fatalf("merge_status phase = %T, want conflict", got.GetPhase())
	}
}

func TestMergedMapsToTheMergedPhase(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	applyPhases(t, m, "ws1", merge.PhaseMerged)

	// Act.
	got := mustMergeStatus(t, m, "ws1")

	// Assert.
	if got.GetMerged() == nil {
		t.Fatalf("merge_status phase = %T, want merged", got.GetPhase())
	}
}

func TestMergeFailedMapsToTheFailedPhase(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	applyPhases(t, m, "ws1", merge.PhaseMergeFailed)

	// Act.
	got := mustMergeStatus(t, m, "ws1")

	// Assert.
	if got.GetFailed() == nil {
		t.Fatalf("merge_status phase = %T, want failed", got.GetPhase())
	}
}

func TestTheFailedCauseDropsItsRoutingPrefix(t *testing.T) {
	// The state log routes a merge row by prefixing its cause kind. The prefix
	// is the daemon's own bookkeeping, so reporting it in front of the reason
	// would tell the user their merge failed because of "merge_transition".
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.ApplyMergeTransition("ws1", string(merge.PhaseMergeFailed), "merge enqueue refused: disk full"); err != nil {
		t.Fatalf("ApplyMergeTransition: %v", err)
	}

	// Act.
	got := mustMergeStatus(t, m, "ws1").GetFailed()

	// Assert.
	if got.GetCause() != "merge enqueue refused: disk full" {
		t.Fatalf("failed cause = %q, want the note without the routing prefix", got.GetCause())
	}
}

func TestAFailureRecordedWithNoNoteCarriesNoCause(t *testing.T) {
	// Arrange — a transition whose cause kind is the bare routing token.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.ApplyMergeTransition("ws1", string(merge.PhaseMergeFailed), ""); err != nil {
		t.Fatalf("ApplyMergeTransition: %v", err)
	}

	// Act.
	got := mustMergeStatus(t, m, "ws1").GetFailed()

	// Assert — an empty cause, never the routing token standing in for one.
	if got.GetCause() != "" {
		t.Fatalf("failed cause = %q, want empty for a transition recorded with no note", got.GetCause())
	}
}

func TestMergeStatusReportsTheInstantThePhaseWasEntered(t *testing.T) {
	// Arrange — a superseded phase, so the newest row is the one that must win.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	applyPhases(t, m, "ws1", merge.PhaseMergeEnqueuing, merge.PhaseMerging)

	// Act.
	got := mustMergeStatus(t, m, "ws1")

	// Assert — the state's own at_ms is the merging row's, and the phase agrees.
	if got.GetPhaseStartedAtMs() != mustCurrent(t, m, "ws1").GetAtMs() {
		t.Fatalf("phase_started_at_ms = %d, want the newest merge row's instant (%d)",
			got.GetPhaseStartedAtMs(), mustCurrent(t, m, "ws1").GetAtMs())
	}
}

func TestMergeStatusReportsNoTickAheadOfItsPhase(t *testing.T) {
	// There are no within-phase ticks to report until the merge pipeline
	// produces them, so the two instants are deliberately identical.
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	applyPhases(t, m, "ws1", merge.PhaseMerging)

	// Act.
	got := mustMergeStatus(t, m, "ws1")

	// Assert.
	if got.GetUpdatedAtMs() != got.GetPhaseStartedAtMs() {
		t.Fatalf("updated_at_ms = %d, want the phase instant %d", got.GetUpdatedAtMs(), got.GetPhaseStartedAtMs())
	}
}

func TestMergeStatusAlwaysCarriesARunID(t *testing.T) {
	// A status with no run id is one no reader can correlate at all; the
	// placeholder is derived rather than left empty until the pipeline mints a
	// real one.
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	applyPhases(t, m, "ws1", merge.PhaseMerging)

	// Act.
	got := mustMergeStatus(t, m, "ws1")

	// Assert.
	if got.GetRunId() == "" {
		t.Fatal("merge_status carries an empty run_id")
	}
}

func TestMergeStatusRunIDSeparatesWorkspaces(t *testing.T) {
	// Arrange — two workspaces on the same phase.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1", "s2": "ws2"})
	applyPhases(t, m, "ws1", merge.PhaseMerging)
	applyPhases(t, m, "ws2", merge.PhaseMerging)

	// Act.
	one := mustMergeStatus(t, m, "ws1").GetRunId()
	two := mustMergeStatus(t, m, "ws2").GetRunId()

	// Assert — one id naming two workspaces' runs would collapse them in any
	// reader that keys on it.
	if one == two {
		t.Fatalf("both workspaces report run_id %q", one)
	}
}

func TestMergeStatusRidesTheOldMergeFieldsRatherThanReplacingThem(t *testing.T) {
	// Both forms coexist until the cutover wave, so a frontend that has not
	// moved yet keeps reading the phase it always read.
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	applyPhases(t, m, "ws1", merge.PhaseMerging)

	// Act.
	got := mustCurrent(t, m, "ws1")

	// Assert.
	if got.GetMergePhase() != string(merge.PhaseMerging) {
		t.Fatalf("merge_phase = %q, want it still stamped alongside merge_status", got.GetMergePhase())
	}
}
