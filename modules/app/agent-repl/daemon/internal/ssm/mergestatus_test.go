package ssm

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/workspace/merge"
)

// WorkspaceState.merge_status, which ONLY the merge pipeline produces.
//
// The wave-0 projection over the merge axis is gone: it minted a placeholder
// run_id per state-log row, so one run published a different id at every phase.
// What is asserted here is the replacement contract — a transition the pipeline
// did not publish a status for carries NO status, and every status on the wire
// is the one the run handed in.
//
// Every case reads the status off mustCurrent rather than off a helper directly:
// the claim being made is that the ONE WorkspaceState construction funnel stamps
// it, so a test that called the stamper would pass with the funnel unwired.

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

func TestATransitionWithNoPublishedStatusCarriesNoMergeStatus(t *testing.T) {
	// THE PLACEHOLDER'S GRAVE. A merge-axis row on its own says which phase the
	// workspace is in and nothing about which run is in it, so a status stamped
	// from it could only carry an id nothing minted.
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})

	// Act.
	applyPhases(t, m, "ws1", merge.PhaseMerging)

	// Assert.
	if got := mustCurrent(t, m, "ws1").GetMergeStatus(); got != nil {
		t.Fatalf("merge_status = %v for a transition the pipeline published no status for, want none", got)
	}
}

func TestATransitionWithNoPublishedStatusStillCarriesTheMergePhase(t *testing.T) {
	// The coarse phase is what reports such a transition, which is why leaving
	// merge_status unset loses nothing a frontend was already reading.
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})

	// Act.
	applyPhases(t, m, "ws1", merge.PhaseMerging)

	// Assert.
	if got := mustCurrent(t, m, "ws1").GetMergePhase(); got != string(merge.PhaseMerging) {
		t.Fatalf("merge_phase = %q, want it still stamped where merge_status is absent", got)
	}
}

func TestTheRetainedStatusRidesEveryLaterFrame(t *testing.T) {
	// A run publishes at its phase edges, and the frames pushed BETWEEN them (a
	// connectivity edge, a turn) must still report the run rather than dropping
	// it until the next phase.
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.ApplyMergeStatus("ws1", string(merge.PhaseMerging), "cherry-picking 1/1",
		pipelineStatus("run-retained", 1, 0, "deadbeef1234", "the only commit")); err != nil {
		t.Fatalf("ApplyMergeStatus: %v", err)
	}

	// Act — an unrelated push for the same workspace.
	if err := m.Apply(evSessionStarted("s1", 2)); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	// Assert.
	if got := mustMergeStatus(t, m, "ws1").GetRunId(); got != "run-retained" {
		t.Fatalf("run_id = %q on a frame between phase edges, want the retained run-retained", got)
	}
}

func TestThePublishedStatusReachesTheWireVerbatim(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	want := pipelineStatus("run-verbatim", 4, 2, "cafebabe0000", "land the thing")

	// Act.
	if err := m.ApplyMergeStatus("ws1", string(merge.PhaseMerging), "cherry-picking 3/4", want); err != nil {
		t.Fatalf("ApplyMergeStatus: %v", err)
	}

	// Assert — the run's own timestamps, not the state log's row instant.
	got := mustMergeStatus(t, m, "ws1")
	if got.GetPhaseStartedAtMs() != want.GetPhaseStartedAtMs() || got.GetUpdatedAtMs() != want.GetUpdatedAtMs() {
		t.Fatalf("timestamps = (%d, %d), want the run's own (%d, %d)",
			got.GetPhaseStartedAtMs(), got.GetUpdatedAtMs(), want.GetPhaseStartedAtMs(), want.GetUpdatedAtMs())
	}
}

func TestMergeStatusRidesTheOldMergeFieldsRatherThanReplacingThem(t *testing.T) {
	// Both forms coexist until the cutover wave, so a frontend that has not
	// moved yet keeps reading the phase it always read.
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})

	// Act.
	if err := m.ApplyMergeStatus("ws1", string(merge.PhaseMerging), "cherry-picking 1/1",
		pipelineStatus("run-both", 1, 0, "deadbeef1234", "the only commit")); err != nil {
		t.Fatalf("ApplyMergeStatus: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").GetMergePhase(); got != string(merge.PhaseMerging) {
		t.Fatalf("merge_phase = %q, want it still stamped alongside merge_status", got)
	}
}
