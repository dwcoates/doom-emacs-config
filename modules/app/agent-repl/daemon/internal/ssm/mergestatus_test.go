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

func TestATransitionWithNoPublishedStatusStillResolvesTheMergingState(t *testing.T) {
	// REWRITTEN off the retired flat merge_phase field: the coarse phase word is
	// gone from the wire, but the merge AXIS that produced it is not, and the
	// axis is what makes a merge with no published run visible at all. Asserting
	// the resolved RenderState is asserting the same guarantee against the
	// surface that still carries it.
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})

	// Act.
	applyPhases(t, m, "ws1", merge.PhaseMerging)

	// Assert.
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_MERGING {
		t.Fatalf("state = %v where merge_status is absent, want RENDER_STATE_MERGING from the axis", got)
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

func TestMergeStatusRidesTheAxisThatResolvedTheFrame(t *testing.T) {
	// REWRITTEN off the retired flat merge_phase field, which this used to assert
	// coexisted with merge_status. There is no second form to coexist with any
	// more; what the cutover must preserve is that a published status arrives on
	// a frame the merge axis also resolved, so the run's progress and the state
	// it is progressing through cannot disagree.
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})

	// Act.
	if err := m.ApplyMergeStatus("ws1", string(merge.PhaseMerging), "cherry-picking 1/1",
		pipelineStatus("run-both", 1, 0, "deadbeef1234", "the only commit")); err != nil {
		t.Fatalf("ApplyMergeStatus: %v", err)
	}

	// Assert.
	got := mustCurrent(t, m, "ws1")
	if got.GetState() != frontendv1.RenderState_RENDER_STATE_MERGING {
		t.Fatalf("state = %v alongside a published merge_status, want RENDER_STATE_MERGING", got.GetState())
	}
	if got.GetMergeStatus().GetRunId() != "run-both" {
		t.Fatalf("run_id = %q, want the published run on the same frame the axis resolved", got.GetMergeStatus().GetRunId())
	}
}
