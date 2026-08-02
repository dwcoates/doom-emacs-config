package ssm

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
	"claude-repld/internal/workspace/merge"
)

// pipelineStatus builds a cherry_picking status the merge pipeline would
// publish.
func pipelineStatus(runID string, total, landed int32, sha, subject string) *frontendv1.MergeStatus {
	return &frontendv1.MergeStatus{
		RunId:            runID,
		PhaseStartedAtMs: 1000,
		UpdatedAtMs:      1001,
		Phase: &frontendv1.MergeStatus_CherryPicking{CherryPicking: &frontendv1.MergeStatusCherryPicking{
			CommitsTotal:   total,
			CommitsLanded:  landed,
			CurrentSha:     sha,
			CurrentSubject: subject,
		}},
	}
}

// THE GUARANTEE: a status the pipeline publishes reaches the wire verbatim,
// stamped through the ONE WorkspaceState construction funnel rather than around
// it.
func TestApplyMergeStatusStampsThePipelinesOwnAccount(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	want := pipelineStatus("run-abc", 7, 3, "abc123def456", "fix the parser")

	// Act.
	if err := m.ApplyMergeStatus("ws1", string(merge.PhaseMerging), "cherry-picking 4/7", want); err != nil {
		t.Fatalf("ApplyMergeStatus: %v", err)
	}

	// Assert.
	got := mustCurrent(t, m, "ws1").GetMergeStatus()
	if got == nil {
		t.Fatal("the pushed WorkspaceState carries no merge_status")
	}
	if got.GetRunId() != "run-abc" {
		t.Fatalf("run_id = %q, want the pipeline's run-abc", got.GetRunId())
	}
	pick := got.GetCherryPicking()
	if pick == nil {
		t.Fatalf("phase = %T, want cherry_picking", got.GetPhase())
	}
	if pick.GetCommitsTotal() != 7 || pick.GetCommitsLanded() != 3 {
		t.Fatalf("total/landed = %d/%d, want 7/3", pick.GetCommitsTotal(), pick.GetCommitsLanded())
	}
	if pick.GetCurrentSubject() != "fix the parser" {
		t.Fatalf("current_subject = %q, want the plan's subject", pick.GetCurrentSubject())
	}
}

// A plain transition NEVER stands in for a run. The state log has no run
// identity, no plan size and no commit in it, so a status projected from it
// could only report a run nothing minted — and once the pipeline does publish,
// its account is the only one on the wire.
func TestAPlainTransitionNeverStandsInForThePipelinesAccount(t *testing.T) {
	// Arrange — a plain transition first, which nothing published a status for.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	if err := m.ApplyMergeTransition("ws1", string(merge.PhaseMerging), "cherry-pick starting"); err != nil {
		t.Fatalf("ApplyMergeTransition: %v", err)
	}
	if got := mustCurrent(t, m, "ws1").GetMergeStatus(); got != nil {
		t.Fatalf("merge_status = %v before the pipeline published one, want none", got)
	}

	// Act.
	if err := m.ApplyMergeStatus("ws1", string(merge.PhaseMerging), "cherry-picking 1/1",
		pipelineStatus("run-xyz", 1, 0, "deadbeef1234", "the only commit")); err != nil {
		t.Fatalf("ApplyMergeStatus: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").GetMergeStatus().GetRunId(); got != "run-xyz" {
		t.Fatalf("run_id = %q, want the pipeline's run-xyz", got)
	}
}

// THE VIOLATION EDGE: clearing the merge axis drops the retained run. The run
// is over, and a frontend still rendering its progress would be rendering a
// merge nothing is advancing.
func TestClearingTheMergeAxisDropsTheRetainedRunStatus(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	// A session-status row so the workspace still resolves once the merge axis
	// is gone; without one it has no render-bearing signal at all.
	settledSession(t, m, "ws1", "s1")
	if err := m.ApplyMergeStatus("ws1", string(merge.PhaseMerging), "cherry-picking 1/1",
		pipelineStatus("run-xyz", 1, 0, "deadbeef1234", "the only commit")); err != nil {
		t.Fatalf("ApplyMergeStatus: %v", err)
	}

	// Act.
	if err := m.ApplyMergeTransition("ws1", "", "the run is over"); err != nil {
		t.Fatalf("ApplyMergeTransition(clear): %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").GetMergeStatus(); got != nil {
		t.Fatalf("merge_status = %+v after the axis was cleared, want none", got)
	}
}

// A nil status is refused: ApplyMergeStatus exists to carry one, and a caller
// that lost it wanted ApplyMergeTransition.
func TestApplyMergeStatusRefusesANilStatus(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})

	// Act.
	err := m.ApplyMergeStatus("ws1", string(merge.PhaseMerging), "cherry-picking", nil)

	// Assert.
	if err == nil {
		t.Fatal("ApplyMergeStatus(nil) error = nil, want the nil status refused")
	}
}

// A SECOND `merged` row — which the pipeline writes when it finishes the
// after-action so the terminal status can carry its outcome — must not stand
// the session down again. The first teardown already took it.
func TestASecondMergedRowDoesNotTearTheSessionDownAgain(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	authority := &fakeSessionAuthority{}
	if err := m.bindMergedTeardown(authority); err != nil {
		t.Fatalf("bindMergedTeardown: %v", err)
	}
	if err := m.ApplyMergeTransition("ws1", string(merge.PhaseMerged), "cherry-pick landed"); err != nil {
		t.Fatalf("first merged: %v", err)
	}

	// Act.
	if err := m.ApplyMergeTransition("ws1", string(merge.PhaseMerged), "the after-action finished"); err != nil {
		t.Fatalf("second merged: %v", err)
	}

	// Assert.
	if got := len(authority.tornDown); got != 1 {
		t.Fatalf("the session was stood down %d times, want exactly 1", got)
	}
}
