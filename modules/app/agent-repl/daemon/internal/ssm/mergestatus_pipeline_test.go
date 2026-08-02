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

// --- the merge-progress push key ---------------------------------------------
//
// Every row below is a publication that does NOT move the render state, which
// is the whole reason the key exists: the run's middle — every pick, every test
// gate, and the second `merged` carrying the after-action's outcome — resolves
// to a state the previous frame already reported.

// drainStatuses reads every merge_status a subscriber has been handed for ws
// without blocking, in arrival order.
func drainStatuses(states <-chan *frontendv1.WorkspaceState, ws string) []*frontendv1.MergeStatus {
	var out []*frontendv1.MergeStatus
	for {
		select {
		case msg := <-states:
			if msg.GetWorkspace() != ws {
				continue
			}
			if status := msg.GetMergeStatus(); status != nil {
				out = append(out, status)
			}
		default:
			return out
		}
	}
}

// testingStatus builds the testing status the gate publishes for the commit
// that just landed.
func testingStatus(runID string, total, landed int32, sha string) *frontendv1.MergeStatus {
	return &frontendv1.MergeStatus{
		RunId:            runID,
		PhaseStartedAtMs: 2000,
		UpdatedAtMs:      2001,
		Phase: &frontendv1.MergeStatus_Testing{Testing: &frontendv1.MergeStatusTesting{
			CommitsTotal:  total,
			CommitsLanded: landed,
			CurrentSha:    sha,
		}},
	}
}

// mergedStatus builds a terminal merged status.
func mergedStatus(runID string, total int32, afterActionErr string, updatedAt int64) *frontendv1.MergeStatus {
	return &frontendv1.MergeStatus{
		RunId:            runID,
		PhaseStartedAtMs: 3000,
		UpdatedAtMs:      updatedAt,
		Phase: &frontendv1.MergeStatus_Merged{Merged: &frontendv1.MergeStatusMerged{
			CommitsTotal:     total,
			AfterActionError: afterActionErr,
		}},
	}
}

// THE GUARANTEE: a phase change within one render state is pushed. cherry_picking
// and testing both resolve to `merging`, so a push keyed on the render state
// alone drops the testing arm entirely and a frontend never learns the suite ran.
func TestAStatusPhaseChangeWithinOneRenderStateIsPushed(t *testing.T) {
	// Arrange — a run already picking, so the render state is settled on merging.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	if err := m.ApplyMergeStatus("ws1", string(merge.PhaseMerging), "cherry-picking 1/3",
		pipelineStatus("run-1", 3, 0, "aaaa", "first")); err != nil {
		t.Fatalf("cherry_picking: %v", err)
	}
	states, cancel := m.Subscribe()
	t.Cleanup(cancel)

	// Act — the same render state, a different phase.
	if err := m.ApplyMergeStatus("ws1", string(merge.PhaseMerging), "testing 1/3",
		testingStatus("run-1", 3, 1, "aaaa")); err != nil {
		t.Fatalf("testing: %v", err)
	}

	// Assert.
	got := drainStatuses(states, "ws1")
	if len(got) != 1 {
		t.Fatalf("the testing phase produced %d pushes, want 1: cherry_picking and testing share the `merging` render state, so a state-keyed push drops it", len(got))
	}
	if got[0].GetTesting() == nil {
		t.Fatalf("the pushed status carries phase %T, want testing", got[0].GetPhase())
	}
}

// A within-phase TICK is pushed too: the pick cursor advancing is the progress a
// user is watching, and it never changes either the arm or the render state.
func TestAWithinPhaseProgressTickIsPushed(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	if err := m.ApplyMergeStatus("ws1", string(merge.PhaseMerging), "cherry-picking 1/3",
		pipelineStatus("run-1", 3, 0, "aaaa", "first")); err != nil {
		t.Fatalf("first pick: %v", err)
	}
	states, cancel := m.Subscribe()
	t.Cleanup(cancel)

	// Act — the same arm, a moved cursor.
	if err := m.ApplyMergeStatus("ws1", string(merge.PhaseMerging), "cherry-picking 2/3",
		pipelineStatus("run-1", 3, 1, "bbbb", "second")); err != nil {
		t.Fatalf("second pick: %v", err)
	}

	// Assert.
	got := drainStatuses(states, "ws1")
	if len(got) != 1 {
		t.Fatalf("the advanced pick cursor produced %d pushes, want 1", len(got))
	}
	if landed := got[0].GetCherryPicking().GetCommitsLanded(); landed != 1 {
		t.Fatalf("the pushed status reports commits_landed=%d, want 1", landed)
	}
}

// The SECOND `merged` — republished once the after-action has run — must reach
// the wire, because it is the only frame carrying after_action_error. The first
// already put the workspace in the merged render state, so nothing but the
// status itself has moved.
func TestTheSecondMergedStatusCarryingTheAfterActionErrorIsPushed(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	if err := m.ApplyMergeStatus("ws1", string(merge.PhaseMerged), "the merge landed",
		mergedStatus("run-1", 2, "", 3001)); err != nil {
		t.Fatalf("first merged: %v", err)
	}
	states, cancel := m.Subscribe()
	t.Cleanup(cancel)

	// Act.
	if err := m.ApplyMergeStatus("ws1", string(merge.PhaseMerged), "the merge landed",
		mergedStatus("run-1", 2, "the after-action turn errored", 3002)); err != nil {
		t.Fatalf("second merged: %v", err)
	}

	// Assert.
	got := drainStatuses(states, "ws1")
	if len(got) != 1 {
		t.Fatalf("the after-action outcome produced %d pushes, want 1: without it the failure is swallowed", len(got))
	}
	if errText := got[0].GetMerged().GetAfterActionError(); errText != "the after-action turn errored" {
		t.Fatalf("after_action_error = %q, want the after-action's failure", errText)
	}
}

// An UNCHANGED status is not republished. The pipeline builds a fresh
// MergeStatus per publication, so a pointer-keyed push would fire on every
// re-resolve and report progress the run did not make.
func TestAnUnchangedStatusIsNotRepublished(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	if err := m.ApplyMergeStatus("ws1", string(merge.PhaseMerging), "cherry-picking 1/3",
		pipelineStatus("run-1", 3, 0, "aaaa", "first")); err != nil {
		t.Fatalf("first pick: %v", err)
	}
	states, cancel := m.Subscribe()
	t.Cleanup(cancel)

	// Act — the identical status, rebuilt.
	if err := m.ApplyMergeStatus("ws1", string(merge.PhaseMerging), "cherry-picking 1/3",
		pipelineStatus("run-1", 3, 0, "aaaa", "first")); err != nil {
		t.Fatalf("republish: %v", err)
	}

	// Assert.
	if got := drainStatuses(states, "ws1"); len(got) != 0 {
		t.Fatalf("an unchanged status produced %d pushes, want 0", len(got))
	}
}
