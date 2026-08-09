package ssm

import (
	"strings"
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
	if err := applyTest(m, evSessionStarted("s1", 1)); err != nil {
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
	if err := applyTest(m, evSessionStarted("s1", 1)); err != nil {
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
	if err := applyTest(m, evSessionStarted("s1", 2)); err != nil {
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

// --- a stopped merge under a live turn ---------------------------------------
//
// THE STALE MERGE ON A WORKING SESSION. `merge_failed` never leaves the axis on
// its own, and the composite resolution deliberately hands a live turn the win
// over it (a terminal merge owns nothing, so it may not mask a turn the user
// just started). merge_status was stamped off the AXIS alone, so the wire
// carried state=SUBMITTING beside merge_status=failed — and every surface that
// reads the status renders it in preference to the phase word, which is why the
// footer and the sidebar went on reporting a merge that had stopped while the
// agent answered the prompt underneath them.

// failedPipelineStatus is a stopped run's own account of itself.
func failedPipelineStatus(runID, cause string) *frontendv1.MergeStatus {
	return &frontendv1.MergeStatus{
		RunId:            runID,
		PhaseStartedAtMs: 2000,
		UpdatedAtMs:      2001,
		Phase: &frontendv1.MergeStatus_Failed{Failed: &frontendv1.MergeStatusFailed{
			Cause:          cause,
			CommitsTotal:   3,
			CommitsLanded:  1,
			FailingSha:     "feedface0000",
			FailingSubject: "the commit that stopped the run",
		}},
	}
}

// openStoppedMerge arranges an operational workspace resting on a published
// `merge_failed` run.
func openStoppedMerge(t *testing.T, runID string) (*Manager, *capLog) {
	t.Helper()
	m, cl, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	connectOperational(t, m, "ws1", "s1", "g1")
	if err := m.ApplyMergeStatus("ws1", string(merge.PhaseMergeFailed), "cherry-pick refused",
		failedPipelineStatus(runID, "cherry-pick refused")); err != nil {
		t.Fatalf("ApplyMergeStatus(merge_failed): %v", err)
	}
	return m, cl
}

func TestAStoppedMergesStatusRidesTheFrameThatStillResolvesToIt(t *testing.T) {
	// The other half of the guarantee: withholding is about the FRAME, so an
	// idle workspace still reports the failure it rests on.
	// Arrange.
	m, _ := openStoppedMerge(t, "run-idle")

	// Act.
	got := mustCurrent(t, m, "ws1")

	// Assert.
	if got.GetState() != frontendv1.RenderState_RENDER_STATE_MERGE_FAILED {
		t.Fatalf("state = %v on a workspace with no live turn, want RENDER_STATE_MERGE_FAILED", got.GetState())
	}
	if got.GetMergeStatus().GetRunId() != "run-idle" {
		t.Fatalf("run_id = %q, want run-idle on the frame the merge axis resolved", got.GetMergeStatus().GetRunId())
	}
}

func TestAStoppedMergesStatusIsWithheldOnceThePromptIsAccepted(t *testing.T) {
	// THE EDGE THE USER SEES FIRST. The accepted prompt is published
	// synchronously, so this frame is the one standing on screen from the
	// keystroke onward — it is exactly the frame the failure must not ride.
	// Arrange.
	m, _ := openStoppedMerge(t, "run-accepted")

	// Act.
	var published *frontendv1.WorkspaceState
	// PromptAdmissionUser: the prompt this test is about is a person's, which is
	// also what makes the withheld failure matter — it is what they are looking
	// at when they send it.
	if err := m.MarkPromptAccepted("ws1", "s1", "req-1", PromptAdmissionUser, func(state *frontendv1.WorkspaceState) {
		published = state
	}); err != nil {
		t.Fatalf("MarkPromptAccepted: %v", err)
	}

	// Assert.
	if published.GetState() != frontendv1.RenderState_RENDER_STATE_SUBMITTING {
		t.Fatalf("state = %v on the accepted frame, want RENDER_STATE_SUBMITTING", published.GetState())
	}
	if published.GetMergeStatus() != nil {
		t.Fatalf("merge_status = %v on a SUBMITTING frame, want none", published.GetMergeStatus())
	}
}

func TestAStoppedMergesStatusIsWithheldWhileTheAgentThinks(t *testing.T) {
	// Arrange.
	m, _ := openStoppedMerge(t, "run-thinking")

	// Act.
	if err := applyTest(m, evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("Apply(turn started): %v", err)
	}

	// Assert.
	got := mustCurrent(t, m, "ws1")
	if got.GetState() != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = %v under a live turn, want RENDER_STATE_THINKING", got.GetState())
	}
	if got.GetMergeStatus() != nil {
		t.Fatalf("merge_status = %v on a THINKING frame, want none", got.GetMergeStatus())
	}
}

func TestWithholdingAStoppedMergesStatusIsLoggedWithTheRun(t *testing.T) {
	// The withheld run must be recoverable from the log alone: a status that
	// silently stops reaching the wire is indistinguishable from a pipeline that
	// stopped publishing one.
	// Arrange.
	m, cl := openStoppedMerge(t, "run-logged")

	// Act.
	if err := applyTest(m, evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("Apply(turn started): %v", err)
	}

	// Assert.
	if !cl.contains("ssm: merge_status WITHHELD ws=ws1 run=run-logged axis=merge_failed state=RENDER_STATE_THINKING") {
		t.Fatalf("missing withholding log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

func TestAWithheldStatusReturnsWhenTheFrameResolvesToTheMergeAgain(t *testing.T) {
	// WITHHELD, NOT DROPPED. The run is still the account of that merge, so the
	// frame that resolves to the failure again carries it — the retained status
	// is not spent by the turn that outranked it.
	// Arrange.
	m, _ := openStoppedMerge(t, "run-returning")
	if err := applyTest(m, evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("Apply(turn started): %v", err)
	}

	// Act.
	if err := applyTest(m, evTurnEnded("s1", 2, false)); err != nil {
		t.Fatalf("Apply(turn ended): %v", err)
	}

	// Assert.
	got := mustCurrent(t, m, "ws1")
	if got.GetState() != frontendv1.RenderState_RENDER_STATE_MERGE_FAILED {
		t.Fatalf("state = %v once the turn ended, want RENDER_STATE_MERGE_FAILED", got.GetState())
	}
	if got.GetMergeStatus().GetRunId() != "run-returning" {
		t.Fatalf("run_id = %q once the frame resolves to the merge again, want run-returning", got.GetMergeStatus().GetRunId())
	}
}

func TestARunningMergesStatusSurvivesItsOwnTurn(t *testing.T) {
	// A merge's own before/after-action IS a turn in this session, and the merge
	// render states outrank it, so the run that is driving that turn keeps
	// reporting itself.
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	connectOperational(t, m, "ws1", "s1", "g1")
	if err := m.ApplyMergeStatus("ws1", string(merge.PhaseMergeBeforeAction), "before-action",
		pipelineStatus("run-before-action", 2, 0, "abc123def456", "the first commit")); err != nil {
		t.Fatalf("ApplyMergeStatus(merge_before_action): %v", err)
	}

	// Act.
	if err := applyTest(m, evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("Apply(turn started): %v", err)
	}

	// Assert.
	got := mustCurrent(t, m, "ws1")
	if got.GetState() != frontendv1.RenderState_RENDER_STATE_MERGING {
		t.Fatalf("state = %v during a merge's own turn, want RENDER_STATE_MERGING", got.GetState())
	}
	if got.GetMergeStatus().GetRunId() != "run-before-action" {
		t.Fatalf("run_id = %q during the merge's own turn, want run-before-action", got.GetMergeStatus().GetRunId())
	}
}

func TestARunningMergeKeepsItsStatusOnAHibernatedFrame(t *testing.T) {
	// THE OTHER BAND. A merge does not need a live shim to cherry-pick, so a
	// merging workspace whose session is asleep resolves to a CONNECTIVITY
	// verdict — which says nothing about the merge. Withholding the run there
	// would blank the status of exactly the merges that run unattended.
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	connectOperational(t, m, "ws1", "s1", "g1")
	if err := m.ApplySessionConnectivity("ws1", "s1", "g1", SessionConnectivityHibernated, "idle_sweep"); err != nil {
		t.Fatalf("ApplySessionConnectivity(hibernated): %v", err)
	}

	// Act.
	if err := m.ApplyMergeStatus("ws1", string(merge.PhaseMerging), "cherry-picking 1/2",
		pipelineStatus("run-asleep", 2, 1, "abc123def456", "the first commit")); err != nil {
		t.Fatalf("ApplyMergeStatus(merging): %v", err)
	}

	// Assert.
	got := mustCurrent(t, m, "ws1")
	if isSessionStatusRenderState(got.GetState()) {
		t.Fatalf("state = %v, want a non-session-status band for a workspace with no live session", got.GetState())
	}
	if got.GetMergeStatus().GetRunId() != "run-asleep" {
		t.Fatalf("run_id = %q on a frame outside the session-status band, want run-asleep", got.GetMergeStatus().GetRunId())
	}
}

// --- retirement of a retained run --------------------------------------------
//
// THE MERGED-FOREVER LOG FLOOD. A retained status is retained for as long as the
// merge AXIS behind it stands, and it must go the moment the axis is cleared.
// Two writers clear the axis: the pipeline's own `merge_none` transition, and
// the bring-up that retires a sticky `merged` when a merged workspace is
// reopened. Only the first swept the retention, so every workspace that ever
// merged went on holding its landed run in memory — and stampMergeStatusLocked's
// invariant guard fired on every resolve of it from then on, several times a
// second, indefinitely. Observed live on three workspaces after they merged.

// mergedPipelineStatus is a landed run's own terminal account of itself.
func mergedPipelineStatus(runID string, total int32) *frontendv1.MergeStatus {
	return &frontendv1.MergeStatus{
		RunId:            runID,
		PhaseStartedAtMs: 3000,
		UpdatedAtMs:      3001,
		Phase: &frontendv1.MergeStatus_Merged{Merged: &frontendv1.MergeStatusMerged{
			CommitsTotal: total,
		}},
	}
}

// openMergedRun arranges a settled workspace resting on a landed `merged` run.
func openMergedRun(t *testing.T, runID string) (*Manager, *capLog) {
	t.Helper()
	m, cl, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	settledSession(t, m, "ws1", "s1")
	if err := m.ApplyMergeStatus("ws1", string(merge.PhaseMerging), "cherry-picking 1/1",
		pipelineStatus(runID, 1, 0, "deadbeef1234", "the only commit")); err != nil {
		t.Fatalf("ApplyMergeStatus(merging): %v", err)
	}
	if err := m.ApplyMergeStatus("ws1", string(merge.PhaseMerged), "cherry-pick landed on target",
		mergedPipelineStatus(runID, 1)); err != nil {
		t.Fatalf("ApplyMergeStatus(merged): %v", err)
	}
	return m, cl
}

func TestALandedRunKeepsItsStatusWhileTheMergedAxisStands(t *testing.T) {
	// RETIREMENT IS THE AXIS'S EDGE, NOT THE RUN'S. `merged` is a merge-axis row
	// like any other, and the frame it resolves is exactly the frame that must
	// report how the merge ended. Retiring the status at MERGED itself would blank
	// the landing from the wire.
	// Arrange.
	m, _ := openMergedRun(t, "run-landed")

	// Act.
	got := mustCurrent(t, m, "ws1")

	// Assert.
	if got.GetMergeStatus().GetRunId() != "run-landed" {
		t.Fatalf("run_id = %q on the merged frame, want run-landed", got.GetMergeStatus().GetRunId())
	}
}

func TestReopeningAMergedWorkspaceRetiresItsRetainedRun(t *testing.T) {
	// Arrange.
	m, _ := openMergedRun(t, "run-reopened")

	// Act — the workspace is brought up again, which retires the `merged` axis.
	connectOperational(t, m, "ws1", "s1", "gen-1")

	// Assert.
	if got := mustCurrent(t, m, "ws1").GetMergeStatus(); got != nil {
		t.Fatalf("merge_status = %v after the merge axis was retired on reopen, want none", got)
	}
}

func TestReopeningAMergedWorkspaceTripsNoInvariantGuard(t *testing.T) {
	// THE FLOOD ITSELF. The retained run outliving its axis is what the guard
	// reports, so a reopen that leaves it behind fires on this resolve and every
	// resolve after it.
	// Arrange.
	m, cl := openMergedRun(t, "run-quiet")

	// Act.
	connectOperational(t, m, "ws1", "s1", "gen-1")
	mustCurrent(t, m, "ws1")

	// Assert.
	if cl.contains("has a retained pipeline merge_status") {
		t.Fatalf("the invariant guard fired after a reopen retired the merge axis:\n%s", strings.Join(cl.lines, "\n"))
	}
}

func TestRetiringARetainedRunOnReopenNamesTheRun(t *testing.T) {
	// A status that silently stops reaching the wire is indistinguishable from a
	// pipeline that stopped publishing one, so the drop names the run it ended.
	// Arrange.
	m, cl := openMergedRun(t, "run-logged-retire")

	// Act.
	connectOperational(t, m, "ws1", "s1", "gen-1")

	// Assert.
	if !cl.contains("ssm: pipeline merge_status RETIRED ws=ws1 run=run-logged-retire cause=" + causeMergeReopened) {
		t.Fatalf("missing retirement log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

func TestClearingTheMergeAxisRetiresTheRetainedRun(t *testing.T) {
	// The OTHER writer of `merge_none`, through the same funnel: the pipeline's
	// own axis clear.
	// Arrange.
	m, cl, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	if err := m.ApplyMergeStatus("ws1", string(merge.PhaseMerging), "cherry-picking 1/1",
		pipelineStatus("run-cleared", 1, 0, "deadbeef1234", "the only commit")); err != nil {
		t.Fatalf("ApplyMergeStatus(merging): %v", err)
	}

	// Act.
	if err := m.ApplyMergeTransition("ws1", sigMergeNone, "test arrangement"); err != nil {
		t.Fatalf("ApplyMergeTransition(merge_none): %v", err)
	}

	// Assert.
	if !cl.contains("ssm: pipeline merge_status RETIRED ws=ws1 run=run-cleared") {
		t.Fatalf("missing retirement log for a pipeline axis clear:\n%s", strings.Join(cl.lines, "\n"))
	}
}

func TestAReopenOverAStoppedMergeKeepsItsRetainedRun(t *testing.T) {
	// THE TERMINAL-BUT-NOT-MERGED PATH. A failed run never leaves the axis on its
	// own and a bring-up does not retire it (a stopped merge is exactly what the
	// user was brought back to act on), so its retained account stands with it.
	// Arrange.
	m, _ := openStoppedMerge(t, "run-stopped-reopen")

	// Act.
	if err := m.ApplySessionConnectivity("ws1", "s1", "g2", SessionConnectivityConnecting, "bring_up"); err != nil {
		t.Fatalf("ApplySessionConnectivity(connecting): %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").GetMergeStatus().GetRunId(); got != "run-stopped-reopen" {
		t.Fatalf("run_id = %q after a bring-up over a stopped merge, want the retained run-stopped-reopen", got)
	}
}

func TestTheInvariantGuardStillFiresForAnOrphanedRetainedRun(t *testing.T) {
	// THE DEFECT CLASS THE GUARD EXISTS FOR. Retirement removes the ONE producer
	// of orphans we know of; the guard is what reports the next one, so it must
	// still fire for a retained run with no merge axis behind it at all.
	// Arrange — a retained run on a workspace whose axis never spoke.
	m, cl, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	settledSession(t, m, "ws1", "s1")
	m.mu.Lock()
	m.recordPipelineStatusLocked("ws1", pipelineStatus("run-orphan", 1, 0, "deadbeef1234", "the only commit"))
	m.mu.Unlock()

	// Act.
	got := mustCurrent(t, m, "ws1")

	// Assert.
	if got.GetMergeStatus() != nil {
		t.Fatalf("merge_status = %v for an orphaned retained run, want none", got.GetMergeStatus())
	}
	if !cl.contains("ssm: INVARIANT VIOLATION ws=ws1 has a retained pipeline merge_status (run=run-orphan)") {
		t.Fatalf("the invariant guard did not fire for an orphaned retained run:\n%s", strings.Join(cl.lines, "\n"))
	}
}
