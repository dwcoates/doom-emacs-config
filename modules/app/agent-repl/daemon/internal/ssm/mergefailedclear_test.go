package ssm

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
	"claude-repld/internal/workspace/merge"
)

// failedMerge arranges a workspace resting on the terminal `merge_failed`, with
// a settled session behind it so there is something for the cleared axis to
// stop masking.
func failedMerge(t *testing.T, m *Manager, workspace, sessionID string) {
	t.Helper()
	settledSession(t, m, workspace, sessionID)
	applyPhases(t, m, workspace, merge.PhaseMerging, merge.PhaseMergeFailed)
	if got := mustCurrent(t, m, workspace).GetState(); got != frontendv1.RenderState_RENDER_STATE_MERGE_FAILED {
		t.Fatalf("arrangement: state = %v, want RENDER_STATE_MERGE_FAILED", got)
	}
}

// THE GUARANTEE: an explicit hard restart takes a workspace OFF a terminal
// `merge_failed`. Without it the verdict outranks the whole session-status
// ladder forever, and the restart brings a healthy session up underneath a
// failure nothing can clear.
func TestRestartClearsATerminalMergeFailedAxis(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	failedMerge(t, m, "ws1", "s1")

	// Act.
	cleared, err := m.ClearFailedMergeAxis("ws1", "session_restart:r1")
	if err != nil {
		t.Fatalf("ClearFailedMergeAxis: %v", err)
	}

	// Assert.
	if !cleared {
		t.Fatal("ClearFailedMergeAxis reported nothing cleared over a merge_failed workspace")
	}
	if got := mustCurrent(t, m, "ws1").GetState(); got == frontendv1.RenderState_RENDER_STATE_MERGE_FAILED {
		t.Fatal("the workspace still resolves RENDER_STATE_MERGE_FAILED after a restart cleared the axis")
	}
}

// THE POINT OF THE CLEAR: once the axis is cleared the resolver prefers the
// LIVE session again, rather than falling to some other unrelated state.
func TestClearedMergeFailedResolvesTheLiveSession(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	failedMerge(t, m, "ws1", "s1")
	connectOperational(t, m, "ws1", "s1", "gen-1")

	// Act.
	if _, err := m.ClearFailedMergeAxis("ws1", "session_restart:r1"); err != nil {
		t.Fatalf("ClearFailedMergeAxis: %v", err)
	}

	// Assert — the session's own settled turn is what the frame reports.
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("state = %v after the axis was cleared, want the live session's own RENDER_STATE_DONE", got)
	}
}

// THE FAILURE RECORD IS SUPERSEDED, NOT DELETED: the `merge_failed` row and its
// cause text stay in the append-only log, so the failure remains readable.
func TestClearingKeepsTheFailedRowInTheLog(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	failedMerge(t, m, "ws1", "s1")

	// Act.
	if _, err := m.ClearFailedMergeAxis("ws1", "session_restart:r1"); err != nil {
		t.Fatalf("ClearFailedMergeAxis: %v", err)
	}

	// Assert.
	var n int
	if err := m.db.QueryRow(
		`SELECT COUNT(*) FROM workspace_state WHERE workspace = ? AND state = ?`,
		"ws1", sigMergeFailed,
	).Scan(&n); err != nil {
		t.Fatalf("count merge_failed rows: %v", err)
	}
	if n != 1 {
		t.Fatalf("merge_failed rows = %d after a clear, want the original row left standing", n)
	}
}

// THE VIOLATION EDGE: a merge that is genuinely in flight is NOT a verdict the
// user has acted on, so a restart must leave it alone.
func TestRestartDoesNotClearAnInFlightMerge(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	settledSession(t, m, "ws1", "s1")
	applyPhases(t, m, "ws1", merge.PhaseMerging, merge.PhaseMergeConflict)

	// Act.
	cleared, err := m.ClearFailedMergeAxis("ws1", "session_restart:r1")
	if err != nil {
		t.Fatalf("ClearFailedMergeAxis: %v", err)
	}

	// Assert.
	if cleared {
		t.Fatal("a restart cleared a conflicted merge that is still in flight")
	}
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_MERGE_CONFLICT {
		t.Fatalf("state = %v after a restart over a conflicted merge, want RENDER_STATE_MERGE_CONFLICT retained", got)
	}
}

// A `merged` workspace is not a failure the user acted on either: its own
// reopen edge owns that retirement, and clearing it here would be a second
// authority on the same question.
func TestRestartDoesNotClearAMergedAxis(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	settledSession(t, m, "ws1", "s1")
	applyPhases(t, m, "ws1", merge.PhaseMerging, merge.PhaseMerged)

	// Act.
	cleared, err := m.ClearFailedMergeAxis("ws1", "session_restart:r1")
	if err != nil {
		t.Fatalf("ClearFailedMergeAxis: %v", err)
	}

	// Assert.
	if cleared {
		t.Fatal("a restart cleared the axis of a MERGED workspace; only a terminal merge_failed is cleared here")
	}
}

// A workspace with no merge history at all passes through untouched: inventing
// a merge_none row would put a merge fact on a workspace that never merged.
func TestRestartOnAWorkspaceThatNeverMergedWritesNoMergeRow(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	settledSession(t, m, "ws1", "s1")

	// Act.
	if _, err := m.ClearFailedMergeAxis("ws1", "session_restart:r1"); err != nil {
		t.Fatalf("ClearFailedMergeAxis: %v", err)
	}

	// Assert.
	r, err := resolve(m.db, "ws1", m.logf)
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if r.mergePhase != "" {
		t.Fatalf("merge axis = %q after a restart on a never-merged workspace, want no merge row at all", r.mergePhase)
	}
}

// An empty workspace is a caller error, refused loudly rather than answered
// with a clean "nothing to clear".
func TestClearFailedMergeAxisRefusesAnEmptyWorkspace(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{})

	// Act.
	_, err := m.ClearFailedMergeAxis("", "session_restart:r1")

	// Assert.
	if err == nil {
		t.Fatal("ClearFailedMergeAxis accepted an empty workspace")
	}
}

// THE OTHER CLEAR EDGE: a NEW merge supersedes the old verdict, because its
// own `merge_enqueuing` row is the newest merge row from then on.
func TestEnqueuingANewMergeClearsAPriorMergeFailed(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	failedMerge(t, m, "ws1", "s1")

	// Act.
	applyPhases(t, m, "ws1", merge.PhaseMergeEnqueuing)

	// Assert.
	if got := mustCurrent(t, m, "ws1").GetState(); got == frontendv1.RenderState_RENDER_STATE_MERGE_FAILED {
		t.Fatal("a workspace with a NEW merge enqueued still resolves RENDER_STATE_MERGE_FAILED")
	}
}

// The stale run's ACCOUNT goes with the verdict a new attempt supersedes:
// leaving it retained put the old failure's cause on every frame of the retry.
func TestEnqueuingANewMergeRetiresTheFailedRunStatus(t *testing.T) {
	// Arrange — a failed run that published its own terminal status.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	settledSession(t, m, "ws1", "s1")
	if err := m.ApplyMergeStatus("ws1", string(merge.PhaseMergeFailed), "test arrangement",
		&frontendv1.MergeStatus{RunId: "run-1"}); err != nil {
		t.Fatalf("ApplyMergeStatus: %v", err)
	}
	if got := mustCurrent(t, m, "ws1").GetMergeStatus().GetRunId(); got != "run-1" {
		t.Fatalf("arrangement: merge_status run = %q, want run-1", got)
	}

	// Act.
	applyPhases(t, m, "ws1", merge.PhaseMergeEnqueuing)

	// Assert.
	if got := mustCurrent(t, m, "ws1").GetMergeStatus().GetRunId(); got != "" {
		t.Fatalf("merge_status run = %q after a NEW merge was enqueued, want the failed run retired", got)
	}
}

// THE CLEAR IS DURABLE. The verdict replays out of the append-only log on every
// reload, so a clear that lived only in memory would come undone at the next
// daemon start — which is exactly how a restart appeared not to work.
func TestTheClearedAxisSurvivesAReload(t *testing.T) {
	// Arrange.
	m, cl, path := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	failedMerge(t, m, "ws1", "s1")
	if _, err := m.ClearFailedMergeAxis("ws1", "session_restart:r1"); err != nil {
		t.Fatalf("ClearFailedMergeAxis: %v", err)
	}
	if err := m.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Act — a fresh Manager over the same durable state log.
	reloaded, err := Open(Options{DBPath: path, Logf: cl.logf, Resolver: fakeResolver{"s1": "ws1"}})
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	t.Cleanup(func() { reloaded.Close() })

	// Assert.
	if got := mustCurrent(t, reloaded, "ws1").GetState(); got == frontendv1.RenderState_RENDER_STATE_MERGE_FAILED {
		t.Fatal("RENDER_STATE_MERGE_FAILED came back after a reload; the clear must be durable")
	}
}
