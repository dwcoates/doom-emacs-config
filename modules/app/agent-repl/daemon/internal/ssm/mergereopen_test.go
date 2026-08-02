package ssm

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
	"claude-repld/internal/workspace/merge"
)

// settledSession gives a workspace a session-status row, which an operational
// controller generation requires: the composite refuses to project an
// operational controller with no status at all.
func settledSession(t *testing.T, m *Manager, workspace, sessionID string) {
	t.Helper()
	if err := m.Apply(evTurnStarted(sessionID, 1)); err != nil {
		t.Fatalf("Apply(turn started) for %s: %v", workspace, err)
	}
	if err := m.Apply(evTurnEnded(sessionID, 2, false)); err != nil {
		t.Fatalf("Apply(turn ended) for %s: %v", workspace, err)
	}
}

// THE GUARANTEE: a merged workspace that is REOPENED stops resolving `merged`.
// `merged` ranks above the whole color ladder and no session-status row can
// supersede a merge row, so without the retirement a reopened workspace pushed
// `merged` for the rest of its life while its live session worked behind it.
func TestReopeningAMergedWorkspaceRetiresTheMergeAxis(t *testing.T) {
	// Arrange — a workspace that merged.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	settledSession(t, m, "ws1", "s1")
	applyPhases(t, m, "ws1", merge.PhaseMerging, merge.PhaseMerged)
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_MERGED {
		t.Fatalf("arrangement: state = %v, want RENDER_STATE_MERGED", got)
	}

	// Act — it is reopened: a new controller generation enters connecting.
	connectOperational(t, m, "ws1", "s1", "gen-1")

	// Assert.
	if got := mustCurrent(t, m, "ws1").GetState(); got == frontendv1.RenderState_RENDER_STATE_MERGED {
		t.Fatal("a reopened workspace still resolves RENDER_STATE_MERGED; the merge axis must be retired on bring-up")
	}
}

// The durable merged-at fact is NOT what the retirement clears. It is set-once
// and orders the frontends' recently-merged section, so a reopen that moved it
// would reorder history.
func TestReopeningAMergedWorkspaceKeepsTheMergedAtFact(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	settledSession(t, m, "ws1", "s1")
	applyPhases(t, m, "ws1", merge.PhaseMerging, merge.PhaseMerged)
	mergedAt := mustCurrent(t, m, "ws1").GetMergedAtMs()
	if mergedAt == 0 {
		t.Fatal("arrangement: merged_at_ms = 0, want the instant the merge landed")
	}

	// Act.
	connectOperational(t, m, "ws1", "s1", "gen-1")

	// Assert.
	if got := mustCurrent(t, m, "ws1").GetMergedAtMs(); got != mergedAt {
		t.Fatalf("merged_at_ms = %d after a reopen, want the unchanged %d", got, mergedAt)
	}
}

// THE VIOLATION EDGE: a merge that is genuinely still in flight must NOT be
// retired by a bring-up. A conflicted merge often causes the very bring-up that
// would clear it, and clearing it would lose the state the user has to act on.
func TestReopeningDoesNotRetireAnInFlightMerge(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	settledSession(t, m, "ws1", "s1")
	applyPhases(t, m, "ws1", merge.PhaseMerging, merge.PhaseMergeConflict)

	// Act.
	connectOperational(t, m, "ws1", "s1", "gen-1")

	// Assert.
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_MERGE_CONFLICT {
		t.Fatalf("state = %v after a bring-up over a conflicted merge, want RENDER_STATE_MERGE_CONFLICT retained", got)
	}
}

// A workspace with no merge history at all must pass through the bring-up
// untouched: there is no axis to retire, and inventing a merge_none row would
// put a merge fact on a workspace that never merged.
func TestReopeningAWorkspaceThatNeverMergedWritesNoMergeRow(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	settledSession(t, m, "ws1", "s1")

	// Act.
	connectOperational(t, m, "ws1", "s1", "gen-1")

	// Assert. REWRITTEN off the retired flat merge_phase wire field onto the
	// merge AXIS it was projected from — the axis is what "no merge row" is a
	// statement about, and it is untouched by the field's retirement.
	r, err := resolve(m.db, "ws1", m.logf)
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if r.mergePhase != "" {
		t.Fatalf("merge axis = %q after a bring-up on a never-merged workspace, want no merge row at all", r.mergePhase)
	}
}
