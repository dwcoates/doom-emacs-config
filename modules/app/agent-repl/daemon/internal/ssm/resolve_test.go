package ssm

import (
	"database/sql"
	"fmt"
	"path/filepath"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// newTestDB opens a fresh SSM database in a temp dir (file-backed, so WAL
// and reopen behavior are exercised).
func newTestDB(t *testing.T) *sql.DB {
	t.Helper()
	db, err := openDB(filepath.Join(t.TempDir(), "state.db"), t.Logf)
	if err != nil {
		t.Fatalf("openDB: %v", err)
	}
	t.Cleanup(func() { db.Close() })
	return db
}

// newWiredTestDB is newTestDB with WORKSPACE already on the legacy connectivity projection, at an
// `at` below every seed a test appends.
//
// Almost every resolution test is about an axis ABOVE the wiring — which agent
// row wins, whether purple outranks red, how the merge states sit. None of them
// can be asked at all of an unwired workspace, because `severed` (rank 12)
// outranks every one of those candidates by construction: that is the whole
// point of the connection-truth law. So the wiring is arranged, once, here, and
// the tests that are genuinely ABOUT the legacy connectivity projection use newTestDB directly.
func newWiredTestDB(t *testing.T, workspace string) *sql.DB {
	t.Helper()
	db := newTestDB(t)
	seedSignal(t, db, workspace, "", sigWired, causeWired, -1, 0)
	return db
}

// seedSignal appends one raw signal row at a caller-chosen `at`, so tests
// can control latest-per-axis ordering deterministically. The row carries no
// task id; use seedTaskSignal for the live-task counter.
func seedSignal(t *testing.T, db *sql.DB, ws, sid, state, cause string, seq int64, at int64) {
	t.Helper()
	seedTaskSignal(t, db, ws, sid, state, cause, seq, at, "")
}

// seedTaskSignal is seedSignal with an explicit task id, for the live-task
// counter's per-task dedup.
func seedTaskSignal(t *testing.T, db *sql.DB, ws, sid, state, cause string, seq int64, at int64, taskID string) {
	t.Helper()
	cs := sql.NullInt64{}
	if seq >= 0 {
		cs = sql.NullInt64{Int64: seq, Valid: true}
	}
	if err := appendRow(db, ws, sid, state, cause, cs, at, taskID); err != nil {
		t.Fatalf("appendRow(%s): %v", state, err)
	}
}

// TestResolvePrecedence drives the precedence resolution over combinations
// of the latest per-axis signals, one case per edge. `at` values encode
// which signal is latest within an axis.
func TestResolvePrecedence(t *testing.T) {
	type sig struct {
		state string
		cause string
		at    int64
	}
	tests := []struct {
		name string
		sigs []sig
		want frontendv1.RenderState
	}{
		{
			name: "merge_conflict dominates everything",
			sigs: []sig{{sigThinking, causeTurnStarted, 1}, {sigMerging, causeMergeTransition, 2}, {sigMergeConflict, causeMergeTransition, 3}},
			want: frontendv1.RenderState_RENDER_STATE_MERGE_CONFLICT,
		},
		{
			name: "merge_failed dominates merging and agent",
			sigs: []sig{{sigThinking, causeTurnStarted, 1}, {sigMergeFailed, causeMergeTransition, 2}},
			want: frontendv1.RenderState_RENDER_STATE_MERGE_FAILED,
		},
		{
			name: "merged shows when no session-status lifecycle present",
			sigs: []sig{{sigMerged, causeMergeTransition, 1}},
			want: frontendv1.RenderState_RENDER_STATE_MERGED,
		},
		{
			// The agent-state guard is GONE. A merged workspace leaves the
			// tab-bar the moment the merge lands, so there is no tab for a
			// live agent state to compete for — the guard existed only to
			// keep that tab from flashing during the async teardown, and
			// removing the tab makes the flash impossible rather than
			// merely covered up.
			name: "merged is not suppressed by a later agent state",
			sigs: []sig{{sigMerged, causeMergeTransition, 1}, {sigIdle, causeSessionStarted, 2}},
			want: frontendv1.RenderState_RENDER_STATE_MERGED,
		},
		{
			name: "merged survives a teardown that leaves the session dead",
			sigs: []sig{{sigMerged, causeMergeTransition, 1}, {sigDead, causeSessionEnded, 2}},
			want: frontendv1.RenderState_RENDER_STATE_MERGED,
		},
		{
			name: "merging beats dead",
			sigs: []sig{{sigDead, causeSessionEnded, 1}, {sigMerging, causeMergeTransition, 2}},
			want: frontendv1.RenderState_RENDER_STATE_MERGING,
		},
		{
			// The mark's own rank: it is a merge state, so it still outranks
			// the whole color ladder — a merge attempt the user just made is
			// what the workspace is doing.
			name: "merge_enqueuing beats dead",
			sigs: []sig{{sigDead, causeSessionEnded, 1}, {sigMergeEnqueuing, causeMergeTransition, 2}},
			want: frontendv1.RenderState_RENDER_STATE_MERGE_ENQUEUING,
		},
		{
			name: "merge_queued beats dead",
			sigs: []sig{{sigDead, causeSessionEnded, 1}, {sigMergeQueued, causeMergeTransition, 2}},
			want: frontendv1.RenderState_RENDER_STATE_MERGE_QUEUED,
		},
		{
			name: "dead beats thinking",
			sigs: []sig{{sigThinking, causeTurnStarted, 1}, {sigDead, causeSessionEnded, 2}},
			want: frontendv1.RenderState_RENDER_STATE_DEAD,
		},
		{
			name: "thinking beats permission (latest agent wins within axis)",
			sigs: []sig{{sigPermission, "permission", 1}, {sigThinking, causeTurnStarted, 2}},
			want: frontendv1.RenderState_RENDER_STATE_THINKING,
		},
		{
			name: "done is the clean post-turn state",
			sigs: []sig{{sigThinking, causeTurnStarted, 1}, {sigDone, causeTurnEnded, 2}},
			want: frontendv1.RenderState_RENDER_STATE_DONE,
		},
		{
			// An abnormal CONCLUSION is a vendor/account block, not a state
			// of its own: the turn is over and only a human or the vendor
			// can release whatever ended it.
			name: "vendor_blocked on an errored turn end",
			sigs: []sig{{sigThinking, causeTurnStarted, 1}, {sigVendorBlocked, causeVendorBlocked, 2}},
			want: frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED,
		},
		{
			// The legacy connectivity projection's own two blue tokens. `starting` says a bring-up
			// is in flight; `severed` says nothing is, nothing is wired, and
			// something broke.
			name: "starting is bring-up",
			sigs: []sig{{sigStarting, causeWired, 5}},
			want: frontendv1.RenderState_RENDER_STATE_INIT,
		},
		{
			name: "severed when the wiring broke",
			sigs: []sig{{sigSevered, causeWired, 5}},
			want: frontendv1.RenderState_RENDER_STATE_SEVERED,
		},
		{
			name: "idle when available and no other signal",
			sigs: []sig{{sigIdle, causeSessionStarted, 1}},
			want: frontendv1.RenderState_RENDER_STATE_IDLE,
		},
		{
			// Shim-asserted readiness: the route is proven usable without a
			// first message ever having been sent.
			name: "ready when the shim asserted readiness",
			sigs: []sig{{sigReady, causeSessionStarted, 1}},
			want: frontendv1.RenderState_RENDER_STATE_READY,
		},
		{
			name: "degraded ranks under dead, over agent states",
			sigs: []sig{{sigThinking, causeTurnStarted, 1}, {sigDegraded, "degraded", 2}},
			want: frontendv1.RenderState_RENDER_STATE_DEGRADED,
		},
		{
			name: "cleared merge axis falls through to agent state",
			sigs: []sig{{sigMerging, causeMergeTransition, 1}, {sigMergeNone, causeMergeTransition, 2}, {sigThinking, causeTurnStarted, 3}},
			want: frontendv1.RenderState_RENDER_STATE_THINKING,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			db := newWiredTestDB(t, "ws")
			const ws = "ws"
			for i, s := range tt.sigs {
				seedSignal(t, db, ws, "sess", s.state, s.cause, int64(i), s.at)
			}
			// Act.
			got, err := resolve(db, ws, nil)
			if err != nil {
				t.Fatalf("resolve: %v", err)
			}
			// Assert.
			if !got.found {
				t.Fatalf("resolve found=false, want state %s", renderName(tt.want))
			}
			if got.state != tt.want {
				t.Fatalf("resolve state = %s, want %s", renderName(got.state), renderName(tt.want))
			}
		})
	}
}

// TestResolveIdleAsyncPromotion covers the derived idle_async: an idle
// winner with live background tasks promotes; without tasks it stays idle.
func TestResolveIdleAsyncPromotion(t *testing.T) {
	tests := []struct {
		name      string
		tasks     []string // sequence of task signals
		wantState frontendv1.RenderState
		wantCount int64
	}{
		{"idle with no tasks stays idle", nil, frontendv1.RenderState_RENDER_STATE_IDLE, 0},
		{"idle with one live task promotes", []string{sigTaskStarted}, frontendv1.RenderState_RENDER_STATE_IDLE_ASYNC, 1},
		{"idle with started then ended stays idle", []string{sigTaskStarted, sigTaskEnded}, frontendv1.RenderState_RENDER_STATE_IDLE, 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			db := newWiredTestDB(t, "ws")
			const ws = "ws"
			seedSignal(t, db, ws, "sess", sigIdle, causeSessionStarted, 0, 1)
			for i, ts := range tt.tasks {
				seedSignal(t, db, ws, "sess", ts, ts, int64(i+1), int64(i+2))
			}
			// Act.
			got, err := resolve(db, ws, nil)
			if err != nil {
				t.Fatalf("resolve: %v", err)
			}
			// Assert.
			if got.state != tt.wantState {
				t.Fatalf("state = %s, want %s", renderName(got.state), renderName(tt.wantState))
			}
			if got.liveTaskCount != tt.wantCount {
				t.Fatalf("liveTaskCount = %d, want %d", got.liveTaskCount, tt.wantCount)
			}
		})
	}
}

// TestResolveTurnActive checks turn_active tracks the latest turn edge and
// that a non-turn agent signal (session start) reports turn_active=false.
func TestResolveTurnActive(t *testing.T) {
	tests := []struct {
		name  string
		state string
		cause string
		want  bool
	}{
		{"turn started is active", sigThinking, causeTurnStarted, true},
		{"turn ended is inactive", sigDone, causeTurnEnded, false},
		{"session started is inactive", sigIdle, causeSessionStarted, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			db := newWiredTestDB(t, "ws")
			seedSignal(t, db, "ws", "sess", tt.state, tt.cause, 0, 1)
			got, err := resolve(db, "ws", nil)
			if err != nil {
				t.Fatalf("resolve: %v", err)
			}
			if got.turnActive != tt.want {
				t.Fatalf("turnActive = %t, want %t", got.turnActive, tt.want)
			}
		})
	}
}

// TestResolveNoRenderBearingSignal: a workspace with only task counters (no
// agent/merge axis) has no resolved render state — matching the elisp nil.
func TestResolveNoRenderBearingSignal(t *testing.T) {
	// Arrange.
	db := newWiredTestDB(t, "ws")
	seedSignal(t, db, "ws", "sess", sigTaskStarted, causeTaskStarted, 0, 1)
	// Act.
	got, err := resolve(db, "ws", nil)
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	// Assert.
	if got.found {
		t.Fatalf("found=true, want false for a task-only workspace (state=%s)", renderName(got.state))
	}
}

// TestResolveUnknownWorkspace: an unknown workspace resolves found=false
// (explicit miss, no silent default).
func TestResolveUnknownWorkspace(t *testing.T) {
	db := newWiredTestDB(t, "ws")
	got, err := resolve(db, "never-seen", nil)
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if got.found {
		t.Fatalf("found=true for unknown workspace, want false")
	}
}

// TestResolveMergePhaseSurfaced: merge_phase reflects the live merge token
// even when an agent state wins the render precedence.
func TestResolveMergePhaseSurfaced(t *testing.T) {
	// Arrange: an in-flight merge_queued plus a live thinking agent.
	db := newWiredTestDB(t, "ws")
	seedSignal(t, db, "ws", "sess", sigThinking, causeTurnStarted, 0, 2)
	seedSignal(t, db, "ws", "", sigMergeQueued, causeMergeTransition, -1, 1)
	// Act.
	got, err := resolve(db, "ws", nil)
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	// Assert: merge_queued outranks thinking, so it wins AND merge_phase set.
	if got.state != frontendv1.RenderState_RENDER_STATE_MERGE_QUEUED {
		t.Fatalf("state = %s, want MERGE_QUEUED", renderName(got.state))
	}
	if got.mergePhase != sigMergeQueued {
		t.Fatalf("mergePhase = %q, want %q", got.mergePhase, sigMergeQueued)
	}
}

// TestResolveCountsTheLiveTaskSet covers orphan ends and mixed lifecycle sets.
// Live means Starts EXCEPT Ends: an end with no observed start is an anomaly to
// repair at ingestion/open, never a negative contribution to this projection.
func TestResolveCountsTheLiveTaskSet(t *testing.T) {
	tests := []struct {
		name      string
		starts    []string
		ends      []string
		wantCount int64
	}{
		{
			name:      "one unmatched end contributes nothing",
			ends:      []string{"ghost-1"},
			wantCount: 0,
		},
		{
			name:      "two unmatched ends contribute nothing",
			ends:      []string{"ghost-1", "ghost-2"},
			wantCount: 0,
		},
		{
			name:      "mixed set counts only starts without ends",
			starts:    []string{"live-1", "done", "live-2"},
			ends:      []string{"done", "orphan"},
			wantCount: 2,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			db := newWiredTestDB(t, "ws")
			const ws = "ws"
			seedSignal(t, db, ws, "sess", sigIdle, causeSessionStarted, 0, 1)
			at := int64(2)
			for _, id := range tt.starts {
				seedTaskSignal(t, db, ws, "sess", sigTaskStarted, sigTaskStarted, at, at, id)
				at++
			}
			for _, id := range tt.ends {
				seedTaskSignal(t, db, ws, "sess", sigTaskEnded, sigTaskEnded, at, at, id)
				at++
			}
			var logged []string
			logf := func(format string, args ...any) {
				logged = append(logged, fmt.Sprintf(format, args...))
			}
			got, err := resolve(db, ws, logf)
			if err != nil {
				t.Fatalf("resolve: %v", err)
			}
			if got.liveTaskCount != tt.wantCount {
				t.Fatalf("liveTaskCount = %d, want %d", got.liveTaskCount, tt.wantCount)
			}
			if len(logged) != 0 {
				t.Fatalf("resolve must be a pure projection, got logs: %v", logged)
			}
		})
	}
}

// TestResolveDoesNotLogWhenTheTaskCountIsSane pins resolution as a pure
// projection. Anomaly reporting belongs to the ingestion/open repair edges.
func TestResolveDoesNotLogWhenTheTaskCountIsSane(t *testing.T) {
	// Arrange
	db := newWiredTestDB(t, "ws")
	const ws = "ws"
	seedSignal(t, db, ws, "sess", sigIdle, causeSessionStarted, 0, 1)
	seedTaskSignal(t, db, ws, "sess", sigTaskStarted, sigTaskStarted, 1, 2, "t1")
	seedTaskSignal(t, db, ws, "sess", sigTaskEnded, sigTaskEnded, 2, 3, "t1")
	var logged []string
	logf := func(format string, args ...any) {
		logged = append(logged, fmt.Sprintf(format, args...))
	}
	// Act
	got, err := resolve(db, ws, logf)
	// Assert
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if got.liveTaskCount != 0 {
		t.Fatalf("liveTaskCount = %d, want 0", got.liveTaskCount)
	}
	if len(logged) != 0 {
		t.Fatalf("a matched start/end must not log an impossible state, got: %v", logged)
	}
}
