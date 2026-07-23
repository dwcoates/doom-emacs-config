package ssm

import (
	"fmt"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// fakeResolver binds session ids to workspaces from a static map.
type fakeResolver map[string]string

func (f fakeResolver) Workspace(sessionID string) (string, bool) {
	ws, ok := f[sessionID]
	return ws, ok
}

// capLog captures log lines for assertions, safe for concurrent use.
type capLog struct {
	mu    sync.Mutex
	lines []string
}

func (c *capLog) logf(format string, args ...any) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.lines = append(c.lines, fmt.Sprintf(format, args...))
}

func (c *capLog) contains(substr string) bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	for _, l := range c.lines {
		if strings.Contains(l, substr) {
			return true
		}
	}
	return false
}

func (c *capLog) count(substr string) int {
	c.mu.Lock()
	defer c.mu.Unlock()
	n := 0
	for _, l := range c.lines {
		if strings.Contains(l, substr) {
			n++
		}
	}
	return n
}

// event helpers ------------------------------------------------------------

func evSessionStarted(sid string, seq uint64) *corev1.Event {
	return &corev1.Event{SessionId: sid, Seq: seq, Payload: &corev1.Event_SessionStarted{SessionStarted: &corev1.SessionStarted{}}}
}
func evTurnStarted(sid string, seq uint64) *corev1.Event {
	return &corev1.Event{SessionId: sid, Seq: seq, Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{}}}
}
func evTurnEnded(sid string, seq uint64, isErr bool) *corev1.Event {
	return &corev1.Event{SessionId: sid, Seq: seq, Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{IsError: isErr}}}
}
func evSessionEnded(sid string, seq uint64) *corev1.Event {
	return &corev1.Event{SessionId: sid, Seq: seq, Payload: &corev1.Event_SessionEnded{SessionEnded: &corev1.SessionEnded{}}}
}
func evTaskStarted(sid string, seq uint64, taskID string) *corev1.Event {
	return &corev1.Event{SessionId: sid, Seq: seq, Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{TaskId: taskID}}}
}
func evTaskEnded(sid string, seq uint64, taskID string, status corev1.TerminalStatus) *corev1.Event {
	return &corev1.Event{SessionId: sid, Seq: seq, Payload: &corev1.Event_TaskEnded{TaskEnded: &corev1.TaskEnded{TaskId: taskID, Status: status}}}
}

// openTest opens a Manager on a temp DB with a capturing logger.
func openTest(t *testing.T, resolver Resolver) (*Manager, *capLog, string) {
	t.Helper()
	path := filepath.Join(t.TempDir(), "state.db")
	cl := &capLog{}
	m, err := Open(Options{DBPath: path, Logf: cl.logf, Resolver: resolver})
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { m.Close() })
	return m, cl, path
}

func mustCurrent(t *testing.T, m *Manager, ws string) *frontendv1.WorkspaceState {
	t.Helper()
	cur, found, err := m.Current(ws)
	if err != nil {
		t.Fatalf("Current(%s): %v", ws, err)
	}
	if !found {
		t.Fatalf("Current(%s): not found", ws)
	}
	return cur
}

// TestApplyLifecycleTransitions drives the full agent-axis lifecycle through
// Apply, one transition per case.
func TestApplyLifecycleTransitions(t *testing.T) {
	tests := []struct {
		name string
		ev   *corev1.Event
		want frontendv1.RenderState
	}{
		{"session started -> idle", evSessionStarted("s1", 1), frontendv1.RenderState_RENDER_STATE_IDLE},
		{"turn started -> thinking", evTurnStarted("s1", 1), frontendv1.RenderState_RENDER_STATE_THINKING},
		{"clean turn end -> done", evTurnEnded("s1", 1, false), frontendv1.RenderState_RENDER_STATE_DONE},
		{"errored turn end -> stop_failed", evTurnEnded("s1", 1, true), frontendv1.RenderState_RENDER_STATE_STOP_FAILED},
		{"session ended -> dead", evSessionEnded("s1", 1), frontendv1.RenderState_RENDER_STATE_DEAD},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
			// Act.
			if err := m.Apply(tt.ev); err != nil {
				t.Fatalf("Apply: %v", err)
			}
			// Assert.
			if got := mustCurrent(t, m, "ws1").State; got != tt.want {
				t.Fatalf("state = %s, want %s", renderName(got), renderName(tt.want))
			}
		})
	}
}

// TestApplyIdempotentReapply: re-applying the same event (same session+seq)
// is a no-op and logs the skip.
func TestApplyIdempotentReapply(t *testing.T) {
	// Arrange.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	ev := evTurnStarted("s1", 7)
	if err := m.Apply(ev); err != nil {
		t.Fatalf("Apply first: %v", err)
	}
	// Act: apply the identical event again.
	if err := m.Apply(evTurnStarted("s1", 7)); err != nil {
		t.Fatalf("Apply second: %v", err)
	}
	// Assert: only one thinking transition, and the duplicate was logged.
	if got := cl.count("→RENDER_STATE_THINKING"); got != 1 {
		t.Fatalf("thinking transitions logged = %d, want 1", got)
	}
	if !cl.contains("duplicate event skipped") {
		t.Fatalf("expected a duplicate-skip log line, got: %v", cl.lines)
	}
}

// TestLiveTaskCounting covers task started/ended/lost accounting and the
// idle_async promotion it drives.
func TestLiveTaskCounting(t *testing.T) {
	// Arrange: idle session.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("session start: %v", err)
	}
	// Act + Assert, step by step (each step is one counting edge).
	steps := []struct {
		ev        *corev1.Event
		wantCount int64
		wantState frontendv1.RenderState
	}{
		{evTaskStarted("s1", 2, "a1"), 1, frontendv1.RenderState_RENDER_STATE_IDLE_ASYNC},
		{evTaskStarted("s1", 3, "a2"), 2, frontendv1.RenderState_RENDER_STATE_IDLE_ASYNC},
		{evTaskEnded("s1", 4, "a1", corev1.TerminalStatus_TERMINAL_STATUS_DONE), 1, frontendv1.RenderState_RENDER_STATE_IDLE_ASYNC},
		{evTaskEnded("s1", 5, "a2", corev1.TerminalStatus_TERMINAL_STATUS_LOST), 0, frontendv1.RenderState_RENDER_STATE_IDLE},
	}
	for i, s := range steps {
		if err := m.Apply(s.ev); err != nil {
			t.Fatalf("step %d Apply: %v", i, err)
		}
		cur := mustCurrent(t, m, "ws1")
		if cur.LiveTaskCount != s.wantCount {
			t.Fatalf("step %d: count = %d, want %d", i, cur.LiveTaskCount, s.wantCount)
		}
		if cur.State != s.wantState {
			t.Fatalf("step %d: state = %s, want %s", i, renderName(cur.State), renderName(s.wantState))
		}
	}
}

// TestMergeTransitionInterleaving: a merge transition dominates a live agent
// state while in flight, then clearing the merge falls back to the agent
// state — interleaved with turn events.
func TestMergeTransitionInterleaving(t *testing.T) {
	// Arrange: a thinking session.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn start: %v", err)
	}
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("pre-merge state = %s, want THINKING", renderName(got))
	}

	// Act 1: a merge is queued while the turn is live.
	if err := m.ApplyMergeTransition("ws1", sigMergeQueued, "waiting-on-lock"); err != nil {
		t.Fatalf("merge queued: %v", err)
	}
	// Assert 1: merge_queued dominates thinking.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_MERGE_QUEUED {
		t.Fatalf("state = %s, want MERGE_QUEUED", renderName(got))
	}

	// Act 2: turn ends underneath the merge; then the merge clears.
	if err := m.Apply(evTurnEnded("s1", 2, false)); err != nil {
		t.Fatalf("turn end: %v", err)
	}
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_MERGE_QUEUED {
		t.Fatalf("state after turn end under merge = %s, want MERGE_QUEUED", renderName(got))
	}
	if err := m.ApplyMergeTransition("ws1", "", "released"); err != nil {
		t.Fatalf("merge clear: %v", err)
	}
	// Assert 2: with the merge cleared, the underlying done state surfaces.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("state after merge clear = %s, want DONE", renderName(got))
	}
}

// TestMergeTransitionUnknownPhaseErrors: an unknown merge phase errors
// loudly (no silent fallback).
func TestMergeTransitionUnknownPhaseErrors(t *testing.T) {
	m, _, _ := openTest(t, fakeResolver{})
	err := m.ApplyMergeTransition("ws1", "bogus", "")
	if err == nil {
		t.Fatalf("expected error for unknown merge phase, got nil")
	}
	if !strings.Contains(err.Error(), "unknown merge phase") {
		t.Fatalf("error = %v, want it to mention unknown merge phase", err)
	}
}

// TestPersistenceAcrossReopen: resolved state survives closing and
// reopening the SSM on the same DB path, and the reopen does not re-log the
// restored state as a fresh transition.
func TestPersistenceAcrossReopen(t *testing.T) {
	// Arrange: idle session with a live task, on a stable path.
	path := filepath.Join(t.TempDir(), "state.db")
	res := fakeResolver{"s1": "ws1"}
	m1, err := Open(Options{DBPath: path, Resolver: res})
	if err != nil {
		t.Fatalf("Open 1: %v", err)
	}
	if err := m1.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("session start: %v", err)
	}
	if err := m1.Apply(evTaskStarted("s1", 2, "a1")); err != nil {
		t.Fatalf("task start: %v", err)
	}
	before := mustCurrent(t, m1, "ws1")
	if before.State != frontendv1.RenderState_RENDER_STATE_IDLE_ASYNC {
		t.Fatalf("pre-reopen state = %s, want IDLE_ASYNC", renderName(before.State))
	}
	if err := m1.Close(); err != nil {
		t.Fatalf("Close 1: %v", err)
	}

	// Act: reopen on the same path.
	cl := &capLog{}
	m2, err := Open(Options{DBPath: path, Logf: cl.logf, Resolver: res})
	if err != nil {
		t.Fatalf("Open 2: %v", err)
	}
	t.Cleanup(func() { m2.Close() })

	// Assert: state (including live_task_count) survived.
	after := mustCurrent(t, m2, "ws1")
	if after.State != frontendv1.RenderState_RENDER_STATE_IDLE_ASYNC {
		t.Fatalf("post-reopen state = %s, want IDLE_ASYNC", renderName(after.State))
	}
	if after.LiveTaskCount != 1 {
		t.Fatalf("post-reopen live_task_count = %d, want 1", after.LiveTaskCount)
	}
	// The reopen restores silently — no transition line for the restore.
	if cl.contains("transition ws=ws1") {
		t.Fatalf("reopen logged a transition; want a silent restore. lines: %v", cl.lines)
	}
	if !cl.contains("restored 1 workspace") {
		t.Fatalf("expected a restore-count log line, got: %v", cl.lines)
	}

	// And a subsequent real change on m2 still resolves correctly.
	if err := m2.Apply(evTaskEnded("s1", 3, "a1", corev1.TerminalStatus_TERMINAL_STATUS_DONE)); err != nil {
		t.Fatalf("task end: %v", err)
	}
	if got := mustCurrent(t, m2, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_IDLE {
		t.Fatalf("state after task end = %s, want IDLE", renderName(got))
	}
}

// TestSubscribePushesTransitions: a subscriber receives a WorkspaceState on
// each state change and stops after unsubscribe.
func TestSubscribePushesTransitions(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	ch, cancel := m.Subscribe()

	// Act 1: a transition.
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("Apply: %v", err)
	}
	// Assert 1: the push arrived with the resolved state.
	select {
	case msg := <-ch:
		if msg.State != frontendv1.RenderState_RENDER_STATE_THINKING {
			t.Fatalf("pushed state = %s, want THINKING", renderName(msg.State))
		}
		if msg.Workspace != "ws1" {
			t.Fatalf("pushed workspace = %q, want ws1", msg.Workspace)
		}
	default:
		t.Fatalf("expected a push, channel empty")
	}

	// Act 2: unsubscribe, then cause another transition.
	cancel()
	if err := m.Apply(evTurnEnded("s1", 2, false)); err != nil {
		t.Fatalf("Apply: %v", err)
	}
	// Assert 2: the channel is closed (drained), no further live values.
	if _, ok := <-ch; ok {
		t.Fatalf("expected closed channel after cancel")
	}
}

// TestApplyNoTransitionNoPush: an event that does not change the resolved
// state produces no transition log and no push.
func TestApplyNoTransitionNoPush(t *testing.T) {
	// Arrange: an already-thinking session with a subscriber.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("Apply turn: %v", err)
	}
	ch, _ := m.Subscribe()
	// Act: a task starts (count changes) but the winning state stays thinking.
	if err := m.Apply(evTaskStarted("s1", 2, "a1")); err != nil {
		t.Fatalf("Apply task: %v", err)
	}
	// Assert: no thinking→thinking transition line, and no push.
	if cl.count("transition ws=ws1") != 1 { // only the original turn-start
		t.Fatalf("transition count = %d, want 1 (no self-transition)", cl.count("transition ws=ws1"))
	}
	select {
	case msg := <-ch:
		t.Fatalf("unexpected push for a non-transition: %s", renderName(msg.State))
	default:
	}
}

// TestApplyUnboundSessionErrors: an event whose session has no workspace
// binding errors loudly rather than dropping the event.
func TestApplyUnboundSessionErrors(t *testing.T) {
	m, _, _ := openTest(t, fakeResolver{})
	err := m.Apply(evTurnStarted("orphan", 1))
	if err == nil {
		t.Fatalf("expected error for unbound session, got nil")
	}
	if !strings.Contains(err.Error(), "no workspace bound") {
		t.Fatalf("error = %v, want unbound-session message", err)
	}
}

// TestApplyNilAndEmptyEventErrors: malformed inputs error loudly.
func TestApplyNilAndEmptyEventErrors(t *testing.T) {
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(nil); err == nil {
		t.Fatalf("expected error for nil event")
	}
	if err := m.Apply(&corev1.Event{Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{}}}); err == nil {
		t.Fatalf("expected error for event with empty session_id")
	}
}

// TestApplyIgnoresEphemeral: an ephemeral payload is ignored (logged, no
// state change).
func TestApplyIgnoresEphemeral(t *testing.T) {
	// Arrange.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	// Act: a content delta (ephemeral) with a bound session.
	ev := &corev1.Event{SessionId: "s1", Seq: 1, Payload: &corev1.Event_ContentDelta{ContentDelta: &corev1.ContentDelta{}}}
	if err := m.Apply(ev); err != nil {
		t.Fatalf("Apply: %v", err)
	}
	// Assert: no state resolved, and the ignore was logged.
	if _, found, _ := m.Current("ws1"); found {
		t.Fatalf("ephemeral event produced a resolved state; want none")
	}
	if !cl.contains("ignoring non-lifecycle event kind=content_delta") {
		t.Fatalf("expected an ignore log line, got: %v", cl.lines)
	}
}

// TestSnapshotAllWorkspaces returns each workspace with a render state, in
// stable order.
func TestSnapshotAllWorkspaces(t *testing.T) {
	// Arrange: two bound sessions in two workspaces.
	m, _, _ := openTest(t, fakeResolver{"s1": "wsA", "s2": "wsB"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("Apply s1: %v", err)
	}
	if err := m.Apply(evSessionStarted("s2", 1)); err != nil {
		t.Fatalf("Apply s2: %v", err)
	}
	// Act.
	snap, err := m.Snapshot()
	if err != nil {
		t.Fatalf("Snapshot: %v", err)
	}
	// Assert.
	if len(snap) != 2 {
		t.Fatalf("snapshot len = %d, want 2", len(snap))
	}
	if snap[0].Workspace != "wsA" || snap[0].State != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("snap[0] = %q/%s, want wsA/THINKING", snap[0].Workspace, renderName(snap[0].State))
	}
	if snap[1].Workspace != "wsB" || snap[1].State != frontendv1.RenderState_RENDER_STATE_IDLE {
		t.Fatalf("snap[1] = %q/%s, want wsB/IDLE", snap[1].Workspace, renderName(snap[1].State))
	}
}

// TestTransitionLogFormat: the transition line carries old→new, cause kind
// and cause seq per the §12 contract.
func TestTransitionLogFormat(t *testing.T) {
	// Arrange.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	// Act.
	if err := m.Apply(evTurnStarted("s1", 42)); err != nil {
		t.Fatalf("Apply: %v", err)
	}
	// Assert.
	want := "ws=ws1 ∅→RENDER_STATE_THINKING cause_kind=turn_started cause_seq=42"
	if !cl.contains(want) {
		t.Fatalf("missing transition line %q; got: %v", want, cl.lines)
	}
}
