package ssm

import (
	"database/sql"
	"fmt"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	_ "modernc.org/sqlite"
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
	return &corev1.Event{SessionId: sid, Seq: seq, Plane: corev1.Plane_PLANE_STREAM, Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{}}}
}
func evTurnEnded(sid string, seq uint64, isErr bool) *corev1.Event {
	return &corev1.Event{SessionId: sid, Seq: seq, Plane: corev1.Plane_PLANE_STREAM, Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{IsError: isErr}}}
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

// openTest opens a Manager on a temp DB with a capturing logger, and WIRES
// every workspace the resolver names.
//
// The wiring is arrangement, not subject. It is sessioncontroller's fact, produced off
// the bring-up gate's own verdict, and until it stands the connection-truth law
// holds every workspace on the axis's closed half no matter what the agent
// reports — so a test
// about the session-status lifecycle, the context cuts, or the backfill cannot be asked at all
// of an unwired workspace. Tests that are ABOUT the axis call ApplyWired
// themselves, and openUnwiredTest exists for the ones that must start from
// nothing.
func openTest(t *testing.T, resolver Resolver) (*Manager, *capLog, string) {
	t.Helper()
	m, cl, path := openUnwiredTest(t, resolver)
	wireAll(t, m, resolver)
	return m, cl, path
}

// wireAll puts every workspace RESOLVER names on the legacy connectivity projection. See openTest
// for why this is arrangement rather than subject.
func wireAll(t *testing.T, m *Manager, resolver Resolver) {
	t.Helper()
	fake, ok := resolver.(fakeResolver)
	if !ok {
		return
	}
	for _, ws := range fake {
		if err := m.ApplyWired(ws, WiringWired, "test arrangement"); err != nil {
			t.Fatalf("ApplyWired(%s): %v", ws, err)
		}
	}
}

// openUnwiredTest opens a Manager with NO workspace wired — the honest state of
// a daemon that has just come up.
func openUnwiredTest(t *testing.T, resolver Resolver) (*Manager, *capLog, string) {
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

// TestApplyLifecycleTransitions drives the full session-status lifecycle lifecycle through
// Apply, one transition per case.
func TestApplyLifecycleTransitions(t *testing.T) {
	tests := []struct {
		name string
		ev   *corev1.Event
		want frontendv1.RenderState
	}{
		{"session started -> ready", evSessionStarted("s1", 1), frontendv1.RenderState_RENDER_STATE_READY},
		{"turn started -> thinking", evTurnStarted("s1", 1), frontendv1.RenderState_RENDER_STATE_THINKING},
		{"clean turn end -> done", evTurnEnded("s1", 1, false), frontendv1.RenderState_RENDER_STATE_DONE},
		{"errored turn end -> vendor_blocked", evTurnEnded("s1", 1, true), frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED},
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
	// Arrange: an idle session — yellow is a promotion of a green.
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
		{evTaskEnded("s1", 5, "a2", corev1.TerminalStatus_TERMINAL_STATUS_LOST), 0, frontendv1.RenderState_RENDER_STATE_READY},
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
	m1, err := Open(Options{DBPath: path, Logf: func(string, ...any) {}, Resolver: res})
	if err != nil {
		t.Fatalf("Open 1: %v", err)
	}
	wireAll(t, m1, res)
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

	// Assert, FIRST HALF: the WIRING did not survive. Nothing is connected to a
	// daemon that has just started, and the connection-truth law makes that the
	// only honest thing a restored tab can claim — so the restored workspace is
	// asleep however green its log reads. Phase B's reattach sweep re-wires the
	// working set; until then this is correct rather than merely tolerated.
	if got := mustCurrent(t, m2, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_HIBERNATED {
		t.Fatalf("post-reopen state = %s, want HIBERNATED (nothing is wired to a fresh daemon, and a fresh daemon is not a broken one)", renderName(got))
	}
	// The reopen restores silently — no transition line for the restore. Checked
	// BEFORE the re-wire below, which is a real transition rather than a restore.
	if cl.contains("transition ws=ws1") {
		t.Fatalf("reopen logged a transition; want a silent restore. lines: %v", cl.lines)
	}
	if !cl.contains("restored 1 workspace") {
		t.Fatalf("expected a restore-count log line, got: %v", cl.lines)
	}
	connectOperational(t, m2, "ws1", "s1", "generation-reopen")

	// Assert, SECOND HALF: the LOG survived. Re-wire, and the state (including
	// live_task_count) is exactly what it was.
	after := mustCurrent(t, m2, "ws1")
	if after.State != frontendv1.RenderState_RENDER_STATE_IDLE_ASYNC {
		t.Fatalf("post-reopen state = %s, want IDLE_ASYNC", renderName(after.State))
	}
	if after.LiveTaskCount != 1 {
		t.Fatalf("post-reopen live_task_count = %d, want 1", after.LiveTaskCount)
	}
	// And a subsequent real change on m2 still resolves correctly.
	if err := m2.Apply(evTaskEnded("s1", 3, "a1", corev1.TerminalStatus_TERMINAL_STATUS_DONE)); err != nil {
		t.Fatalf("task end: %v", err)
	}
	if got := mustCurrent(t, m2, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_READY {
		t.Fatalf("state after task end = %s, want READY", renderName(got))
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

// TestApplyLogsNoSelfTransition: an event that leaves the resolved state where
// it was writes no transition line (§12: log deltas only).
func TestApplyLogsNoSelfTransition(t *testing.T) {
	// Arrange: an already-thinking session.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("Apply turn: %v", err)
	}
	// Act: a task starts; the winning state stays thinking.
	if err := m.Apply(evTaskStarted("s1", 2, "a1")); err != nil {
		t.Fatalf("Apply task: %v", err)
	}
	// Assert: only the original turn-start transition line.
	if cl.count("transition ws=ws1") != 1 {
		t.Fatalf("transition count = %d, want 1 (no self-transition)", cl.count("transition ws=ws1"))
	}
}

// TestApplyPushesWhenOnlyTheLiveTaskCountMoves: live_task_count is RENDERED
// (the footer's live-task figure, via progress.ApplyWorkspaceState), so a
// count-only change must reach subscribers. Keying the push on the render state
// alone left a stale number on screen with nothing that could correct it.
func TestApplyPushesWhenOnlyTheLiveTaskCountMoves(t *testing.T) {
	// Arrange: an already-thinking session with a subscriber.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("Apply turn: %v", err)
	}
	ch, _ := m.Subscribe()
	// Act: a task starts; the state stays thinking but the count goes 0→1.
	if err := m.Apply(evTaskStarted("s1", 2, "a1")); err != nil {
		t.Fatalf("Apply task: %v", err)
	}
	// Assert.
	select {
	case msg := <-ch:
		if msg.GetLiveTaskCount() != 1 {
			t.Fatalf("pushed live_task_count = %d, want 1", msg.GetLiveTaskCount())
		}
	default:
		t.Fatal("a live-task-count change must be pushed")
	}
}

// TestApplyNoVisibleChangeNoPush: an event that moves neither the state nor the
// live-task count stays quiet.
func TestApplyNoVisibleChangeNoPush(t *testing.T) {
	// Arrange: an already-thinking session with a subscriber.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("Apply turn: %v", err)
	}
	ch, _ := m.Subscribe()
	// Act: another turn start — same state, same (zero) task count.
	if err := m.Apply(evTurnStarted("s1", 2)); err != nil {
		t.Fatalf("Apply turn: %v", err)
	}
	// Assert.
	select {
	case msg := <-ch:
		t.Fatalf("unexpected push for a non-change: %s", renderName(msg.State))
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

func TestApplyRejectsFilePlaneTurnLifecycle(t *testing.T) {
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	ev := &corev1.Event{
		SessionId: "s1",
		Seq:       7,
		Plane:     corev1.Plane_PLANE_FILE,
		DedupKey:  "turn:s1:old-stop-hook",
		Payload: &corev1.Event_TurnEnded{TurnEnded: &corev1.TurnEnded{
			StopReason: "end_turn",
		}},
	}
	err := m.Apply(ev)
	if err == nil {
		t.Fatal("file-plane TurnEnded was accepted")
	}
	if !strings.Contains(err.Error(), "non-authoritative turn lifecycle") ||
		!strings.Contains(err.Error(), "plane=PLANE_FILE") ||
		!strings.Contains(err.Error(), "seq=7") {
		t.Fatalf("error = %q", err)
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
	if snap[1].Workspace != "wsB" || snap[1].State != frontendv1.RenderState_RENDER_STATE_READY {
		t.Fatalf("snap[1] = %q/%s, want wsB/READY", snap[1].Workspace, renderName(snap[1].State))
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

// --- per-task idempotency of the live-task counter ---------------------------

func TestLiveTaskCountIgnoresADuplicateTaskEnded(t *testing.T) {
	// Arrange — one task, ended TWICE at different seqs. This is the real
	// shape now that a shell spool's EXIT= marker can report a completion the
	// stream plane or a TaskStop tool result already reported.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	mustApply(t, m, evSessionStarted("s1", 1))
	mustApply(t, m, evTaskStarted("s1", 2, "a1"))

	// Act.
	mustApply(t, m, evTaskEnded("s1", 3, "a1", corev1.TerminalStatus_TERMINAL_STATUS_DONE))
	mustApply(t, m, evTaskEnded("s1", 4, "a1", corev1.TerminalStatus_TERMINAL_STATUS_LOST))

	// Assert — the second end is a no-op, not a second decrement.
	if got := mustCurrent(t, m, "ws1").LiveTaskCount; got != 0 {
		t.Fatalf("live task count = %d after a duplicate end, want 0", got)
	}
}

func TestLiveTaskCountNeverGoesNegativeOnDuplicateEnds(t *testing.T) {
	// Arrange — the failure the old SUM produced: repeated ends drove the
	// counter below zero, which mis-derives idle vs idle_async.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	mustApply(t, m, evSessionStarted("s1", 1))
	mustApply(t, m, evTaskStarted("s1", 2, "a1"))

	// Act.
	for seq := uint64(3); seq <= 6; seq++ {
		mustApply(t, m, evTaskEnded("s1", seq, "a1", corev1.TerminalStatus_TERMINAL_STATUS_DONE))
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").LiveTaskCount; got != 0 {
		t.Fatalf("live task count = %d, want 0 (never negative)", got)
	}
}

func TestLiveTaskCountKeepsADuplicateEndFromResurrectingIdleAsync(t *testing.T) {
	// Arrange — two live tasks on a live session; one of them ends twice.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	mustApply(t, m, evSessionStarted("s1", 1))
	mustApply(t, m, evTaskStarted("s1", 2, "a1"))
	mustApply(t, m, evTaskStarted("s1", 3, "a2"))

	// Act — a1 ends twice; a2 is still running.
	mustApply(t, m, evTaskEnded("s1", 4, "a1", corev1.TerminalStatus_TERMINAL_STATUS_DONE))
	mustApply(t, m, evTaskEnded("s1", 5, "a1", corev1.TerminalStatus_TERMINAL_STATUS_DONE))

	// Assert — a2 is still live, so the workspace stays idle_async.
	cur := mustCurrent(t, m, "ws1")
	if cur.LiveTaskCount != 1 {
		t.Fatalf("live task count = %d, want 1 (a2 is still running)", cur.LiveTaskCount)
	}
	if cur.State != frontendv1.RenderState_RENDER_STATE_IDLE_ASYNC {
		t.Fatalf("state = %s, want idle_async", renderName(cur.State))
	}
}

func TestLiveTaskCountIgnoresADuplicateTaskStarted(t *testing.T) {
	// Arrange — the same dedup must hold on the opening side, or a task
	// reported started twice would need two ends to clear.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	mustApply(t, m, evSessionStarted("s1", 1))

	// Act.
	mustApply(t, m, evTaskStarted("s1", 2, "a1"))
	mustApply(t, m, evTaskStarted("s1", 3, "a1"))

	// Assert.
	if got := mustCurrent(t, m, "ws1").LiveTaskCount; got != 1 {
		t.Fatalf("live task count = %d after a duplicate start, want 1", got)
	}
}

func TestLiveTaskCountStillCountsDistinctTasks(t *testing.T) {
	// Arrange — the dedup must not collapse genuinely different tasks.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	mustApply(t, m, evSessionStarted("s1", 1))

	// Act.
	mustApply(t, m, evTaskStarted("s1", 2, "a1"))
	mustApply(t, m, evTaskStarted("s1", 3, "a2"))
	mustApply(t, m, evTaskStarted("s1", 4, "a3"))

	// Assert.
	if got := mustCurrent(t, m, "ws1").LiveTaskCount; got != 3 {
		t.Fatalf("live task count = %d, want 3", got)
	}
}

// mustApply applies ev, failing the test on error.
func mustApply(t *testing.T, m *Manager, ev *corev1.Event) {
	t.Helper()
	if err := m.Apply(ev); err != nil {
		t.Fatalf("Apply(seq %d): %v", ev.GetSeq(), err)
	}
}

// --- v1 -> v2 migration ------------------------------------------------------

// writeV1DB creates a DB with the pre-task_id v1 schema and one row, as an
// already-deployed daemon would have left it.
func writeV1DB(t *testing.T, path string) {
	t.Helper()
	db, err := sql.Open("sqlite", path)
	if err != nil {
		t.Fatalf("open v1 db: %v", err)
	}
	defer db.Close()
	if _, err := db.Exec(`
		CREATE TABLE schema_meta (version INTEGER NOT NULL);
		INSERT INTO schema_meta(version) VALUES (1);
		CREATE TABLE workspace_state (
			workspace   TEXT    NOT NULL,
			session_id  TEXT,
			state       TEXT    NOT NULL,
			cause_kind  TEXT    NOT NULL,
			cause_seq   INTEGER,
			at          INTEGER NOT NULL,
			PRIMARY KEY (workspace, at)
		);
		INSERT INTO workspace_state(workspace, session_id, state, cause_kind, cause_seq, at)
		VALUES ('ws1','s1','idle','session_started',1,1);
	`); err != nil {
		t.Fatalf("seed v1 db: %v", err)
	}
}

func TestMigrateAddsTaskIDToAV1Database(t *testing.T) {
	// Arrange — a DB left by a daemon that predates the column.
	path := filepath.Join(t.TempDir(), "state.db")
	writeV1DB(t, path)

	// Act.
	m, err := Open(Options{DBPath: path, Logf: func(string, ...any) {}, Resolver: fakeResolver{"s1": "ws1"}})
	if err != nil {
		t.Fatalf("Open a v1 db: %v", err)
	}
	t.Cleanup(func() { m.Close() })
	// The migration is observed after a new controller generation establishes.
	connectOperational(t, m, "ws1", "s1", "generation-reopen")

	// Assert — the workspace still resolves, which it cannot without the column.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_READY {
		t.Fatalf("state after migration = %s, want ready", renderName(got))
	}
}

func TestMigrateRecordsTheNewSchemaVersion(t *testing.T) {
	// Arrange — leaving the stamp at 1 would make the version guard meaningless.
	path := filepath.Join(t.TempDir(), "state.db")
	writeV1DB(t, path)
	m, err := Open(Options{DBPath: path, Logf: func(string, ...any) {}, Resolver: fakeResolver{}})
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	m.Close()

	// Act.
	db, err := sql.Open("sqlite", path)
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	defer db.Close()
	var version int64
	if err := db.QueryRow(`SELECT version FROM schema_meta LIMIT 1`).Scan(&version); err != nil {
		t.Fatalf("read version: %v", err)
	}

	// Assert.
	if version != schemaVersion {
		t.Fatalf("schema version = %d, want %d", version, schemaVersion)
	}
}

func TestMigrateIsIdempotentAcrossReopens(t *testing.T) {
	// Arrange — ALTER TABLE ADD COLUMN is not idempotent in SQLite, so a
	// second open must not try to re-add the column.
	path := filepath.Join(t.TempDir(), "state.db")
	writeV1DB(t, path)
	first, err := Open(Options{DBPath: path, Logf: func(string, ...any) {}, Resolver: fakeResolver{}})
	if err != nil {
		t.Fatalf("first Open: %v", err)
	}
	first.Close()

	// Act.
	second, err := Open(Options{DBPath: path, Logf: func(string, ...any) {}, Resolver: fakeResolver{}})

	// Assert.
	if err != nil {
		t.Fatalf("second Open: %v", err)
	}
	second.Close()
}

func TestMigratedV1RowsStillCountIndividually(t *testing.T) {
	// Arrange — pre-migration task rows carry a NULL task_id. They must keep
	// counting one-per-row rather than collapsing into a single NULL group.
	path := filepath.Join(t.TempDir(), "state.db")
	writeV1DB(t, path)
	db, err := sql.Open("sqlite", path)
	if err != nil {
		t.Fatalf("open: %v", err)
	}
	if _, err := db.Exec(`
		INSERT INTO workspace_state(workspace, session_id, state, cause_kind, cause_seq, at)
		VALUES ('ws1','s1','task_started','task_started',2,2),
		       ('ws1','s1','task_started','task_started',3,3);
	`); err != nil {
		t.Fatalf("seed legacy task rows: %v", err)
	}
	db.Close()

	// Act.
	m, err := Open(Options{DBPath: path, Logf: func(string, ...any) {}, Resolver: fakeResolver{"s1": "ws1"}})
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { m.Close() })

	// Assert — two distinct legacy starts, not one collapsed NULL.
	if got := mustCurrent(t, m, "ws1").LiveTaskCount; got != 2 {
		t.Fatalf("live task count over legacy NULL rows = %d, want 2", got)
	}
}

// --- ApplyConnectionDegraded (F4) -------------------------------------------
//
// The transport-level heartbeat miss called the Degraded sink and appended NO
// state row, so it produced a banner and no workspace color at all. Retiring
// the banner without this would have lost the ambience entirely.

func TestConnectionDegradedResolvesTheWorkspaceDegraded(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})

	// Act.
	if err := m.ApplyConnectionDegraded("ws1", true, "no shim traffic for 30s"); err != nil {
		t.Fatalf("ApplyConnectionDegraded: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_DEGRADED {
		t.Fatalf("state = %v, want RENDER_STATE_DEGRADED", got)
	}
}

func TestConnectionRecoveredRevealsTheStateUnderneath(t *testing.T) {
	// Arrange: a live workspace that then went quiet. The legacy impairment projection sits
	// ON TOP of the session-status lifecycle rather than replacing it, so clearing it must
	// reveal the session underneath rather than leave the workspace stateless.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	if err := m.ApplyConnectionDegraded("ws1", true, "no traffic"); err != nil {
		t.Fatalf("degrade: %v", err)
	}
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_DEGRADED {
		t.Fatalf("state before recovery = %v, want DEGRADED", got)
	}

	// Act.
	if err := m.ApplyConnectionDegraded("ws1", false, ""); err != nil {
		t.Fatalf("recover: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").GetState(); got == frontendv1.RenderState_RENDER_STATE_DEGRADED {
		t.Fatal("state stayed DEGRADED after recovery; the axis did not clear")
	}
}

func TestConnectionDegradedRejectsAnEmptyWorkspace(t *testing.T) {
	// Arrange: a degraded transition with nothing to key by is a caller bug,
	// and writing it under "" would degrade a workspace nobody can see.
	m, _, _ := openTest(t, fakeResolver{})

	// Act.
	err := m.ApplyConnectionDegraded("", true, "no traffic")

	// Assert.
	if err == nil {
		t.Fatal("ApplyConnectionDegraded with no workspace must error")
	}
}

// AtMs is the frontend's freshness watermark: the connect snapshot is refused
// until it is at least as new as the newest delivered frame, so a stamp that
// moves backwards wedges the connect loop into an unbounded retry storm (and
// starves that connection's command reads). The stamp is therefore monotonic
// per workspace, whatever the axis branches resolve.
func TestStampFreshnessNeverLetsAtMsRegress(t *testing.T) {
	// Arrange — a manager that has already stamped a newer frame.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	newer := &frontendv1.WorkspaceState{Workspace: "ws1", AtMs: 2000}
	m.mu.Lock()
	m.stampFreshnessLocked("ws1", newer)
	m.mu.Unlock()

	// Act — a later build resolves an OLDER winning row.
	older := &frontendv1.WorkspaceState{Workspace: "ws1", AtMs: 1000}
	m.mu.Lock()
	m.stampFreshnessLocked("ws1", older)
	m.mu.Unlock()

	// Assert — the stamp is raised to the watermark, never regressed.
	if got := older.GetAtMs(); got != 2000 {
		t.Fatalf("AtMs = %d after a regressing resolution, want the 2000 watermark", got)
	}
}

// A newer resolution must advance the watermark rather than being clamped.
func TestStampFreshnessAdvancesWithNewerFrames(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	m.mu.Lock()
	m.stampFreshnessLocked("ws1", &frontendv1.WorkspaceState{Workspace: "ws1", AtMs: 1000})
	m.mu.Unlock()

	// Act.
	newer := &frontendv1.WorkspaceState{Workspace: "ws1", AtMs: 3000}
	m.mu.Lock()
	m.stampFreshnessLocked("ws1", newer)
	m.mu.Unlock()

	// Assert — untouched, and the watermark moved with it.
	if got := newer.GetAtMs(); got != 3000 {
		t.Fatalf("AtMs = %d, want 3000", got)
	}
	m.mu.Lock()
	watermark := m.pushedAtMs["ws1"]
	m.mu.Unlock()
	if watermark != 3000 {
		t.Fatalf("watermark = %d, want 3000", watermark)
	}
}

// THE GUARANTEE: two DIFFERENT composites never carry the same AtMs. The
// webapp reads AtMs as the frame's revision and holds "one revision names one
// state"; the old clamp lifted a regressing stamp to the watermark and minted a
// second composite with the first one's revision, which the webapp reported as
// `revision conflicted`.
func TestStampFreshnessGivesADifferentCompositeItsOwnRevision(t *testing.T) {
	// Arrange — a delivered frame at the watermark.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	delivered := &frontendv1.WorkspaceState{
		Workspace: "ws1", AtMs: 2000, State: frontendv1.RenderState_RENDER_STATE_THINKING,
	}
	m.mu.Lock()
	m.stampFreshnessLocked("ws1", delivered)
	m.mu.Unlock()

	// Act — a DIFFERENT composite resolves off an older row.
	regressed := &frontendv1.WorkspaceState{
		Workspace: "ws1", AtMs: 1000, State: frontendv1.RenderState_RENDER_STATE_DONE,
	}
	m.mu.Lock()
	m.stampFreshnessLocked("ws1", regressed)
	m.mu.Unlock()

	// Assert.
	if got := regressed.GetAtMs(); got == delivered.GetAtMs() {
		t.Fatalf("a different composite got the delivered frame's revision %d; distinct composites must never share one", got)
	}
	if got := regressed.GetAtMs(); got <= delivered.GetAtMs() {
		t.Fatalf("AtMs = %d, want strictly newer than the %d watermark", got, delivered.GetAtMs())
	}
}

// THE VIOLATION EDGE: an IDENTICAL composite must NOT earn a new revision. A
// resync's Snapshot rebuilds the frame the last push carried, and bumping there
// would make the state appear to change on every reconnect.
func TestStampFreshnessKeepsTheRevisionOfAnIdenticalComposite(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	delivered := &frontendv1.WorkspaceState{
		Workspace: "ws1", AtMs: 2000, State: frontendv1.RenderState_RENDER_STATE_THINKING,
	}
	m.mu.Lock()
	m.stampFreshnessLocked("ws1", delivered)
	m.mu.Unlock()

	// Act — the same state, resolved again at the same instant.
	resync := &frontendv1.WorkspaceState{
		Workspace: "ws1", AtMs: 2000, State: frontendv1.RenderState_RENDER_STATE_THINKING,
	}
	m.mu.Lock()
	m.stampFreshnessLocked("ws1", resync)
	m.mu.Unlock()

	// Assert.
	if got := resync.GetAtMs(); got != 2000 {
		t.Fatalf("AtMs = %d for an identical composite, want the delivered 2000", got)
	}
}

// Workspaces must not share a watermark: one workspace's newer frame cannot
// inflate another's stamp.
func TestStampFreshnessIsPerWorkspace(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	m.mu.Lock()
	m.stampFreshnessLocked("ws-a", &frontendv1.WorkspaceState{Workspace: "ws-a", AtMs: 9000})
	m.mu.Unlock()

	// Act.
	other := &frontendv1.WorkspaceState{Workspace: "ws-b", AtMs: 100}
	m.mu.Lock()
	m.stampFreshnessLocked("ws-b", other)
	m.mu.Unlock()

	// Assert.
	if got := other.GetAtMs(); got != 100 {
		t.Fatalf("AtMs = %d, want ws-b's own 100, not ws-a's watermark", got)
	}
}
